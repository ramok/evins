-module(fsm_test_bidir).
-behaviour(fsm).

-include("fsm.hrl").

-export([start_link/1, trans/0, final/0, init_event/0]).
-export([init/1,handle_event/3,stop/1]).

-export([handle_idle/3, handle_alarm/3, handle_write/3, handle_final/3]).

-define(TRANS, [
    {idle, [
        {internal,          idle},
        {answer_timeout,    idle},
        {send_chunk,        write},
        {alarm,             final}
        ]},

    {write, [
        {error,             idle},
        {alarm,             final},
        {set_event,         idle}
        ]},

    {alarm,
         [{final,           alarm}
         ]},

    {final, []}
    ]).

start_link(SM) -> fsm:start_link(SM).
init(SM)       -> SM.
trans()        -> ?TRANS.
final()        -> [alarm].
init_event()   -> internal.
stop(_SM)      -> ok.

%%--------------------------------Handler functions-------------------------------
handle_event(MM, SM, Term) ->
    ?INFO(?ID, "HANDLE EVENT~n", []),
    ?TRACE(?ID, "~p~n", [Term]),
    case Term of
        {timeout, Event} ->
            ?INFO(?ID, "!! timeout ~140p~n", [Event]),
            fsm:run_event(MM, SM#sm{event=Event}, {});

        {connected} ->
            ?INFO(?ID, "connected ~n", []),
            SM;

        {async, Tuple} ->
            ?INFO(?ID, "async ~p ~n", [Tuple]),
            SM;

        {async, {pid,_NPID}, Tuple} ->
            ?INFO(?ID, "async ~p ~n", [Tuple]),
            SM;

        {sync, Req, Answer} ->
            ?INFO(?ID, "sync ~p ~p ~n", [Req, Answer]),
            SM;

        {raw, Data} ->
            RES = check_file_data(SM, Data),
            if RES -> SM;
            true -> ?ERROR(?ID, "corrupted received data. ~n", []),
                    fsm:run_event(MM, SM#sm{event=alarm}, {})
            end;

        UUg ->
            ?ERROR(?ID, "~s: unhandled event:~p~n", [?MODULE, UUg]),
            SM
    end.

handle_idle(_MM, SM, Term) ->
    SM1 =
    if SM#sm.event =:= internal
            -> init_sm(SM);
    true    -> SM
    end,
    ?TRACE(?ID, "!!!! handle_idle ~120p~n", [Term]),
    SM1#sm{event = eps}.

handle_write(_MM, SM, Term) ->
    ?TRACE(?ID, "!!!! handle_write ~120p~n", [Term]),
    Result = send_file_data(SM),
    if Result =/= error ->
           fsm:set_timeout(SM#sm{event = set_event}, {ms, random:uniform(30000) + 500}, send_chunk);
    true ->SM#sm{event = error}
    end.

handle_final(_MM, SM, Term) ->
    ?TRACE(?ID, "=========================== Final ~120p~n", [Term]),
    fsm:clear_timeouts(SM#sm{event = eps}).

-spec handle_alarm(any(), any(), any()) -> no_return().
handle_alarm(_MM, SM, _Term) ->
    exit({alarm, SM#sm.module}).

% ------- Helper functions --------
init_sm(SM) ->
    ?TRACE(?ID, "init_sm~n",[]),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    open_file(SM, fd_send),
    open_file(SM, fd_read),
    send_file_data(SM),
    fsm:set_timeout(SM#sm{event = eps}, {ms, random:uniform(5000)}, send_chunk).

open_file(SM, ETSFD) ->
    Fname = readETS(SM, filename),
    case file:open(Fname, [read, raw, binary]) of
        {ok, Fd}        -> insertETS(SM, ETSFD, Fd),
                           SM;
        {error, Reason} -> ?ERROR(?ID, "open file error, reason : ~p~n", [Reason]),
                           SM
    end.

send_file_data(SM) ->
    case read_file(SM, fd_send, random:uniform(3000) + 1000) of
        error    -> error;
        Data     -> fsm:cast(SM, at, {send, {raw, Data}})
    end.

check_file_data(SM, DataToCheck) ->
    case read_file(SM, fd_read, byte_size(DataToCheck)) of
        error       -> error;
        DataToCheck -> true;
        _           -> false
    end.

read_file(SM, ETSFD, Count) ->
    case readETS(SM, ETSFD) of
        empty   -> error;
        Fd      -> case file:read(Fd, Count) of
                       {ok, Data}       -> ?TRACE(?ID, "file read data ~p fd: ~p ~n", [Data, ETSFD]),
                                           Data;
                       eof              -> file:close(Fd),
                                           error;
                       {error, Reason}  -> ?ERROR(?ID, "read file error, reason : ~p~n", [Reason]),
                                           file:close(Fd),
                                           error
                   end
    end.

cleanETS(SM, Term) -> ets:match_delete(SM#sm.share, {Term, '_'}).

insertETS(SM, Term, Value) ->
    cleanETS(SM, Term),
    case ets:insert(SM#sm.share, [{Term, Value}]) of
        true  -> SM;
        false -> insertETS(SM, Term, Value)
    end.

readETS(SM, Term) ->
    case ets:lookup(SM#sm.share, Term) of
        [{Term, Value}] -> Value;
        _               -> empty
    end.



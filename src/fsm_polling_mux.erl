%% Copyright (c) 2015, Veronika Kebkal <veronika.kebkal@evologics.de>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%% 3. The name of the author may not be used to endorse or promote products
%%    derived from this software without specific prior written permission.
%%
%% Alternatively, this software may be distributed under the terms of the
%% GNU General Public License ("GPL") version 2 as published by the Free
%% Software Foundation.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(fsm_polling_mux).
-behaviour(fsm).

-include("fsm.hrl").

-export([start_link/1, trans/0, final/0, init_event/0]).
-export([init/1, handle_event/3, stop/1]).

-export([handle_polling/3, handle_wait_poll_pbm/3, handle_wait_poll_data/3]).
-export([handle_poll_response_pbm/3, handle_poll_response_data/3]).
-export([handle_idle/3, handle_alarm/3, handle_final/3]).

-define(TRANS, [
                {idle,
                 [{internal, idle},
                 {init_counter, idle},
                 {poll_start, polling},
                 {poll_stop, idle},
                 {recv_poll_seq, poll_response_pbm},

                 {recv_poll_data, idle},
                 {recv_poll_pbm, idle}
                 ]},

                {polling,
                 [{poll_next_addr, polling},
                 {send_poll_data, wait_poll_pbm},
                 {poll_start, polling},
                 {poll_stop, idle},
                 {retransmit_im, polling},

                 {recv_poll_data, polling},
                 {recv_poll_pbm, polling},

                 {error, idle}
                 ]},

                {wait_poll_pbm,
                 [{poll_start, polling},
                 {poll_stop, idle},

                 {process_poll_pbm, wait_poll_pbm},

                 {recv_poll_pbm, wait_poll_data},
                 {recv_poll_data, wait_poll_pbm},

                 {poll_next_addr, polling}
                 ]},

                {wait_poll_data,
                 [
                 {poll_start, polling},
                 {poll_stop, idle},

                 {recv_poll_pbm, wait_poll_data},
                 {recv_poll_data, wait_poll_data},

                 {poll_next_addr, polling},
                 {send_poll_data, wait_poll_data}
                 ]},

                {poll_response_data,
                [{poll_data_completed, idle},
                 {poll_start, polling},
                 {poll_stop, idle},

                 {recv_poll_seq, poll_response_data},
                 {send_poll_pbm, poll_response_data},
                 {wait_ok, poll_response_data},
                 {send_poll_data, idle}
                ]},

                {poll_response_pbm,
                 [{recv_poll_seq, poll_response_pbm},

                 {recv_poll_pbm, poll_response_pbm},
                 {recv_poll_data, poll_response_pbm},

                 {send_poll_pbm, poll_response_data},
                 {no_poll_data, idle},

                 {poll_start, polling},
                 {poll_stop, idle}

                 %{send_poll_data, poll_response_pbm}
                 ]},

                {alarm,
                 [{final, alarm}
                 ]},

                {final, []}
               ]).

start_link(SM) -> fsm:start_link(SM).
init(SM)       -> SM.
trans()        -> ?TRANS.
final()        -> [alarm].
init_event()   -> eps.
stop(_SM)      -> ok.

%%--------------------------------Handler functions-------------------------------
handle_event(MM, SM, Term) ->
  %?INFO(?ID, "HANDLE EVENT  ~p   ~p ~n", [MM, SM]),
  ?TRACE(?ID, "~p~n", [Term]),
  SeqPollAddrs = share:get(SM, polling_seq),
  Local_address = share:get(SM, local_address),

  % case Local_address of
  %   2 ->
  %    io:format("!!!!!!!!!!!!! handle_event ~p: ~p ~p ~p~n", [Local_address, Term, SM#sm.event, SM#sm.state]);
  %   1 ->
  %    io:format("!!!!!!!!!!!!! handle_event ~p: ~p ~p ~p~n", [Local_address, Term, SM#sm.event, SM#sm.state]);
  %     _ -> nothing
  % end,

  io:format("!!!!!!!!!!!!! handle_event ~p: ~p ~p ~p~n", [Local_address, Term, SM#sm.event, SM#sm.state]),
  case Term of
    {timeout, answer_timeout} ->
      TupleResponse = {response, {error, <<"ANSWER TIMEOUT">>}},
      fsm:cast(SM, polling_mux, {send, TupleResponse}),
      SM;

    {timeout, {send_burst, SBurstTuple}} ->
      fsm:run_event(MM, SM#sm{event = send_poll_pbm, event_params = SBurstTuple}, {});
    {timeout, {retransmit_im, STuple}} ->
      fsm:run_event(MM, SM#sm{event = retransmit_im}, {retransmit_im, STuple});
    {timeout, {retransmit_pbm, STuple}} ->
      fsm:run_event(MM, SM#sm{event = recv_poll_seq, event_params = STuple}, {});
    {timeout, Event} ->
      fsm:run_event(MM, SM#sm{event = Event}, {});
    {connected} ->
      ?INFO(?ID, "connected ~n", []),
      fsm:run_event(MM, SM#sm{event=internal}, {});
    {rcv_ul, {nl, send, _TransmitLen, Local_address, _MsgType, _Payload}} ->
      fsm:cast(SM, polling_mux, {send, {response, {nl, error}}}),
      SM;
    {rcv_ul, STuple = {nl, send, _TransmitLen, _IDst, _MsgType, _Payload}} ->
      fsm:cast(SM, polling_mux, {send, {response, {nl, ok}}}),
      add_to_CDT_queue(SM, STuple),
      SM;
    {rcv_ul, {nl, send, _TransmitLen, _IDst, _Payload}} ->
      % Is it neceessary to add the msg type???
      fsm:cast(SM, polling_mux, {send, {response, {nl, error}}}),
      SM;
    {rcv_ul, {set, polling_seq, LSeq}} ->
      fsm:cast(SM, polling_mux, {send, {response, {nl, ok}}}),
      init_poll_index(SM),
      share:put(SM, polling_seq, LSeq),
      SM;
    {rcv_ul, {set, polling, start}} when (SeqPollAddrs =/= []) ->
      SM1 = fsm:clear_timeouts(SM),
      fsm:cast(SM1, polling_mux, {send, {response, {nl, ok}}}),
      fsm:run_event(MM, SM1#sm{event = poll_start}, {});
    {rcv_ul, {set, polling, start}} ->
      fsm:cast(SM, polling_mux, {send, {response, {nl, seq, empty}}}),
      SM;
    {rcv_ul, {set, polling, stop}} ->
      SM1 = fsm:clear_timeouts(SM),
      fsm:cast(SM, polling_mux, {send, {response, {nl, ok}}}),
      fsm:run_event(MM, SM1#sm{event = poll_stop}, {});

    {async, {pid, _Pid}, RTuple = {recv, _Len, _Src, Local_address , _,  _,  _,  _,  _, _Data}} ->
      fsm:run_event(MM, SM#sm{event = recv_poll_data}, {recv_poll_data, RTuple});
    {async, {pid, _Pid}, RTuple = {recvpbm, _Len, _Src, Local_address, _, _, _, _, _Data}} ->
      fsm:run_event(MM, SM#sm{event = process_poll_pbm}, {recv_poll_pbm, RTuple});
    {async, {pid, Pid}, {recvim, Len, Src, Local_address, Flag, _, _, _,_, Data}} ->
      RecvTuple = {recv, {Pid, Len, Src, Local_address, Flag, Data}},
      fsm:run_event(MM, SM#sm{event = recv_poll_seq, event_params = RecvTuple}, {});

    {async, {pid, _Pid}, {recvim, _Len, _Src, _Dst, _Flag, _, _, _, _, _Data}} ->
      % TODO: ooverhearing
      SM;
    {async, PosTerm = {usbllong, _FCurrT, _FMeasT, _Src, _X, _Y, _Z,
                      _E, _N, _U, _Roll, _Pitch, _Yaw, _P, _Rssi, _I, _Acc}} ->

      fsm:cast(SM, polling_nmea, {send, PosTerm}),
      SM;
    {async,{delivered, _, _Src}} ->
      %TODO: clear_queue(SM, Src, dsensitive),
      SM;
    {async, _Tuple} ->
      SM;
    {sync, "*SEND", "OK"} ->
      SM1 = fsm:clear_timeout(SM, answer_timeout),
      SM2 = nl_mac_hf:clear_spec_timeout(SM1, send_burst),
      fsm:run_event(MM, SM2#sm{event = send_poll_data}, {});
    {sync, _Req, _Answer} ->
      SM1 = fsm:clear_timeout(SM, answer_timeout),
      SM1;

    {nl, error} ->
      fsm:cast(SM, polling_mux, {send, {response, {nl, error}}});

    UUg ->
      ?ERROR(?ID, "~s: unhandled event:~p~n", [?MODULE, UUg]),
      SM
  end.

handle_idle(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  case SM#sm.event of
    internal ->
      init_poll(SM),
      fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, init_counter);
      %SM#sm{event = init_counter};
    init_counter ->
      SM1 = fsm:send_at_command(SM, {at, "?PC", ""}),
      SM1#sm{event = eps};
    _ ->
      SM#sm{event = eps}
  end.

handle_polling(_MM, #sm{event = poll_next_addr} = SM, Term) ->
  ?TRACE(?ID, "handle_polling ~120p~n", [Term]),
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),

  %TODO: PID ?
  STuple = create_CDT_msg(SM),

  Current_poll_addr = get_current_poll_addr(SM),
  io:format("handle_polling ======================================>  ~p ~n", [Current_poll_addr]),

  case Answer_timeout of
    true ->
      fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {retransmit_im, STuple});
    false ->
      SM1 = fsm:send_at_command(SM, STuple),
      SM1#sm{event = send_poll_data}
  end;
handle_polling(_MM, #sm{event = retransmit_im} = SM, Term) ->
  ?TRACE(?ID, "handle_polling ~120p~n", [Term]),
  SM#sm{event = poll_next_addr};
handle_polling(_MM, #sm{event = poll_start} = SM, Term) ->
  ?TRACE(?ID, "handle_polling ~120p~n", [Term]),
  init_poll_index(SM),
  SM#sm{event = poll_next_addr};
handle_polling(_MM, SM, Term) ->
  ?TRACE(?ID, "handle_polling ~120p~n", [Term]),
  SM#sm{event = eps}.


handle_wait_poll_pbm(_MM, SM, Term = {recv_poll_pbm, Pbm}) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  % TODO: if pbm contains burst data flag, wait till recv_poll_data
  % if not, directly to poll next address
  % or timeout, nothing was received

  Current_poll_addr = get_current_poll_addr(SM),
  {recvpbm, _, Src, _, _, _, _, _, _} = Pbm,

  case Src of
    Current_poll_addr ->
      ExtrLen = extract_VDT_pbm_msg(SM, Pbm),

      io:format("<<<<  Extr ~p ~120p~n", [Src, ExtrLen]),

      case ExtrLen of
        0 ->
          poll_next_addr(SM),
          SM#sm{event = poll_next_addr};
        _ ->
          SM#sm{event = recv_poll_pbm}
      end;
    _ ->
      % TODO: Maybe timetout, if no conn to VDT, no pbm
      SM#sm{event = eps}
  end;
handle_wait_poll_pbm(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  SM#sm{event = eps}.


handle_wait_poll_data(_MM, SM, Term = {recv_poll_data, RTuple}) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  Current_poll_addr = get_current_poll_addr(SM),
  {recv, _, Src, _, _, _, _, _, _, Payload} = RTuple,

  VDT_msg = extract_VDT_msg(SM, Payload),
  case Src of
    Current_poll_addr ->
      io:format("<<<<  VDT_msg ~p ~120p~n", [Src, VDT_msg]),
      poll_next_addr(SM),
      SM#sm{event = poll_next_addr};
    _ ->
      SM#sm{event = eps}
  end;
handle_wait_poll_data(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  SM#sm{event = eps}.

handle_poll_response_data(_MM, #sm{event = send_poll_pbm} = SM, _Term) ->
  Local_address = share:get(SM, local_address),
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),
  Event_params = SM#sm.event_params,
  % TODO: processed data!

  io:format("!!!!!!!!!!!!!!!!1 ~p~n", [Event_params]),

  {recv, {Pid, _Len, Src, Dst, _Flag, _}} = Event_params,
  Data = create_VDT_msg(SM, Src),
  case Data of
    nothing ->
      SM#sm{event = poll_data_completed};
    _ when Local_address == Dst ->
      SBurstTuple = {at, {pid, Pid}, "*SEND", Src, Data},
      case Answer_timeout of
        true ->
          % !!! TODO: retry in some ms, check!
          fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {send_burst, Event_params});
        false ->
          SM1 = fsm:send_at_command(SM, SBurstTuple),
          SM1#sm{event = wait_ok, event_params = Event_params}
          %fsm:set_timeout(SM1#sm{event = wait_ok}, ?ANSWER_TIMEOUT, {send_burst, Event_params})
      end;
      _ ->
        SM#sm{event = poll_data_completed}
  end;
handle_poll_response_data(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  SM#sm{event = eps}.

handle_poll_response_pbm(_MM, #sm{event = recv_poll_seq} = SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),
  Event_params = SM#sm.event_params,

  {recv, {Pid, _Len, Src, _Dst, _Flag, _}} = Event_params,
  Data = create_VDT_pbm_msg(SM, Src),

  case Answer_timeout of
    true ->
      % !!! TODO: retry in some ms, check!
      fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {retransmit_pbm, Event_params} );
    false ->
      SPMTuple = {at, {pid, Pid}, "*SENDPBM", Src, Data},
      SM1 = fsm:send_at_command(SM, SPMTuple),
      SM1#sm{event = send_poll_pbm, event_params = Event_params}
  end;
handle_poll_response_pbm(_MM, SM, _Term) ->
  SM#sm{event = eps}.

-spec handle_alarm(any(), any(), any()) -> no_return().
handle_alarm(_MM, SM, _Term) ->
  exit({alarm, SM#sm.module}).

handle_final(_MM, SM, Term) ->
  ?TRACE(?ID, "Final ~120p~n", [Term]).

%%--------------------------------------Helper functions------------------------
init_poll(SM)->
  share:put(SM, polling_seq, []),
  init_poll_index(SM).

init_poll_index(SM) ->
  share:put(SM, current_polling_i, 1).

poll_next_addr(SM) ->
  LSeq = share:get(SM, polling_seq),
  Current_polling_i = share:get(SM, current_polling_i),
  Ind =
  if Current_polling_i >= length(LSeq) -> 1;
  true -> Current_polling_i + 1
  end,
  share:put(SM, current_polling_i, Ind),
  get_current_poll_addr(SM).

get_current_poll_addr(SM) ->
  LSeq = share:get(SM, polling_seq),
  Ind = share:get(SM, current_polling_i),
  lists:nth(Ind, LSeq).

%extract_CDT_msg(SM, _Msg) ->
  % TODO: check LA, parse ther Rest of the mesg
  % Local_address = share:get(SM, local_address),
  % {recvpbm, _Len, _Dst, _Src, _, _Rssi, _Int, _, Payload} = Msg,
  % CBitsPosLen = nl_mac_hf:count_flag_bits(63),
  % <<"C", PLen:CBitsPosLen, RPayload/bitstring>> = Payload,
  % <<Pos:PLen/binary, Rest/bitstring>> = RPayload,
  % Data_bin = (bit_size(Rest) rem 8) =/= 0,
  % CDTData =
  % if Data_bin =:= false ->
  %   Add = bit_size(Rest) rem 8,
  %   <<_:Add, Data/binary>> = Rest,
  %   Data;
  % true ->
  %   Rest
  % end,
  % io:format("!!!!!!! extract_CDT_msg   ~p   ~p  ~p ~p~n", [Local_address, Pos, PLen, PayloadData]),
  % SM.


extract_VDT_msg(_SM, Payload) ->
  try
    <<"V", DataVDT/binary>> = Payload,
    DataVDT
  catch error: _Reason -> ignore
  end.

extract_VDT_pbm_msg(SM, Pbm) ->
  try
    Local_address = share:get(SM, local_address),
    {recvpbm, _Len, _Dst, Local_address, _, _Rssi, _Int, _, Payload} = Pbm,
    <<"VPbm", BLenBurst/binary>> = Payload,
    binary:decode_unsigned(BLenBurst, big)
  catch error: _Reason -> 0 %!!! ignore
  end.

create_VDT_msg(SM, Dst) ->
  DstA = binary_to_atom(integer_to_binary(Dst), utf8),

  DS = queue_to_data(SM, DstA, dsensitive),
  DT = queue_to_data(SM, DstA, dtolerant),
  Len = byte_size(DS) + byte_size(DT),
  case Len of
    0 -> nothing;
    _ ->
    DataVDT = list_to_binary([DS, DT]),
    <<"V", DataVDT/binary>>
  end.

create_VDT_pbm_msg(SM, Dst) ->
  %Local_address = share:get(SM, local_address),
  DstA = binary_to_atom(integer_to_binary(Dst), utf8),

  DS = queue_to_data(SM, DstA, dsensitive),
  DT = queue_to_data(SM, DstA, dtolerant),
  Len = byte_size(DS) + byte_size(DT),
  BLenBurst = binary:encode_unsigned(Len, big),
  <<"VPbm", BLenBurst/binary>>.

create_CDT_msg(SM) ->
  % TODO: DATA? Fused Position + CDT ?
  % Burst fomt CDT? or dummy with POS?
  Current_polling_addr = get_current_poll_addr(SM),
  Dst = binary_to_atom(integer_to_binary(Current_polling_addr), utf8),

  DS = queue_to_data(SM, Dst, dsensitive),
  DT = queue_to_data(SM, Dst, dtolerant),

  DataPos = <<"POS....">>,
  LenDataPos = byte_size(DataPos),
  DataCDT = list_to_binary([DS, DT]),

  % TODO: Set max pos len
  CBitsPosLen = nl_mac_hf:count_flag_bits(63),
  BLenPos = <<LenDataPos:CBitsPosLen>>,

  TmpData = <<"C", BLenPos/bitstring, DataPos/binary, DataCDT/binary>>,

  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  Data =
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"C", BLenPos/bitstring, DataPos/binary, 0:Add, DataCDT/binary>>;
  true ->
    TmpData
  end,

  LenData = byte_size(Data),

  if LenData < 64 ->
    {at, {pid, 0},"*SENDIM", Current_polling_addr, ack, Data};
  true ->
    {at, {pid, 0},"*SEND", Current_polling_addr, Data}
  end.

queue_to_data(SM, Dst, Name) ->
  Qname = list_to_atom(atom_to_list(Name) ++ atom_to_list(Dst)),
  Q = share:get(SM, Qname),
  case Q of
    nothing -> <<"">>;
    {[],[]} -> <<"">>;
    _ ->
      LData = queue:to_list(Q),
      F = fun(X) -> {_Time, _Type, _Len, Data} = X, Data end,
      list_to_binary(lists:join(<<"">>, [ F(X)  || X <- LData]))
  end.

add_to_CDT_queue(SM, STuple) ->
  % TODO: maybe we have to same messages to a file
  {nl, send, TransmitLen, IDst, MsgType, Payload} = STuple,
  ADst = binary_to_atom(integer_to_binary(IDst), utf8),
  Qname = list_to_atom(atom_to_list(MsgType) ++ atom_to_list(ADst)),

  QShare = share:get(SM, Qname),
  Q =
  if QShare == nothing ->
    queue:new();
  true ->
    QShare
  end,
  Item = {erlang:monotonic_time(micro_seconds), MsgType, TransmitLen, <<Payload/binary>>},
  NQ =
  case MsgType of
    dtolerant ->
      queue:in(Item, Q);
    dsensitive ->
      %TODO: configurable parameter Max
      nl_mac_hf:queue_limited_push(Q, Item, 3)
  end,
  share:put(SM, Qname, NQ),
  ?TRACE(?ID, "add_to_CDT_queue ~p~n", [share:get(SM, Qname)]).

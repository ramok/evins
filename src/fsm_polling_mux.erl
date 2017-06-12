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
-compile({parse_transform, pipeline}).

-include("fsm.hrl").

-export([start_link/1, trans/0, final/0, init_event/0]).
-export([init/1, handle_event/3, stop/1]).

-export([handle_polling/3, handle_wait_poll_pbm/3, handle_wait_poll_data/3]).
-export([handle_poll_response_pbm/3, handle_poll_response_data/3]).
-export([handle_idle/3, handle_alarm/3, handle_final/3]).

-define(TRANS, [
                {idle,
                 [{internal, idle},
                 {poll_start, polling},
                 {poll_stop, idle},
                 {recv_poll_seq, poll_response_pbm},

                 {recv_poll_data, idle},
                 {recv_poll_pbm, idle},
                 {process_poll_pbm, idle},

                 {send_poll_data, idle}
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
                 {recv_poll_seq, wait_poll_pbm},

                 {poll_next_addr, polling}
                 ]},

                {wait_poll_data,
                 [
                 {poll_start, polling},
                 {poll_stop, idle},

                 {recv_poll_pbm, wait_poll_data},
                 {recv_poll_data, wait_poll_data},

                 {process_poll_pbm, wait_poll_data},

                 {poll_next_addr, polling},
                 {send_poll_data, wait_poll_data}
                 ]},

                {poll_response_data,
                [{poll_data_completed, idle},
                 {poll_start, polling},
                 {poll_stop, idle},
                 {send_next_poll_data, poll_response_data},
                 {recv_poll_seq, poll_response_data},
                 {send_poll_pbm, poll_response_data},
                 {wait_pc, poll_response_data},
                 {send_poll_data, idle}
                ]},

                {poll_response_pbm,
                 [{recv_poll_seq, poll_response_pbm},

                 {recv_poll_pbm, poll_response_pbm},
                 {recv_poll_data, poll_response_pbm},

                 {send_poll_pbm, poll_response_data},
                 {no_poll_data, idle},

                 {poll_start, polling},
                 {poll_stop, idle},

                 {ignore_recv_poll_seq, idle},
                 {send_poll_data, poll_response_pbm}
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
init_event()   -> internal.
stop(_SM)      -> ok.

%%--------------------------------Handler functions-------------------------------
handle_event(MM, SM, Term) ->
  ?INFO(?ID, ">> HANDLE EVENT ~p ~p Term:~p~n", [SM#sm.state, SM#sm.event, Term]),
  %% ?TRACE(?ID, "~p~n", [Term]),
  SeqPollAddrs = share:get(SM, polling_seq),
  Local_address = share:get(SM, local_address),

  Wait_pc = fsm:check_timeout(SM, wait_pc),
  Send_next_poll_data = fsm:check_timeout(SM, send_next_poll_data_tmo),
  Event_params = SM#sm.event_params,

  case Term of
    {timeout, answer_timeout} ->
      %% TODO: why?
      TupleResponse = {nl, error, <<"ANSWER TIMEOUT">>},
      fsm:cast(SM, nl_impl, {send, TupleResponse});
    {timeout, wait_recv_pbm} ->
      poll_next_addr(SM),
      fsm:run_event(MM, SM#sm{event = poll_next_addr}, {});
    {timeout, wait_rest_poll_data} ->
      poll_next_addr(SM),
      fsm:run_event(MM, SM#sm{event = poll_next_addr}, {});
    {timeout, send_next_poll_data_tmo} ->
      SM;
    {timeout, {send_burst, SBurstTuple}} ->
      fsm:run_event(MM, SM#sm{event = send_poll_pbm, event_params = SBurstTuple}, {});
    {timeout, {retransmit_im, STuple}} ->
      fsm:run_event(MM, SM#sm{event = retransmit_im}, {retransmit_im, STuple});
    {timeout, {retransmit_pbm, STuple}} ->
      fsm:run_event(MM, SM#sm{event = recv_poll_seq, event_params = STuple}, {});
    {timeout, {retransmit, send_next_poll_data}} ->
      fsm:run_event(MM, SM#sm{event = send_next_poll_data}, Event_params);
    {timeout, Event} ->
      fsm:run_event(MM, SM#sm{event = Event}, {});
    {connected} ->
      SM;
    {disconnected, _} ->
      SM;
    {allowed} ->
      ?INFO(?ID, "allowed ~n", []),
      fsm:send_at_command(SM, {at, "?PC", ""});
    {nl, get, protocol} ->
      ProtocolName = share:get(SM, nl_protocol),
      fsm:cast(SM, nl_impl, {send, {nl, protocol, ProtocolName}});
    {nl, get, address} ->
      fsm:cast(SM, nl_impl, {send, {nl, address, Local_address}});
    {nl, set, address, _} ->
      fsm:cast(SM, nl_impl, {send, {nl, address, error}});
    {nl, flush, buffer} ->
      [
       clear_buffer(__),
       fsm:clear_timeouts(__),
       fsm:cast(__, nl_impl, {send, {nl, buffer, ok}})
      ] (SM);
    {nl, send, _MsgType, Local_address, _Payload} ->
      fsm:cast(SM, nl_impl, {send, {nl, send, error}});
    {nl, send, broadcast, _IDst, Payload} ->
      TransmitLen = byte_size(Payload),
      if TransmitLen < 50 ->
          [
           add_to_CDT_queue(__, Term),
           fsm:cast(__, nl_impl, {send, {nl, send, ok}})
          ] (SM);
         true ->
          fsm:cast(SM, nl_impl, {send, {nl, send, error}})
      end;
    {nl, send, _MsgType, _IDst, _Payload} ->
      [
       add_to_CDT_queue(__, Term),
       fsm:cast(__, nl_impl, {send, {nl, send, ok}})
      ] (SM);
    {nl, send, _IDst, _Payload} ->
      %% Is it neceessary to add the msg type???
      fsm:cast(SM, nl_impl, {send, {nl, send, error}});
    {nl, set, polling, LSeq} when is_list(LSeq) ->
      [
       init_poll_index(__),
       share:put(__, polling_seq, LSeq),
       fsm:cast(__, nl_impl, {send, {nl, polling, LSeq}})
       ] (SM);
    {nl, start, polling, Flag} when (SeqPollAddrs =/= []) ->
      [
       fsm:clear_timeouts(__),
       share:put(__, poll_flag_burst, Flag),
       fsm:cast(__, nl_impl, {send, {nl, polling, ok}}),
       fsm:set_event(__, poll_start),
       fsm:run_event(MM, __, {})
      ] (SM);
    {nl, start, polling, _Flag} ->
      fsm:cast(SM, nl_impl, {send, {nl, polling, error}});
    {nl, stop, polling} ->
      [
       fsm:clear_timeouts(__),
       fsm:cast(__, nl_impl, {send, {nl, polling, ok}}),
       fsm:set_event(__, poll_stop),
       fsm:run_event(MM, __, {})
      ] (SM);
    {nl, get, routing} ->
      Answer = {nl, routing, nl_mac_hf:routing_to_list(SM)},
      fsm:cast(SM, nl_impl, {send, Answer});
    {async, {pid, _Pid}, RTuple = {recv, _Len, Src, Local_address , _,  _,  _,  _,  _, Data}} ->
      Poll_data = extract_poll_data(SM, Data),
      case Poll_data of
        ignore -> SM;
        _ ->
          fsm:cast(SM, nl_impl, {send, {nl, recv, Local_address, Src, Poll_data}}),
          io:format("<<<<  Poll_data ~p ~p~n", [Src, Poll_data]),
          fsm:run_event(MM, SM#sm{event = recv_poll_data}, {recv_poll_data, RTuple})
      end;
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
      %% TODO: create NMEA string here
      fsm:cast(SM, nmea, {send, PosTerm});
    {async, {deliveredim, Src}} ->
      BroadcastExist = get_share_var(Src, broadcast),
      BroadcastExistMsg = share:get(SM, BroadcastExist),
      case BroadcastExistMsg of
        nothing -> SM;
        _ -> share:clean(SM, BroadcastExist)
      end;
    {async, {delivered, PC, Src}} ->
      delete_delivered_item(SM, tolerant, PC, Src),
      delete_delivered_item(SM, sensitive, PC, Src),
      SM;
    {async, Tuple = {failedim, _}} ->
      io:format(">>>>>>>>>>>>> Failed ~p ~p ~p~n", [?ID, SM#sm.state, Tuple]),
      SM;
    % TODO: ack, pbm was delivered?
    {async, {sendstart, Src, _, _, _}} ->
      BroadcastExist = get_share_var(Src, broadcast),
      BroadcastExistMsg = share:get(SM, BroadcastExist),
      case BroadcastExistMsg of
        {_, broadcast, sent, _, _} ->
          share:clean(SM, BroadcastExist),
          SM;
        _ -> SM
      end;
    {async, _Tuple} ->
      SM;
    {sync,"?PC", PC} when Wait_pc == true ->
      {_, ITuple, _} = Event_params,
      [
       fsm:clear_timeout(__, wait_pc),
       fsm:clear_timeout(__, answer_timeout),
       init_packet_counter(__, PC),
       change_status_item(__, list_to_integer(PC), ITuple),
       fsm:set_event(__, send_next_poll_data),
       fsm:run_event(MM, __, Event_params)
      ] (SM);
    {sync,"?PC", PC} ->
      [
       fsm:clear_timeout(__, answer_timeout),
       init_packet_counter(__, PC)
      ] (SM);
    {sync, "*SEND", "OK"} when Send_next_poll_data == true ->
      [
       fsm:clear_timeout(__, send_next_poll_data_tmo),
       nl_mac_hf:clear_spec_timeout(__, send_burst),
       fsm:clear_timeout(__, answer_timeout),
       fsm:send_at_command(__, {at, "?PC", ""}),
       fsm:set_timeout(__#sm{event_params = Event_params}, ?ANSWER_TIMEOUT, wait_pc)
      ] (SM);
    {sync, "*SEND", "OK"} ->
      [
       fsm:clear_timeout(__, answer_timeout),
       nl_mac_hf:clear_spec_timeout(__, send_burst),
       fsm:set_event(__, send_poll_data),
       fsm:run_event(MM, __, {})
      ] (SM);
    {sync, _Req, _Answer} ->
      fsm:clear_timeout(SM, answer_timeout);
    {nl, error} ->
      fsm:cast(SM, nl_impl, {send, {nl, error}});
    _ when MM#mm.role == nl_impl ->
      fsm:cast(SM, nl_impl, {send, {nl, error}});
    UUg ->
      ?ERROR(?ID, "~s: unhandled event:~p~n", [?MODULE, UUg]),
      SM
  end.

handle_idle(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  case SM#sm.event of
    internal ->
      init_poll(SM),
      SM#sm{event = eps};
    _ ->
      SM#sm{event = eps}
  end.

handle_polling(_MM, #sm{event = poll_next_addr} = SM, Term) ->
  ?TRACE(?ID, "handle_polling ~120p~n", [Term]),
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),

  % TODO: PID ?
  % TODO: multiple burst messages, what msg is the request msg?
  % TODO: remove delivered message

  Current_poll_addr = get_current_poll_addr(SM),
  Poll_flag_burst = share:get(SM, poll_flag_burst),

  BroadcastExist = get_share_var(Current_poll_addr, broadcast),
  BroadcastExistMsg = share:get(SM, BroadcastExist),

  STuple = create_CDT_msg(SM, Poll_flag_burst, BroadcastExistMsg),

  io:format("handle_polling ======================================> ~p ~p ~p~n", [Poll_flag_burst, Current_poll_addr, BroadcastExistMsg]),
  
  case Answer_timeout of
    true ->
      fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {retransmit_im, STuple});
    false ->
      Time_wait_recv = share:get(SM, time_wait_recv),
      [
       fsm:send_at_command(__, STuple),
       fsm:set_timeout(__, {s, Time_wait_recv}, wait_recv_pbm),
       fsm:set_event(__, send_poll_data)
      ] (SM)
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
  % if pbm contains burst data flag, wait till recv_poll_data
  % if not, directly to poll next address
  % or timeout, nothing was received

  Current_poll_addr = get_current_poll_addr(SM),
  {recvpbm, _, Src, Dst, _, _, _, _, _} = Pbm,

  case Src of
    Current_poll_addr ->
      [ExtrLen, BroadcastData] = extract_VDT_pbm_msg(SM, Pbm),

      io:format("<<<<  Extr ~p ~120p~n", [Src, ExtrLen]),

      if BroadcastData =/= <<>> ->
        fsm:cast(SM, nl_impl, {send, {nl, recv, Dst, Src, BroadcastData}});
      true -> nothing
      end,

      case ExtrLen of
        0 ->
          poll_next_addr(SM),
          SM#sm{event = poll_next_addr};
        _ ->
          [
           fsm:set_timeout(__, {s, 20}, wait_rest_poll_data),
           fsm:set_event(__#sm{event_params = {poll_data_len, ExtrLen}}, recv_poll_pbm)
          ] (SM)
      end;
    _ ->
      % if no conn to VDT, no pbm, wait till timeout and poll next address
      SM#sm{event = eps}
  end;
handle_wait_poll_pbm(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  SM#sm{event = eps}.

handle_wait_poll_data(_MM, SMW, Term = {recv_poll_data, RTuple}) ->
  SM = fsm:clear_timeout(SMW, wait_rest_poll_data),
  ?TRACE(?ID, "~120p~n", [Term]),
  Current_poll_addr = get_current_poll_addr(SM),
  {recv, _, Src, _Dst, _, _, _, _, _, Payload} = RTuple,

  Event_params = SM#sm.event_params,
  Poll_data_len_whole =
  case Event_params of
    {poll_data_len, ExtrLen} -> ExtrLen;
    _ -> 0
  end,

  Poll_data = extract_poll_data(SM, Payload),
  Poll_data_len = byte_size(Poll_data),
  WaitingRestData = Poll_data_len_whole - Poll_data_len,

  case Src of
    Current_poll_addr when WaitingRestData =< 0->
      poll_next_addr(SM),
      SM#sm{event = poll_next_addr};
    _ ->
      Time_wait_recv = share:get(SM, time_wait_recv),
      [
       fsm:set_timeout(__, {s, Time_wait_recv}, wait_rest_poll_data),
       fsm:set_event(__#sm{event_params = {poll_data_len, WaitingRestData}}, eps)
      ] (SM)
  end;
handle_wait_poll_data(_MM, SM, Term) ->
  ?TRACE(?ID, "~120p~n", [Term]),
  SM#sm{event = eps}.

handle_poll_response_data(_MM, #sm{event = send_next_poll_data} = SM, {}) ->
  SM#sm{event = eps};
handle_poll_response_data(_MM, #sm{event = send_next_poll_data} = SM, Term) ->
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),
  case Answer_timeout of
    true ->
      fsm:set_timeout(SM#sm{event_params = Term}, ?ANSWER_TIMEOUT, {retransmit, send_next_poll_data} );
    false ->
      {Event_params, {_MsgType, _LocalPC, SBurstTuple}, Tail} = Term,
      {recv, {Pid, _Len, Src, _Dst, _Flag, _}} = Event_params,
      SM1 = fsm:send_at_command(SM, SBurstTuple),
      [NTail, NType, LocalPC, NPayload] = create_next_poll_data_response(SM, Tail),

      case NPayload of
        nothing ->
          SM1#sm{event = poll_data_completed};
        _ ->
          NSBurstTuple = {at, {pid, Pid}, "*SEND", Src, NPayload},
          NT = {Event_params, {NType, LocalPC, NSBurstTuple}, NTail},
          fsm:set_timeout(SM1#sm{event_params = NT}, ?ANSWER_TIMEOUT, send_next_poll_data_tmo)
      end
  end;

handle_poll_response_data(_MM, #sm{event = send_poll_pbm} = SM, _Term) ->
  Local_address = share:get(SM, local_address),
  Answer_timeout = fsm:check_timeout(SM, answer_timeout),
  Event_params = SM#sm.event_params,

  {recv, {Pid, _Len, Src, Dst, _Flag, _}} = Event_params,

  [Tail, MsgType, LocalPC, Data] = create_poll_data_response(SM, Src),
  case Data of
    nothing ->
      SM#sm{event = poll_data_completed};
    _ when Local_address == Dst ->
      case Answer_timeout of
        true ->
          fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {send_burst, Event_params});
        false ->
          SBurstTuple = {at, {pid, Pid}, "*SEND", Src, Data},
          Tuple = {Event_params, {MsgType, LocalPC, SBurstTuple}, Tail},
          [
           fsm:send_at_command(__, {at, "?PC", ""}),
           fsm:set_timeout(__#sm{event_params = Tuple}, ?ANSWER_TIMEOUT, wait_pc)
          ] (SM)
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

  case Answer_timeout of
    true ->
      fsm:set_timeout(SM#sm{event = eps}, ?ANSWER_TIMEOUT, {retransmit_pbm, Event_params} );
    false ->
      {recv, {Pid, _Len, Src, Dst, _Flag, DataCDT}} = Event_params,
      [Poll_data_len, Poll_data, Position, PollingFlagBurst, Process] = extract_CDT_msg(SM, DataCDT),

      case Process of
        ignore ->
          SM#sm{event = ignore_recv_poll_seq};
        answer_pbm ->
          if Poll_data_len > 0 ->
            fsm:cast(SM, nl_impl, {send, {nl, recv, Src, Dst, Poll_data}});
          true -> nothing
          end,
          fsm:cast(SM, nmea, {send, Position}),

          BroadcastExist = get_share_var(Src, broadcast),
          BroadcastExistMsg = share:get(SM, BroadcastExist),

          Data = create_VDT_pbm_msg(SM, Src, PollingFlagBurst, BroadcastExistMsg),
          SPMTuple = {at, {pid, Pid}, "*SENDPBM", Src, Data},
          
          SM1 = fsm:send_at_command(SM, SPMTuple),
          case PollingFlagBurst of
            nb -> SM1#sm{event = ignore_recv_poll_seq};
            b -> SM1#sm{event = send_poll_pbm, event_params = Event_params}
          end
      end
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
  share:put(SM, polling_seq_msgs, []),
  share:put(SM, local_pc, 1),
  share:put(SM, polling_pc, 1),
  share:put(SM, recv_packages, []),
  share:put(SM, poll_flag_burst, nb),
  init_poll_index(SM).

init_packet_counter(SM, PC) ->
  PCI = list_to_integer(PC),
  share:put(SM, packet_counter, PCI).

increase_local_pc(SM, CounterName) ->
  PC = share:get(SM, CounterName),
  if PC > 127 ->
    share:put(SM, CounterName, 1);
  true ->
    share:put(SM, CounterName, PC + 1)
  end.

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

clear_buffer(SM) ->
  LSeq = share:get(SM, polling_seq_msgs),
  F = fun(X) ->
    QSens = get_share_var(X, sensitive),
    share:put(SM, QSens, queue:new()),
    QTol = get_share_var(X, tolerant),
    share:put(SM, QTol, queue:new()),
    BroadcastExist = get_share_var(X, broadcast),
    share:clean(SM, BroadcastExist)
  end,
  [ F(X) || X <- LSeq],
  SM.

get_share_var(Dst, Name) ->
  DstA = binary_to_atom(integer_to_binary(Dst), utf8),
  list_to_atom(atom_to_list(Name) ++ atom_to_list(DstA)).

extract_poll_data(_SM, Payload) ->
  try
    <<"V", DataVDT/binary>> = Payload,
    DataVDT
  catch error: _Reason -> ignore
  end.

extract_VDT_pbm_msg(SM, Pbm) ->
  try
    Local_address = share:get(SM, local_address),
    {recvpbm, _Len, _Dst, Local_address, _, _Rssi, _Int, _, Payload} = Pbm,

    Max_burst_len = share:get(SM, max_burst_len),
    CMaxBurstLenBits = nl_mac_hf:count_flag_bits(Max_burst_len),


    <<"VPbm", BLenBurst:CMaxBurstLenBits, Rest/bitstring>> = Payload,
    Data_bin = (bit_size(Payload) rem 8) =/= 0,
    BroadCastData =
    if Data_bin =:= false ->
      Add = bit_size(Rest) rem 8,
      <<_:Add, Data/binary>> = Rest,
      Data;
    true ->
      Rest
    end,
    [BLenBurst, BroadCastData]

  catch error: _Reason -> [0, <<>>]
  end.

create_VDT_pbm_msg(SM, Dst, FlagBurst, nothing) ->
  Max_burst_len = share:get(SM, max_burst_len),
  CMaxBurstLenBits = nl_mac_hf:count_flag_bits(Max_burst_len),
  Len =
  case FlagBurst of
    nb -> 0;
    b -> get_length_all_msgs(SM, Dst)
  end,

  BLenBurst = <<Len:CMaxBurstLenBits>>,

  TmpData = <<"VPbm", BLenBurst/bitstring>>,
  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"VPbm", BLenBurst/bitstring, 0:Add>>;
  true ->
    TmpData
  end;
create_VDT_pbm_msg(SM, Dst, FlagBurst, BroadcastExistMsg) ->
  BroadcastExist = get_share_var(Dst, broadcast),

  Max_burst_len = share:get(SM, max_burst_len),
  CMaxBurstLenBits = nl_mac_hf:count_flag_bits(Max_burst_len),

  {Time, broadcast, _Status, TransmitLen, BroadcastPayload} = BroadcastExistMsg,
  NTuple = {Time, broadcast, sent, TransmitLen, BroadcastPayload},
  share:put(SM, BroadcastExist, NTuple),

  Len =
  case FlagBurst of
    nb -> 0;
    b -> get_length_all_msgs(SM, Dst)
  end,

  BLenBurst = <<Len:CMaxBurstLenBits>>,

  TmpData = <<"VPbm", BLenBurst/bitstring, BroadcastPayload/binary>>,
  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"VPbm", BLenBurst/bitstring, 0:Add, BroadcastPayload/binary>>;
  true ->
    TmpData
  end.

create_next_poll_data_response(_SM, LTransmission) ->
  case LTransmission of
    nothing ->
      [nothing, nothing, nothing, nothing];
    [] ->
      [nothing, nothing, nothing, nothing];
    _ ->
      [{Type, LocalPC, _, DataVDT} | Tail] = LTransmission,
      [Tail, Type, LocalPC, <<"V", DataVDT/binary>>]
  end.

create_poll_data_response(SM, Dst) ->
  Res = create_data_queue(SM, Dst),
  Len = get_length_all_msgs(SM, Dst),
  case Len of
    0 ->
      [nothing, nothing, nothing, nothing];
    _ ->
    [{Type, LocalPC, _, DataVDT} | Tail] = Res,
    [Tail, Type, LocalPC, <<"V", DataVDT/binary>>]
  end.

get_length_all_msgs(SM, Dst) ->
  DS = queue_to_data(SM, Dst, sensitive),
  DT = queue_to_data(SM, Dst, tolerant),
  byte_size(DS) + byte_size(DT).

create_data_queue(SM, Dst) ->
  % TODO : we have to implement the limit of the transmission list, parameter
  QNsens = get_share_var(Dst, sensitive),
  QSens = share:get(SM, QNsens),
  QNtol = get_share_var(Dst, tolerant),
  QTol = share:get(SM, QNtol),

  Max_packets_sensitive_transmit = share:get(SM, max_packets_sensitive_transmit),
  Max_packets_tolerant_transmit = share:get(SM, max_packets_tolerant_transmit),

  NL =
  case QTol of
    _ when QTol =/= nothing, QSens =/= nothing ->
      QTolPart = get_part_of_list(Max_packets_tolerant_transmit, QTol),
      QSensPart = get_part_of_list(Max_packets_sensitive_transmit, QSens),
      NQ = queue:join(QSensPart, QTolPart),
      queue:to_list(NQ);
    nothing when QSens =/= nothing ->
      QSensPart = get_part_of_list(Max_packets_sensitive_transmit, QSens),
      queue:to_list(QSensPart);
    _ when QTol =/= nothing, QSens == nothing ->
      QTolPart = get_part_of_list(Max_packets_tolerant_transmit, QTol),
      queue:to_list(QTolPart);
    _ ->
      []
  end,

  lists:foldr(
  fun(X, A) ->
    {_PC, LocalPC, _Status, _Time, Type, _Len, Data} = X,
    [{Type, LocalPC, Dst, Data} | A]
  end, [], NL).

get_part_of_list(Max, Q) ->
  case Max >= queue:len(Q) of
    true -> Q;
    false ->
      {Q1, _Q2} = queue:split(Max, Q),
      Q1
  end.

extract_CDT_msg(SM, Msg) ->
  CPollingFlagBurst = nl_mac_hf:count_flag_bits(1),
  CBitsPosLen = nl_mac_hf:count_flag_bits(63),
  CPollingCounterLen = nl_mac_hf:count_flag_bits(127),

  <<"C", PollingFlagBurst:CPollingFlagBurst, PC:CPollingCounterLen, PLen:CBitsPosLen, RPayload/bitstring>> = Msg,
  <<Pos:PLen/binary, Rest/bitstring>> = RPayload,
  Data_bin = (bit_size(Msg) rem 8) =/= 0,
  CDTData =
  if Data_bin =:= false ->
    Add = bit_size(Rest) rem 8,
    <<_:Add, Data/binary>> = Rest,
    Data;
  true ->
    Rest
  end,

  Recv_packages = share:get(SM, recv_packages),
  ResAdd = add_to_limited_list(SM, Recv_packages, PC, recv_packages, 10),

  case ResAdd of
    nothing  ->
      [nothing, nothing, nothing, nothing, ignore];
    _ when  PollingFlagBurst == 1 ->
      [byte_size(CDTData), CDTData, Pos, nb, answer_pbm];
    _ when  PollingFlagBurst == 0 ->
      [byte_size(CDTData), CDTData, Pos, b, answer_pbm]
  end.

create_CDT_msg(SM, nb, nothing) ->
  Current_polling_addr = get_current_poll_addr(SM),
  PollingCounter = share:get(SM, polling_pc),
  DataPos = <<"POS....">>,
  LenDataPos = byte_size(DataPos),

  PollFlagBurst = 1,

  CPollingFlagBurst = nl_mac_hf:count_flag_bits(1),
  CPollingCounterLen = nl_mac_hf:count_flag_bits(127),
  CBitsPosLen = nl_mac_hf:count_flag_bits(63),

  BPollFlagBurst = <<PollFlagBurst:CPollingFlagBurst>>,
  BPollingCounter = <<PollingCounter:CPollingCounterLen>>,
  BLenPos = <<LenDataPos:CBitsPosLen>>,

  TmpData = <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary>>,
  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  Data =
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary, 0:Add>>;
  true ->
    TmpData
  end,
  increase_local_pc(SM, polling_pc),
  {at, {pid, 0},"*SENDIM", Current_polling_addr, ack, Data};
create_CDT_msg(SM, b, nothing) ->
  % TODO: DATA? Fused Position + CDT ?
  % Burst fomt CDT? or dummy with POS?
  % Set max pos len

  Current_polling_addr = get_current_poll_addr(SM),

  DS = queue_to_data(SM, Current_polling_addr, sensitive),
  DT = queue_to_data(SM, Current_polling_addr, tolerant),

  PollingCounter = share:get(SM, polling_pc),
  DataPos = <<"POS....">>,
  LenDataPos = byte_size(DataPos),
  DataCDT = list_to_binary([DS, DT]),

  CPollingFlagBurst = nl_mac_hf:count_flag_bits(1),
  CPollingCounterLen = nl_mac_hf:count_flag_bits(127),
  CBitsPosLen = nl_mac_hf:count_flag_bits(63),

  PollFlagBurst = 0,

  BPollFlagBurst = <<PollFlagBurst:CPollingFlagBurst>>,
  BPollingCounter = <<PollingCounter:CPollingCounterLen>>,
  BLenPos = <<LenDataPos:CBitsPosLen>>,

  TmpData = <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary, DataCDT/binary>>,

  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  Data =
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary, 0:Add, DataCDT/binary>>;
  true ->
    TmpData
  end,

  LenData = byte_size(Data),
  increase_local_pc(SM, polling_pc),

  if LenData < 64 ->
    {at, {pid, 0},"*SENDIM", Current_polling_addr, ack, Data};
  true ->
    {at, {pid, 0},"*SEND", Current_polling_addr, Data}
  end;
create_CDT_msg(SM, FlagBurst, BroadcastExistMsg) ->
  Current_polling_addr = get_current_poll_addr(SM),
  DataPos = <<"POS....">>,
  LenDataPos = byte_size(DataPos),
  PollingCounter = share:get(SM, polling_pc),

  PollFlagBurst =
  case FlagBurst of
    nb -> 1;
    b -> 0
  end,

  CPollingFlagBurst = nl_mac_hf:count_flag_bits(1),
  CPollingCounterLen = nl_mac_hf:count_flag_bits(127),
  CBitsPosLen = nl_mac_hf:count_flag_bits(63),

  {_Time, broadcast, _Status, _TransmitLen, BroadcastPayload} = BroadcastExistMsg,

  BPollFlagBurst = <<PollFlagBurst:CPollingFlagBurst>>,
  BLenPos = <<LenDataPos:CBitsPosLen>>,
  BPollingCounter = <<PollingCounter:CPollingCounterLen>>,

  TmpData = <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary, BroadcastPayload/binary>>,
  Data_bin = is_binary(TmpData) =:= false or ( (bit_size(TmpData) rem 8) =/= 0),
  Data =
  if Data_bin =:= false ->
    Add = (8 - bit_size(TmpData) rem 8) rem 8,
    <<"C", BPollFlagBurst/bitstring, BPollingCounter/bitstring, BLenPos/bitstring, DataPos/binary, 0:Add, BroadcastPayload/binary>>;
  true ->
    TmpData
  end,

  increase_local_pc(SM, polling_pc),
  {at, {pid, 0},"*SENDIM", Current_polling_addr, ack, Data}.

queue_to_data(SM, Dst, Name) ->
  Qname = get_share_var(Dst, Name),
  Q = share:get(SM, Qname),
  case Q of
    nothing -> <<"">>;
    {[],[]} -> <<"">>;
    _ ->
      LData = queue:to_list(Q),
      F = fun(X) -> {_PC, _LocalPC, _Status, _Time, _Type, _Len, Data} = X, Data end,
      list_to_binary(lists:join(<<"">>, [ F(X)  || X <- LData]))
  end.

add_to_limited_list(SM, L, Key, NameQ, Max) ->
  NL =
  case length(L) > Max of
    true -> lists:delete(lists:nth(length(L), L), L);
    false -> L
  end,

  Member = lists:member(Key, L),
  case Member of
    false ->
      share:put(SM, NameQ, [Key | NL]);
    true  ->
      nothing
  end.

add_to_CDT_queue(SM, {nl, send, broadcast, IDst, Payload}) ->
  Polling_seq_msgs = share:get(SM, polling_seq_msgs),
  add_to_limited_list(SM, Polling_seq_msgs, IDst, polling_seq_msgs, 10),
  Time = erlang:monotonic_time(micro_seconds),
  TransmitLen = byte_size(Payload),
  MsgTuple = {Time, broadcast, not_sent, TransmitLen, Payload},
  MsgName = get_share_var(IDst, broadcast),
  share:put(SM, MsgName, MsgTuple),
  ?TRACE(?ID, "add_to_CDT_queue ~p~n", [share:get(SM, MsgName)]),
  SM;
add_to_CDT_queue(SM, {nl, send, MsgType, IDst, Payload}) ->
  LocalPC = share:get(SM, local_pc),
  increase_local_pc(SM, local_pc),
  TransmitLen = byte_size(Payload),
  Polling_seq_msgs = share:get(SM, polling_seq_msgs),
  add_to_limited_list(SM, Polling_seq_msgs, IDst, polling_seq_msgs, 10),
  Qname = get_share_var(IDst, MsgType),

  QShare = share:get(SM, Qname),
  Q =
  if QShare == nothing ->
    queue:new();
  true ->
    QShare
  end,
  PC = unknown,
  Status = not_delivered,
  Time = erlang:monotonic_time(micro_seconds),
  Item = {PC, LocalPC, Status, Time, MsgType, TransmitLen, Payload},
  NQ =
  case MsgType of
    tolerant ->
      queue:in(Item, Q);
    sensitive ->
      Max_sensitive_queue = share:get(SM, max_sensitive_queue),
      nl_mac_hf:queue_limited_push(Q, Item, Max_sensitive_queue)
  end,
  share:put(SM, Qname, NQ),
  ?TRACE(?ID, "add_to_CDT_queue ~p~n", [share:get(SM, Qname)]),
  SM.

delete_delivered_item(SM, Name, PC, Dst) ->
  Qname = get_share_var(Dst, Name),
  QueueItems = share:get(SM, Qname),
  case QueueItems of
    nothing -> nothing;
    _ ->
      QueueItemsList = queue:to_list(QueueItems),

      NL =
      lists:filtermap(fun(X) ->
      case X of
        {PC, _, _, _, _, _, _} -> false;
        _ -> {true, X}
      end end, QueueItemsList),

      NQ = queue:from_list(NL),
      share:put(SM, Qname, NQ)
  end,
  SM.

change_status_item(SM, PC, {MsgType, LocalPC, ATSendTuple}) ->
  {at, {pid, _}, "*SEND", Dst, Data} = ATSendTuple,
  Max_sensitive_queue = share:get(SM, max_sensitive_queue),
  Qname = get_share_var(Dst, MsgType),

  QueueItems = share:get(SM, Qname),
  QueueItemsList = queue:to_list(QueueItems),

  Payload = extract_poll_data(SM, Data),
  NQ =
  lists:foldr(
  fun(X, Q) ->
    case X of
      {_, LocalPC, Status, Time, MsgType, TransmitLen, Payload} when MsgType == tolerant ->
        Item = {PC, LocalPC, Status, Time, MsgType, TransmitLen, Payload},
        queue:in(Item, Q);
      {_, LocalPC, Status, Time, MsgType, TransmitLen, Payload} when MsgType == sensitive ->
        Item = {PC, LocalPC, Status, Time, MsgType, TransmitLen, Payload},
        nl_mac_hf:queue_limited_push(Q, Item, Max_sensitive_queue);
      Rest when MsgType == tolerant ->
        queue:in(Rest, Q);
      Rest when MsgType == sensitive ->
        nl_mac_hf:queue_limited_push(Q, Rest, Max_sensitive_queue)
    end
  end, queue:new(), QueueItemsList),

  share:put(SM, Qname, NQ),
  SM.
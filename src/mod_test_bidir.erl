-module(mod_test_bidir).
-behaviour(fsm_worker).

-include("fsm.hrl").

-export([start/4, register_fsms/4]).


start(Mod_ID, Role_IDs, Sup_ID, {M, F, A}) ->
    evins:rb(start),
    evins:logon(),
    fsm_worker:start(?MODULE, Mod_ID, Role_IDs, Sup_ID, {M, F, A}).

register_fsms(Mod_ID, Role_IDs, Share, ArgS) ->
    Roles = fsm_worker:role_info(Role_IDs, [at]),
    parse_conf(ArgS, Share, Mod_ID),
    [#sm{roles = [hd(Roles)], module = fsm_conf}
   , #sm{roles = Roles,       module = fsm_test_bidir}].

parse_conf(ArgS, Share, Mod_ID) ->
    [Fname]       = [F || {filename, F} <- ArgS],
    if is_list(Fname) -> ets:insert(Share, [{filename, Fname}]);
    true              -> ?ERROR(Mod_ID, "File name hast to be string! ~n", [])
    end.


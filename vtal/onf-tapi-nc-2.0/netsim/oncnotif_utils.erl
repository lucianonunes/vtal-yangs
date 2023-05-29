-module(oncnotif_utils).

-export([get_elem/4
        , new_maapi/0
        ]).

-include_lib("econfd.hrl").
-include("oncnotif.hrl").


get_elem(M, TH, Path, Default) ->
    case econfd_maapi:exists(M, TH, Path) of
        {ok, true}  ->
            {ok, Value} = econfd_maapi:get_elem(M, TH, Path),
            Value;
        {ok, false} ->
            Default
    end.

new_maapi() ->
    {ok, M}  = econfd_maapi:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_maapi:start_user_session(
           M, <<"">>, <<"system">>, [],
           {127,0,0,1}, ?CONFD_PROTO_TCP),
    M.
-module(wechat_login_handler).

-export([init/3, handle/2, terminate/3]).
-include("../include/wechat.hrl").

init(_, Req, _Opts) ->
  {Reply, Req2} = xfutils:only_allow(get, Req),
  {Reply, Req2, no_state}.


handle(Req, State) ->
  Code_Url= wechat_web:get_code_url(),
  lager:info("~n code_Url = ~p~n", [Code_Url]),
    {ok, Req2} = cowboy_req:reply(302, [
        {<<"content-type">>, <<"text/plain">>},
        {<<"location">>,Code_Url}
    ], "", Req),
    {ok, Req2, State}.

terminate(_Reason, Req, State) ->
    ok.

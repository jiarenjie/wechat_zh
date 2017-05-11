%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 四月 2017 10:03
%%%-------------------------------------------------------------------
-module(wechat_callback_handler).
-author("jiarj").

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
  {Reply, Req2} = xfutils:only_allow(get, Req),
  {Reply, Req2, no_state}.


handle(Req, State) ->
  {GetVal,Req2} = cowboy_req:qs_vals(Req),
  lager:info("~n GetVale = ~p~n", [GetVal]),

  {StatusCode,Req3, ReplyBody} = wechat_callback_process:process(GetVal,Req2),

  {ok, Req4} = cowboy_req:reply(StatusCode, [
    {<<"content-type">>, <<"text/html">>}
  ], ReplyBody, Req3),
  {ok, Req4, State}.

terminate(_Reason, Req, State) ->
  ok.
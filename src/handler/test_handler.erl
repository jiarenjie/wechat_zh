%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 五月 2017 10:28
%%%-------------------------------------------------------------------
-module(test_handler).
-author("jiarj").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, handle/2, terminate/3, test/0]).

init(_, Req, Opts) ->
  {Reply, Req2} = xfutils:only_allow(get, Req),
  {Reply, Req2, Opts}.


handle(Req, State) ->

          {ok, Req2} = cowboy_req:reply(200, [
            {<<"content-type">>, <<"text/plain">>}
          ], <<"test">>, Req),
          {ok, Req2, State}.

terminate(_Reason, Req, State) ->
  ok.
test()->
  ?assertEqual(1 ,1),
  ok.

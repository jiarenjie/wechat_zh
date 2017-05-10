%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 五月 2017 15:26
%%%-------------------------------------------------------------------
-module(gws_web_utils).
-author("jiarj").

%% API
-export([get_app_router/0]).

get_app_router()->
  {ok,Router} = application:get_env(wechat_zh,router),
  Routers=[ apply(M,get_router,[]) || M<-Router],
  lists:flatten(Routers).

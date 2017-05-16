%%%-------------------------------------------------------------------
%% @doc wechat_zh public API
%% @end
%%%-------------------------------------------------------------------

-module(wechat_zh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:start(cowboy),
    application:start(user_admin),
    gws_web:start_link(),
    access_token_utils:init_access_token(),
    wechat_zh_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
port() ->
    {ok, Port} = application:get_env(cowboy, http_port),
    Port.
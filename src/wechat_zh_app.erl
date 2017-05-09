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

    Port = port(),
    TransOpts = [{port, Port}],
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/test", test_handler, []}
        ]}
    ]),
    lager:debug(" ======================================== "),
    cowboy:start_http(http, 100,
        TransOpts,
        [
            {env, [{dispatch, Dispatch}]}
        ]),
    lager:debug(" ========================================= "),


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
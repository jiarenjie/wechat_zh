%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 四月 2017 13:47
%%%-------------------------------------------------------------------
-module(wechat_validate).
-author("jiarj").
-compile(export_all).

%% API
-export([validate_req_fields/3, validate_access_token_resp/1]).

-include_lib("eunit/include/eunit.hrl").

validate_req_fields(From, code, Params) when is_atom(From),  is_list(Params) ->
  try
    [{<<"code">>,Code},{<<"state">>,_State}]=Params,
    validate_req_fields(From,Params),
    {ok, code, Code}
  catch
    _:_ ->
      ErrorMsg = <<"state 参数不存在请重新登录"/utf8>>,
      lager:error("vals error = ~s", [ErrorMsg]),
      {fail, <<"99">>, ErrorMsg}
  end.

validate_req_fields_test()->
  PostVals=[{<<"code">>,<<"dasd">>},{<<"state">>,<<"3213123">>}],
  PostVals2=[{<<"code">>,<<"dasd">>}],
  ?assertEqual({ok, code, <<"dasd">>}, validate_req_fields(mcht, code, PostVals)),
  ?assertEqual({fail, <<"99">>, <<"satae 参数不存在请重新登录"/utf8>>}, validate_req_fields(mcht, code, PostVals2)),
  ok.


validate_req_fields(From,  Params) when is_atom(From), is_list(Params) ->
  F = fun({Key, Value}, {ok, _, _} = _AccIn) when is_binary(Key) ->
    try
      validate_one_field(From, Key, Value),
      {ok, <<>>, <<>>}
    catch
      _:_ ->
        ErrorMsg = <<Key/binary, "=[", Value/binary, "]格式错误,请重新登录"/utf8>>,
        lager:error("vals error = ~s", [ErrorMsg]),
        {fail, <<"99">>, ErrorMsg}
    end;

    (_, {fail, _, _} = AccIn) ->
      %% previous post kv already validate fail, just pass it throuth
      AccIn
      end,

  {OkOrFail, RespCd, RespMsg} = lists:foldl(F, {ok, <<>>, <<>>}, Params),
  {OkOrFail, RespCd, RespMsg}.

validate_access_token_resp(Resp)->
  try
    [{<<"access_token">>,_ACCESS_TOKEN},
      {<<"expires_in">>,_Expires_in},
      {<<"refresh_token">>,_REFRESH_TOKEN},
      {<<"openid">>,_OPENID},
      {<<"scope">>,_SCOPE}]=Resp,
      {ok, resp, Resp}
  catch
    _:_ ->
      ErrorMsg = <<"access_token参数错误请重新登录"/utf8>>,
      lager:error("vals error = ~s", [ErrorMsg]),
      {fail, <<"99">>, ErrorMsg}
  end.

validate_access_token_resp_test()->
  Resp=[{<<"access_token">>,<<"AfrRsRT1GirpBnfxd21IeHwbvlwKUAVHyjPsgAx4sm5OtvCpSDgkXvVlUoUaFdrGnVxdG-brGOFUDgCHkmgUfAbMytvxh8q6jJDcZZDVm04">>},{<<"expires_in">>,7200},{<<"refresh_token">>,<<"dsW0xnN17vHq3h4wSWKth6_MTOlaVNiryioWJZBAC16u2dUm3lb-rT8Kk-n5B5e91WCh5jNyyRXEHhwulVuZeOJs0AuZ5uysmfhX0ZQiKUk">>},{<<"openid">>,<<"ofD9N0wpZmyZrpT5A4FobW055TKY">>},{<<"scope">>,<<"snsapi_userinfo">>}],
  ?assertEqual({ok, resp, Resp}, validate_access_token_resp(Resp)),
  ok.




validate_one_field(mcht, <<"code">>, Value) when is_binary(Value) ->
  ok;
validate_one_field(mcht, <<"state">>, Value) when is_binary(Value) ->
  ok = validate_string(integer, Value);

validate_one_field(up, _, _) ->
  ok.

validate_string(integer, String) when is_list(String) ->
  validate_string(integer, list_to_binary(String));
validate_string(integer, String) when is_binary(String) ->
  try
    binary_to_integer(String),
    ok
  catch
    _:_ ->
      fail
  end.


%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 四月 2017 14:06
%%%-------------------------------------------------------------------
-module(wechat_process_callback).
-include_lib("eunit/include/eunit.hrl").
-author("jiarj").

%% API
-export([validate_req/1, render_fail_resp_model/2]).

-compile(export_all).

validate_req(Vals) when is_list(Vals) ->
  lager:debug("~n Vals ~p~n", [Vals]),
  wechat_validate:validate_req_fields(mcht, code, Vals).

get_access_token(Code)  ->
  try
    Response= wechat_web:get_access_token(Code),
    lager:debug("~n access_token ~p~n", [Response]),
    wechat_validate:validate_access_token_resp(Response)
  catch
    _:X ->
      lager:error("Error ", [X, erlang:get_stacktrace()]),
      {fail, 302, <<"获取access_token失败请重新登录"/utf8>>}
  end.

save_access_token(Resp)  ->
  [{<<"access_token">>,ACCESS_TOKEN},
    {<<"expires_in">>,_Expires_in},
    {<<"refresh_token">>,REFRESH_TOKEN},
    {<<"openid">>,OPENID},
    {<<"scope">>,_SCOPE}]=Resp,
  Userinfo= wechat_web:get_userinfo(ACCESS_TOKEN,OPENID),
  [{<<"openid">>,OPENID},
    {<<"nickname">>,Nickname},
    {<<"sex">>,Sex},
    {<<"language">>,_Language},
    {<<"city">>,_City},
    {<<"province">>,_Province},
    {<<"country">>,_Country},
    {<<"headimgurl">>,Headimgurl},
    {<<"privilege">>,_Privilege}]=Userinfo,
  lager:debug("Userinfo ~p~n", [Userinfo]),
  Timestamp=erlang:system_time(seconds),
  lager:debug("Timestamp ~p~n", [Timestamp]),
  User = gws_mnesia:read_by_pk(repo_user,OPENID),
  lager:debug("~n User ~p~n", [User]),
  case User of
    %%openid,access_token,refresh_token,timestamp,role= <<"empty">>,nickname,headimgurl,sex,info=[]
    [{user_info,OPENID1,ACCESS_TOKEN1,REFRESH_TOKEN1,_Timestamp1,Role1,Nickname,Headimgurl,Sex,Info}]->
      %%保存用户信息到数据库
      Model=[{<<"openid">> , OPENID1}
        ,{<<"access_token">> , ACCESS_TOKEN1}
        ,{<<"refresh_token">> , REFRESH_TOKEN1}
        ,{<<"timestamp">> , Timestamp}
        ,{<<"nickname">> , Nickname}
        ,{<<"headimgurl">> , Headimgurl}
        ,{<<"role">> , Role1}
        ,{<<"info">> , Info}
        ,{<<"sex">> , to_binary(Sex)}],
      gws_mnesia:save(repo_user,Model),
      %%生成jwt
      Payload = [{<<"openid">>,OPENID},{<<"nickname">>,Nickname},{<<"role">>,Role1},{<<"timestamp">>,Timestamp}],
      lager:debug("~n Payload = ~p~n", [Payload]),
      JWT = ejwt:encode(Payload, <<"secret">>),
      lager:debug("~n JWT = ~p~n", [JWT]),
      JWT;
    []->
      %%保存用户信息到数据库
      Model=[{<<"openid">> , OPENID}
        ,{<<"access_token">> , ACCESS_TOKEN}
        ,{<<"refresh_token">> , REFRESH_TOKEN}
        ,{<<"timestamp">> , to_binary(Timestamp)}
        ,{<<"nickname">> , Nickname}
        ,{<<"headimgurl">> , Headimgurl}
        ,{<<"sex">> , to_binary(Sex)}],
      gws_mnesia:save(repo_user,Model),
      %%生成jwt
      Payload = [{<<"openid">>,OPENID},{<<"nickname">>,Nickname},{<<"role">>,<<"empty">>},{<<"timestamp">>,Timestamp}],
      JWT = ejwt:encode(Payload, <<"secret">>),
      lager:debug("~n JWT = ~p~n", [JWT]),
      JWT
  end.



render_fail_resp_model(RespCd, RespMsg) when is_binary(RespCd), is_binary(RespMsg) ->
  ErrorVals = [{error_code, RespCd}, {error_msg, RespMsg}],
  error_req_dtl:render(ErrorVals).

to_binary(Sex) when is_integer(Sex)->
  integer_to_binary(Sex);
to_binary(Sex) when is_binary(Sex)->
  Sex.

decode_test() ->
  Expected = [
    {<<"openid">>,<<"ofD9N0wpZmyZrpT5A4FobW055TKY">>}
    ,{<<"nickname">>,<<233,131,143,228,187,129,230,157,176>>}
    ,{<<"role">>,<<"admin">>}
    ,{<<"timestamp">>,1494552908}
  ],
  JWT = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
  "eyJvcGVuaWQiOiJvZkQ5TjB3cFpteVpycFQ1QTRGb2JXMDU1VEtZIiwibmlja25hbWUiOiLpg4_ku4HmnbAiLCJyb2xlIjoiYWRtaW4iLCJ0aW1lc3RhbXAiOjE0OTQ1NTI5MDh9."
  "YTRu43ctKXlkEZKfroE469YFK0IQF_4bLfmUDtY_8so">>,
  ?assertEqual(Expected, ejwt:decode(JWT, <<"secret">>)).

encode_test() ->
  Expected = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
  "eyJvcGVuaWQiOiJvZkQ5TjB3cFpteVpycFQ1QTRGb2JXMDU1VEtZIiwibmlja25hbWUiOiLpg4_ku4HmnbAiLCJyb2xlIjoiYWRtaW4iLCJ0aW1lc3RhbXAiOjE0OTQ1NTI5MDh9."
  "YTRu43ctKXlkEZKfroE469YFK0IQF_4bLfmUDtY_8so">>,
  Payload = [
    {<<"openid">>,<<"ofD9N0wpZmyZrpT5A4FobW055TKY">>}
    ,{<<"nickname">>,<<233,131,143,228,187,129,230,157,176>>}
    ,{<<"role">>,<<"admin">>}
    ,{<<"timestamp">>,1494552908}
  ],
  ?assertEqual(Expected, ejwt:encode(Payload, <<"secret">>)).


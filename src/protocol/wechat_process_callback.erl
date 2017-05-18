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
    lager:debug("~n Response ~p~n", [Response]),
    wechat_validate:validate_access_token_resp(Response)
  catch
    _:X ->
      lager:error("Error ", [X, erlang:get_stacktrace()]),
      {fail, 302, <<"获取access_token失败请重新登录"/utf8>>}
  end.

save_access_token(Resp)  ->
  ACCESS_TOKEN=proplists:get_value(<<"access_token">>,Resp),
  OPENID=proplists:get_value(<<"openid">>,Resp),
  REFRESH_TOKEN=proplists:get_value(<<"refresh_token">>,Resp),

  Userinfo= wechat_web:get_userinfo(ACCESS_TOKEN,OPENID),
  Nickname=proplists:get_value(<<"nickname">>,Userinfo),
  Headimgurl=proplists:get_value(<<"headimgurl">>,Userinfo),
  Sex=proplists:get_value(<<"sex">>,Userinfo),

  ACCESS_TOKEN=proplists:get_value(<<"access_token">>,Resp),

  lager:debug("Userinfo ~p~n", [Userinfo]),
  Timestamp=erlang:system_time(seconds),
  lager:debug("Timestamp ~p~n", [Timestamp]),
  User = gws_mnesia:read_by_pk(repo_user,OPENID),
  lager:debug("~n User ~p~n", [User]),
  case User of
    %%openid,access_token,refresh_token,timestamp,role= <<"empty">>,nickname,headimgurl,sex,info=[]
    [{user_info,OPENID1,ACCESS_TOKEN1,REFRESH_TOKEN1,_Timestamp1,Role1,Nickname1,Headimgurl1,Sex1,Info1}]->
      %%保存用户信息到数据库
      Model=[{<<"openid">> , OPENID1}
        ,{<<"access_token">> , ACCESS_TOKEN1}
        ,{<<"refresh_token">> , REFRESH_TOKEN1}
        ,{<<"timestamp">> , Timestamp}
        ,{<<"nickname">> , Nickname1}
        ,{<<"headimgurl">> , Headimgurl1}
        ,{<<"role">> , Role1}
        ,{<<"info">> , Info1}
        ,{<<"sex">> , to_binary(Sex1)}],
      gws_mnesia:save(repo_user,Model),
      %%生成jwt
      Payload = [{<<"openid">>,OPENID1},{<<"nickname">>,Nickname},{<<"role">>,Role1},{<<"timestamp">>,Timestamp}],
      lager:debug("~n Payload = ~p~n", [Payload]),
      {ok,JWT} = gws_auth_token:encode(Payload),
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
      {ok,JWT} = gws_auth_token:encode(Payload),
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



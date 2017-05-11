%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 四月 2017 9:12
%%%-------------------------------------------------------------------
-module(wechat_web).
-author("jiarj").
-include("include/wechat.hrl").

%% API
-export([get_access_token/1, get_userinfo/2, refresh_token/1, get_code_url/0]).

get_code_url()->
  {ok,Appid} = application:get_env(wechat_zh,appid),
  {ok,Redirect_uri} = application:get_env(wechat_zh,redirect_uri),
  Enc_url=edoc_lib:escape_uri(Redirect_uri),


  <<A:32,_B:32,_C:32>> = crypto:strong_rand_bytes(12),
  Url = [?API_URL_CODE,"?appid=",Appid,"&redirect_uri=",Enc_url,"&response_type=code&scope=snsapi_userinfo&state=",integer_to_list(A),"#wechat_redirect"],
  Url_f =lists:flatten(Url),
  Url_f_b=list_to_binary(Url_f),
  lager:debug("~n get_code_Url = ~p~n", [Url_f_b]),
  Url_f_b.


get_access_token(Code) ->
  {ok,Appid} = application:get_env(wechat_zh,appid),
  {ok,Secret} = application:get_env(wechat_zh,secret),
  %Access_token_Rui = "https://api.weixin.qq.com/sns/oauth2/access_token?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  Access_token_Rui = ?API_URL_ACCESS_TOKEN++"?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  lager:debug("~n Access_token_Rui = ~p~n", [Access_token_Rui]),
  Response= wechat_util:http_get(Access_token_Rui),
  Response.

refresh_token(REFRESH_TOKEN) ->
  {ok,Appid} = application:get_env(wechat_zh,appid),
  %Access_token_Rui = "https://api.weixin.qq.com/sns/oauth2/access_token?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  REFRESH_TOKEN_Rui = ?API_URL_REFRESH_TOKEN++"?appid="++Appid++"&grant_type=refresh_token&refresh_token="++REFRESH_TOKEN,
  lager:debug("~n REFRESH_TOKEN_Rui = ~p~n", [REFRESH_TOKEN_Rui]),
  Response= wechat_util:http_get(REFRESH_TOKEN_Rui),
  Response.


get_userinfo(ACCESS_TOKEN,OPENID) ->
  %Userinfo_Url = "https://api.weixin.qq.com/sns/userinfo?access_token="++binary_to_list(ACCESS_TOKEN)++"&openid="++binary_to_list(OPENID),
  Userinfo_Url =?API_URL_USERINFO++"?access_token="++binary_to_list(ACCESS_TOKEN)++"&openid="++binary_to_list(OPENID)++"&lang=zh_CN",
  lager:debug("~n Userinfo_Url = ~p~n", [Userinfo_Url]),
  Userinfo= wechat_util:http_get(Userinfo_Url),
  Userinfo.
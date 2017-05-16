
%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 四月 2017 10:03
%%%-------------------------------------------------------------------
-module(wechat_check_handler).
-author("jiarj").
-export([init/3, handle/2, terminate/3]).
-include_lib("xmerl/include/xmerl.hrl").

init(_, Req, _Opts) ->
  {Method, _} = cowboy_req:method(Req),
  {Reply, Req3}=reply_msg(Method,Req),
  {Reply, Req3, no_state}.


handle(Req, State) ->
  {ok, [{PostVals,_}|_rest], Req2} = xfutils:post_get_qs(Req),
  io:format("PostVals: ~p~n", [PostVals]),
  {StatusCode,Req3, ReplyBody}=process(PostVals,Req2),
  {ok, Req4} = cowboy_req:reply(StatusCode, [
    {<<"content-type">>, <<"text/xml">>}
  ], ReplyBody, Req3),
  {ok, Req4, State}.

terminate(_Reason, Req, State) ->
  ok.

reply_msg(<<"GET">>, Req) ->

    {Signature, _Req} = cowboy_req:qs_val(<<"signature">>, Req),
    io:format("Signature:~p~n",[Signature]),
    {Timestamp, _Req} = cowboy_req:qs_val(<<"timestamp">>, Req),
    io:format("Timestamp:~p~n",[Timestamp]),
    {Nonce, _Req} = cowboy_req:qs_val(<<"nonce">>, Req),
    io:format("Nonce:~p~n",[Nonce]),
    {Echostr, _Req} = cowboy_req:qs_val(<<"echostr">>, Req),
    io:format("Echostr:~p~n",[Echostr]),
    {Stute,Result} = check_req(Signature,Timestamp,Nonce,Echostr),
    io:format("result:~p~n",[Result]),

    case Result of
      true  ->
        {ok, Req2} = cowboy_req:reply(Stute, [
          {<<"content-type">>, <<"text/plain">>}
        ], Echostr, Req),
        {ok, Req2};
        false  ->
        {ok, Req2} = cowboy_req:reply(Stute,  Req),
        {ok, Req2}
end;
reply_msg(<<"POST">>, Req) ->
  {ok, Req};
reply_msg(_, Req) ->
  {ok, Req2} = cowboy_req:reply(405, Req),
  {shutdown, Req2}.

process(PostVals, Req) ->
  try
    MsgType=get_MsgType(PostVals),
    lager:debug("MsgType:",[MsgType]),
    {Stute,Body}=do_msg(MsgType,PostVals),
    {Stute,Req,Body}
  catch
      _:_  ->
        {400,Req,<<"shuju yichang xml jiexi shibai">>}
  end.


get_MsgType(PostVals) ->
  {ok,MsgType} = xml_utils:get_xml_text("/xml/MsgType/text()",PostVals),
  MsgType.

do_msg(<<"text">>, PostVals) ->
  {ok,FromUserName} = xml_utils:get_xml_text("/xml/FromUserName/text()",PostVals),
  lager:debug("FromUserName:",FromUserName),
  {ok,ToUserName} = xml_utils:get_xml_text("/xml/ToUserName/text()",PostVals),
  lager:debug("ToUserName:",ToUserName),
  {ok,CreateTime} = xml_utils:get_xml_text("/xml/CreateTime/text()",PostVals),
  lager:debug("CreateTime:",CreateTime),

  MchtOrderVals = [
    {toUserName, FromUserName}
    , {fromUserName, ToUserName}
    , {createTime, CreateTime }
    , {content, <<"hello,我是郏仁杰的公众号"/utf8>>}
  ],
  lager:debug("MchtOrderVals:",MchtOrderVals),
  {ok, Body} = msg_text_dtl:render(MchtOrderVals),
  io:format("PostVals: ~p~n", [Body]),
  {200,Body};
do_msg(_, PostVals)->

  {400,<<"nomuch">>}.


check_req(Signature, Timestamp, Nonce, Echostr) ->
  try
    Result = wechat_signature:check(binary_to_list(Signature), binary_to_list(Timestamp), binary_to_list(Nonce)),
    {200,Result}
    catch
      _:_  ->
        {400,false}

  end.


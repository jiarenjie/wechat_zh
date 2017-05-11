-module(wechat_check_handler).

-export([init/3, handle/2, terminate/3, allowed_methods/2, content_types_provided/2, content_types_accepted/2, delete_completed/2, delete_resource/2,  form_urlencoded_post/2, form_data_post/2, json_post/2, form_xml_post/2, from_get/2]).
-include("../include/wechat.hrl").

init({tcp, http}, Req, Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  Methods = [
    <<"HEAD">>,
    <<"GET">>,
    <<"POST">>,
    <<"PATCH">>,
    <<"DELETE">>,
    <<"OPTIONS">>
  ],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  Handlers = [
    {<<"text/html">>, from_get}
  ],
  {Handlers, Req, State}.

content_types_accepted(Req, State) ->
  Handlers = [
    {<<"application/x-www-form-urlencoded">>, form_urlencoded_post},
    {<<"application/xml">>, form_xml_post},
    {<<"text/xml">>, form_xml_post},
    {<<"multipart/form-data">>, form_data_post},
    {<<"application/json">>, json_post}
  ],
  {Handlers, Req, State}.

delete_completed(Req, State) ->
  io:format("will delete resource~n"),
  {true, Req, State}.

delete_resource(Req, State) ->
  io:format("delete resource finish~n"),
  {true, Req, State}.

from_get(Req, State) ->
  {Signature, _Req} = cowboy_req:qs_val(<<"signature">>, Req),
  io:format("Signature:~p~n",[Signature]),
  {Timestamp, _Req} = cowboy_req:qs_val(<<"timestamp">>, Req),
  io:format("Timestamp:~p~n",[Timestamp]),
  {Nonce, _Req} = cowboy_req:qs_val(<<"nonce">>, Req),
  io:format("Nonce:~p~n",[Nonce]),
  {Echostr, _Req} = cowboy_req:qs_val(<<"echostr">>, Req),
  io:format("Echostr:~p~n",[Echostr]),
  Result = wechat_signature:check(binary_to_list(Signature), binary_to_list(Timestamp), binary_to_list(Nonce)),
  io:format("result:~p~n",[Result]),
  case Result of
    false ->
      {error, "Signature no match"};
    true  ->
      {Echostr, Req, State}
  end.


form_urlencoded_post(Req, State) ->
  {ok, [{PostVals,true}], Req2} = xfutils:post_get_qs(Req),
  io:format("PostVals: ~p~n", [PostVals]),
  {ok,{"xml",_,
    [{"ToUserName",[],[ToUserName]},
      {"FromUserName",[],[FromUserName]},
      {"CreateTime",[],[CreateTime]},
      {"MsgType",[],["text"]},
      {"Content",[],[Content]},
      {"MsgId",[],[MsgId]}]},
    []} = erlsom:simple_form(PostVals),

  MchtOrderVals = [
    {toUserName, list_to_binary(FromUserName)}
    , {fromUserName, list_to_binary(ToUserName)}
    , {createTime, list_to_binary(CreateTime) }
    , {content, <<"hellow,我是郏仁杰的公众号"/utf8>>}

  ],
  {ok, Body} = msg_text_dtl:render(MchtOrderVals),
  io:format("PostVals: ~p~n", [Body]),

  NewReq = cowboy_req:set_resp_body(Body,Req),
  {true, NewReq, State}.

form_xml_post(Req, State) ->
  {ok, [{PostVals,true}], Req2} = xfutils:post_get_qs(Req),
  io:format("PostVals: ~p~n", [PostVals]),
  {ok,{"xml",_,
    [{"ToUserName",[],[ToUserName]},
      {"FromUserName",[],[FromUserName]},
      {"CreateTime",[],[CreateTime]},
      {"MsgType",[],["text"]},
      {"Content",[],[Content]},
      {"MsgId",[],[MsgId]}]},
    []} = erlsom:simple_form(PostVals),

  MchtOrderVals = [
    {toUserName, list_to_binary(FromUserName)}
    , {fromUserName, list_to_binary(ToUserName)}
    , {createTime, list_to_binary(CreateTime) }
    , {content, <<"hellow,我是郏仁杰的公众号"/utf8>>}

  ],
  {ok, Body} = msg_text_dtl:render(MchtOrderVals),
  io:format("PostVals: ~p~n", [Body]),

  NewReq = cowboy_req:set_resp_body(Body,Req),
  {true, NewReq, State}.

form_data_post(Req, State) ->
  {ok, PostVals, _Req2} = cowboy_req:body_qs(Req),
  PostVal = proplists:get_value(<<"aaa">>, PostVals),
  io:format("form_data_post~p~n",[PostVal]),
  NewReq = cowboy_req:set_resp_body(PostVal,Req),
  {true, NewReq, State}.

json_post(Req, State) ->
  {ok, PostVals, _Req2} = cowboy_req:body_qs(Req),
  PostVal = proplists:get_value(<<"aaa">>, PostVals),
  io:format("json_post~p~n",[PostVal]),
  NewReq = cowboy_req:set_resp_body(PostVal,Req),
  {true, NewReq, State}.


handle(Req, State) ->
  {Signature, _Req} = cowboy_req:qs_val(<<"signature">>, Req),
  io:format("Signature:~p~n",[Signature]),
  {Timestamp, _Req} = cowboy_req:qs_val(<<"timestamp">>, Req),
  io:format("Timestamp:~p~n",[Timestamp]),
  {Nonce, _Req} = cowboy_req:qs_val(<<"nonce">>, Req),
  io:format("Nonce:~p~n",[Nonce]),
  {Echostr, _Req} = cowboy_req:qs_val(<<"echostr">>, Req),
  io:format("Echostr:~p~n",[Echostr]),
  Result = wechat_signature:check(binary_to_list(Signature), binary_to_list(Timestamp), binary_to_list(Nonce)),
  io:format("result:~p~n",[Result]),
  case Result of
    false ->
      {error, "Signature no match"};
    true  ->
      {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
      ], Echostr, Req),
      {ok, Req2, State}
  end.


terminate(_Reason, _Req, _State) ->
    ok.

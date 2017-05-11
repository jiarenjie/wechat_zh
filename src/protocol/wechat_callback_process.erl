%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 四月 2017 13:35
%%%-------------------------------------------------------------------
-module(wechat_callback_process).
-author("jiarj").

%% API
-export([process/2]).


process(Vals,Req) ->
  Pipeline = [
    {validate_req, fun fun_validate_req/2}
    , {get_access_token, fun fun_get_access_token/2}
    , {save_access_token,fun fun_save_access_token/2}
  ],

  F = fun
        ({OpName, Handler}, Acc) when is_atom(OpName), is_function(Handler) ->
          lager:debug(" === in ~p,Acc=~p", [OpName, Acc]),
          AccNew = Handler(wechat_process_callback, Acc),
          lager:debug("AccNew=~p", [AccNew]),
          AccNew
      end,

  Resp = try
           JWT = lists:foldl(F, Vals, Pipeline),
           Req2 = cowboy_req:set_resp_header(<<"authorization">>,JWT,Req),
           {200,Req2,<<"jwt">> }

         catch
           throw:{Atom, RespCd, RespMsg}
             ->
             %% fail @ validate/create_req_model
%%             xfutils:post_vals_to_iolist([{resp_code, RespCd}, {resp_msg, RespMsg}]);
             lager:error("txn process error = ~p,RespCode = ~p,RespMsg = ~ts", [Atom, RespCd, RespMsg]),
             {ok, BodyFail} = fun_render_fail_resp_model(wechat_process_callback, RespCd, RespMsg),
             Redirct_Url = <<"/wechat/login">>,
             Req3 = cowboy_req:set_resp_header(<<"location">>,Redirct_Url,Req),
             {302, Req3 ,BodyFail}
         end,
  Resp.


fun_validate_req(M, Vals) when is_atom(M) ->
  case apply(M, validate_req, [Vals]) of
    {ok, code, Code} ->
      %% validate ok, return postvals for next step
      Code
    ;
    {fail, RespCd, RespMsg} ->
      %% infor outer catch
      throw({validate_stop, RespCd, RespMsg})
  end.

fun_get_access_token(M, Code) when is_atom(M) ->
  case apply(M, get_access_token, [Code]) of
    {ok, resp, Resp} ->
      %% validate ok, return postvals for next step
      Resp;
    {fail, RespCd, RespMsg} ->
      %% infor outer catch
      throw({get_access_token_stop, RespCd, RespMsg})
  end.

fun_save_access_token(M, Resp) ->
  try
    apply(M, save_access_token, [Resp])
  catch
    _:X ->
      lager:error("Error ", [X, erlang:get_stacktrace()]),
      throw({get_access_token_stop, <<"99">>, <<"获取保存access_token失败 请重新登录"/utf8>>})
  end.

fun_render_fail_resp_model(M, RespCd, RespMsg) when is_atom(M), is_binary(RespCd), is_binary(RespMsg) ->
  try
    apply(M, render_fail_resp_model, [RespCd, RespMsg])
  catch
    _ :X ->
      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      throw({render_fail_resp_model, <<"99">>, <<"生成应答报文错误"/utf8>>})
  end.







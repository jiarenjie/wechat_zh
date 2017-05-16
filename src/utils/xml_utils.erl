%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 五月 2017 11:29
%%%-------------------------------------------------------------------
-module(xml_utils).
-author("jiarj").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([get_xml_text/2]).

get_xml_text(Roule,Xml) when is_list(Roule) ->
  try
      {XmlElt, _} = xmerl_scan:string(binary_to_list(Xml)),
      [#xmlText{value = Msgtext }] = xmerl_xpath:string(Roule, XmlElt),
      Msgtext2=list_to_binary(Msgtext),
      {ok,Msgtext2}
  catch
      _:_  ->
        {fail,<<"jie xi error">>}
  end.

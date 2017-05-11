-module(wechat_signature).
-include_lib("eunit/include/eunit.hrl").

-export([check/3, jwt_encode/1, jwt_decode/1, jwt_decode/2]).


check(Signature, Timestamp, Nonce) ->
    Token = "wechat",
    TmpList = [Token, Timestamp, Nonce],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    Hash = wechat_util:hexstring(TmpStr),
    io:format("Hash:~p~n",[Hash]),
    string:equal(string:to_lower(Signature), string:to_lower(Hash)).

check_test()->

    Check = check("95a0d735a1b79c3f23054f2dd1068884f654ea4a","1492407520","1056732191"),
    ?assertEqual(true,Check),
    ok.

jwt_encode(Payload)->
    jwt_encode(Payload,<<"secret">>).


jwt_encode(Payload, Secret) when is_binary(Secret) ->
    JWT = ejwt:encode(Payload, Secret),
    JWT.

jwt_decode(JWT) when is_binary(JWT) ->
    Payload = ejwt:decode(JWT),
    Payload.

jwt_decode(JWT,Secret) when is_binary(Secret),is_binary(JWT)->
    Payload = ejwt:decode(JWT,Secret),
    Payload.



decode_dont_check_signature_test() ->
    Expected = [{<<"sub">>,<<"1234567890">>},{<<"name">>,<<"John Doe">>},{<<"admin">>,true}],
    JWT = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
    "eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9."
    "TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ">>,
    ?assertEqual(Expected, jwt_decode(JWT)).

decode_test() ->
    Expected = [{<<"sub">>,<<"1234567890">>},{<<"name">>,<<"John Doe">>},{<<"admin">>,true}],
    JWT = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
    "eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9."
    "TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ">>,
    ?assertEqual(Expected, jwt_decode(JWT, <<"secret">>)).

encode_test() ->
    Expected = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9."
    "eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9."
    "TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ">>,
    Payload = [{<<"sub">>,<<"1234567890">>},{<<"name">>,<<"John Doe">>},{<<"admin">>,true}],
    ?assertEqual(Expected, jwt_encode(Payload, <<"secret">>)).




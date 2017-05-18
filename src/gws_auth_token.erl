%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2016 5:26 PM
%%%-------------------------------------------------------------------
-module(gws_auth_token).
-author("simon").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([encode/1
  , decode/1
  , is_token_valid/1
  , decode_auth_header/1
  , login/2
  , reencode/1
]).

-define(SERVER, ?MODULE).
-define(BH, behaviour_repo).

-record(state, {secrets, expire_time, alg}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
-spec encode(ClaimSet) -> Result when
  ClaimSet :: list() | map(),
  Result :: {ok, binary()} | {error, atom()}.

encode(ClaimSet) when is_list(ClaimSet) or is_map(ClaimSet) ->
  gen_server:call(?SERVER, {encode, ClaimSet}).
%%--------------------------------------------------------------------
-spec decode(Token) -> Result when
  Token :: binary(),
  Result :: {error, atom()} | {ok, map()}.

decode(Token) when is_binary(Token) ->
  gen_server:call(?SERVER, {decode, Token}).

decode_auth_header(Line) when is_binary(Line) ->
  try
    [<<"Bearer">>, Token] = binary:split(Line, <<" ">>, [trim_all, global]),
    decode(Token)
  catch
    _:_ ->
      lager:debug("Auth header wrong format:~ts", [Line]),
      {error, bad_auth_header}
  end.

is_token_valid(Token) when is_binary(Token) ->
  case decode(Token) of
    {ok, _} ->
      true;
    {error, _} ->
      false
  end.

%%--------------------------------------------------------------------
reencode(OldToken) when is_binary(OldToken) ->
  gen_server:call(?SERVER, {reencode, OldToken}).
%%--------------------------------------------------------------------
login(UserName, Password) when is_binary(UserName), is_binary(Password) ->
  gen_server:call(?SERVER, {login, {UserName, Password}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok,Screts} = application:get_env(jwt,secrets),
  lager:debug("Screts = ~p", [Screts]),
  {ok,Expire} = application:get_env(jwt,expire_time),
  lager:debug("Expire = ~p", [Expire]),
  {ok,Alg} = application:get_env(jwt,alg),
  lager:debug("Alg = ~p", [Alg]),
  {ok, #state{secrets = Screts, expire_time = Expire, alg = Alg}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({encode, Claims}, _From, State) ->
  Ret = do_encode(Claims, State),
  lager:debug("Token encode: Claim = ~p,Token = ~p", [Claims, Ret]),
  {reply, Ret, State};
handle_call({decode, Token}, _From, State) ->
  Ret = do_decode(Token, State),
  lager:debug("Token decode: Token = ~p,Result = ~p", [Token, Ret]),
  RetNew = check_expire(Ret),
  {reply, RetNew, State};
handle_call({login, {UserName, Password}}, _From, State) ->
  CheckLoginResult = check_login(UserName, Password),
  lager:debug("check login result = ~p", [CheckLoginResult]),
  ReturnLoginResult = process_login_result(CheckLoginResult, State),
  lager:debug("login result = ~p", [ReturnLoginResult]),
  {reply, ReturnLoginResult, State};
handle_call({reencode, OldToken}, _From, State) ->
  {_, NewToken} = do_reencode(OldToken, State),
  lager:debug("Token reencode: OldToken = ~p,NewToken = ~p", [OldToken, NewToken]),
  {reply, NewToken, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_expire({ok, #{exp:= ExpireTS} = Claims}) ->
  LocalTS = erlang:system_time(seconds),
  case LocalTS > ExpireTS of
    true ->
      %% expired
      {error, expired};
    _ ->
      Claims
  end;

check_expire(Error) ->
  Error.


%%--------------------------------------------------------------------
-spec check_login(UserName, Pwd) -> Result when
  UserName :: binary(),
  Pwd :: binary(),
  Result :: no_such_user | password_not_match | ok.

check_login(UserName, Password) when is_binary(UserName), is_binary(Password) ->
  BH = behaviour_repo,
  M = repo_backend_users_pt,
  {ok, UserRepo} = BH:fetch_index(M, {name, UserName}),
  check_username_match(UserRepo, Password, M).

check_username_match([], _, _) ->
  %% no match user name
  no_such_user;
check_username_match([UserRepo], Password, Module) when is_atom(Module) ->
  %% user name exist ,check pwd match
  PwdInRepo = recop_util:get(Module, UserRepo, password),
  PwdMatch = (PwdInRepo =:= Password),
  check_pwd_match(PwdMatch, UserRepo).

check_pwd_match(true, UserRepo) ->
  %% login ok
  {ok, UserRepo};
check_pwd_match(false, _) ->
  %% pwd not match
  password_not_match.
%%--------------------------------------------------------------------
process_login_result(no_such_user, _) ->
  #{resp_cd => fail, resp_msg => <<"用户不存在"/utf8>>};
process_login_result(password_not_match, _) ->
  #{resp_cd => fail, resp_msg => <<"密码不正确"/utf8>>};
process_login_result({ok, UserRepo}, State) ->
  M = repo_backend_users_pt,
  ID = utils_recop:get(M, UserRepo, id),

  Token = new_token(UserRepo, State),

  #{resp_cd => success, resp_msg => <<"登录成功"/utf8>>
    , id => ID, token => Token
  }.
%%--------------------------------------------------------------------
do_encode(Claims, #state{secrets = Key, expire_time = Expire, alg = Alg} = _State) ->
  lager:debug("Claim = ~p", [Claims]),
  Result = jwt:encode(Alg, Claims, Expire, Key),
  lager:debug("Result = ~p", [Result]),
  Result.
do_decode(Token, #state{secrets = Key} = _State) ->
  lager:debug("Token = ~p", [Token]),
  Result = jwt:decode(Token, Key),
  lager:debug("Result = ~p", [Result]),
  Result.
do_reencode(OldToken, #state{secrets = Key} = State) ->
  lager:debug("OldToken = ~p", [OldToken]),
  Return = case jwt:decode(OldToken, Key) of
             {ok, ClaimMap} ->
               ClaimList = maps:to_list(ClaimMap),
               NewToken = do_encode(ClaimList, State),
               {ok, NewToken};
             {error, ErrorCode} ->
               {error, ErrorCode}
           end,
  lager:debug("Reencode result = ~p", [Return]),
  Return.
%%--------------------------------------------------------------------
new_token(UserRepo, State) ->
  M = repo_backend_users_pt,
  ID = recop_util:get(M, UserRepo, id),
  UserName = recop_util:get(M, UserRepo, name),
  Claims = [{user_id, ID}, {user_name, UserName}],
  {ok, Token} = do_encode(Claims, State),
  Token.


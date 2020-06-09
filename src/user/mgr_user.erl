%% ------------------------------------------------------------------
%% 用户节点管理进程
%% ------------------------------------------------------------------
-module(mgr_user).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_class.hrl").
-include("cfg_news_source.hrl").
-define(db_sec, 600).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-record(mgr_user, {
	changes = []           %% 
	,session_change = []  %% 
}).
 
-export([start_link/0, call/1, cast/1, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

call(Msg) ->
	gen_server:call(?MODULE, Msg).

cast(Msg) ->
		gen_server:cast(?MODULE, Msg).

send(Msg) ->
	?MODULE ! Msg.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	erlang:process_flag(trap_exit, true),
	?INFO("start mgr_user",[]),
	ets:new(?ETS_TODAYHOT_USER, [named_table, {keypos, #todayhot_user.account}]),
	ets:new(?ETS_TODAYHOT_USER_SESSION, [named_table, {keypos, #todayhot_user_session.session}]),
    {UserL, UserSessionL} = dao_user:load(),
	ets:insert(?ETS_TODAYHOT_USER, UserL),
	ets:insert(?ETS_TODAYHOT_USER_SESSION, UserSessionL),
	erlang:send_after(?db_sec*1000, self(), up_db),
	?INFO("finish mgr_user",[]),
    {ok, #mgr_user{}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, NState} ->
			{reply, Reply, NState};
		Reason ->
			?ERR("mgr_user Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_user Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_user Request ~w Reason ~w",[Info, Reason]),
			{noreply, State}
	end.

terminate(_Reason, #mgr_user{changes = Changes, session_change=SessionChanges}) ->
	dao_user:up_user_db(Changes),
	dao_user:up_session_db(SessionChanges),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
do_handle_call({add_account, Account, Password}, _From, State=#mgr_user{session_change=SessionChanges}) ->
	Md5Pwd = util:md5(Password),
	TUser =  #todayhot_user{account=Account, password=Md5Pwd},
	ok = dao_user:insert_user(TUser),
	ets:insert(?ETS_TODAYHOT_USER, TUser),
	Time = util:now()+?d_s(30),
	SessionID = base64:encode(crypto:strong_rand_bytes(32)),
	insert_session(Account, SessionID, Time),
	NSessionChanges = [#todayhot_user_session{account=Account, session=SessionID, time = Time}|SessionChanges],
    {reply, {ok, SessionID, ?d_s(30)}, State#mgr_user{session_change=NSessionChanges}};
do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
	{noreply, State}.

do_handle_info(up_db, State=#mgr_user{changes = Changes, session_change=SessionChanges}) ->
	dao_user:up_user_db(Changes),
	dao_user:up_session_db(SessionChanges),
	erlang:send_after(?db_sec*1000, self(), up_db),
	{noreply, State#mgr_user{changes=[], session_change=[]}};
do_handle_info({up_session, Account, SessionID, Time}, State=#mgr_user{session_change=SessionChanges}) ->
	insert_session(Account, SessionID, Time),
	NSessionChanges = [#todayhot_user_session{account=Account, session=SessionID, time = Time}|lists:keydelete(Account, #todayhot_user_session.account, SessionChanges)],
	{noreply, State#mgr_user{session_change=NSessionChanges}};
do_handle_info({add_rss, Account, SourceId, IsCancel}, State=#mgr_user{changes = Changes}) ->
	TUser = #todayhot_user{source_list = SourceList} = get_user(Account),
	NTUser = case IsCancel of
		?true ->
			TUser#todayhot_user{source_list = lists:keydelete(SourceId, #todayhot_user_source.id, SourceList)};
		_ ->
			TUser#todayhot_user{source_list = [#todayhot_user_source{id=SourceId, time=util:now()}|lists:keydelete(SourceId, #todayhot_user_source.id, SourceList)]}
	end,
	ets:insert(?ETS_TODAYHOT_USER, NTUser),
	NChanges = [NTUser|lists:keydelete(SourceId, #todayhot_user_source.id, Changes)],
	{noreply, State#mgr_user{changes=NChanges}};
do_handle_info(_Msg, State) ->
	{noreply, State}.

get_user(Account) ->
	[TUser = #todayhot_user{}] = ets:lookup(?ETS_TODAYHOT_USER, Account),
	TUser.

insert_session(Account, SessionID, Time) ->
	ets:insert(?ETS_TODAYHOT_USER_SESSION, #todayhot_user_session{account = Account, session = SessionID, time=Time}),
	ok.
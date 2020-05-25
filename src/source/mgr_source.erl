%% ------------------------------------------------------------------
%% 资讯节点管理进程
%% ------------------------------------------------------------------
-module(mgr_source).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 10*60).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_class.hrl").
-include("cfg_news_source.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-define(source_node_num, 50). %% 每100个源一个节点
-record(mgr_source, {
	list = []        %% 系统源列表[{SPid, [SourceId]}]
	,user_list = []  %% 用户自定义源列表[{SPid, [SourceId]}]
	,time = 0
	,total_list = []
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
	?INFO("start mgr_source",[]),
	State = alloc(#mgr_source{}),
	erlang:send_after(3600*1000, self(), alloc),
	?INFO("finish mgr_source",[]),
    {ok, State}.

alloc(State = #mgr_source{total_list = OldList, list = CfgList}) ->
	Fun = fun(SourceId, {Acc1, Acc2}) ->
		case lists:member(SourceId, Acc2) of
			true -> {Acc1, Acc2};
			_ ->
				{alloc(SourceId, Acc1), [SourceId|Acc2]}
		end
	end,
	{NCfgList, NTotalList} = lists:foldl(Fun, {CfgList, OldList}, cfg_news_source:list_key()),
	State#mgr_source{total_list = NTotalList, list = NCfgList}.

alloc(SourceId, []) ->
	case source:start({self(), SourceId, ?false}) of
		{ok, Pid} ->
			[{Pid, [SourceId]}];
		_ -> []
	end;
alloc(SourceId, [{Pid, SL}|List]) ->
	case length(SL) < ?source_node_num of
		true ->
			Pid ! {add_source, SourceId, ?false},
			[{Pid, SL++[SourceId]}|List];
		_ ->
			alloc(SourceId, List)
	end.


handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_tmt Request ~w Reason ~w",[Info, Reason]),
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_handle_call(_Request, _From, State) ->
    {reply, ok, State}.

do_handle_cast(_Msg, State)->
	{noreply, State}.

do_handle_info(alloc, State) ->
	NState = alloc(State),
	erlang:send_after(3600*1000, self(), alloc),
	{noreply, NState};	
do_handle_info(_Msg, State) ->
	{noreply, State}.


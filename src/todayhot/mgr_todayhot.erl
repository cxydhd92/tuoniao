%% ------------------------------------------------------------------
%% todayhot管理进程
%% ------------------------------------------------------------------
-module(mgr_todayhot).
-behaviour(gen_server).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-define(SERVER, ?MODULE).

-define(db_sec, 30*60).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, call/1, cast/1, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(mgr_todayhot, {
    aid = 0
	,changes = []
	,up_nodes = [] %% [#todayhot_nodes{}]
}).
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
	?INFO("start mgr_todayhot",[]),
    erlang:process_flag(trap_exit, true),
	ets:new(?ETS_TODAYHOT, [named_table, {keypos, #todayhot_nodes.class_node}]),
	ets:new(?ETS_TODAYHOT_TODAY, [named_table, {keypos, #todayhot_nodes.class_node}]),
	ets:new(?ETS_TODAYHOT_HOTLIST, [named_table, {keypos, #todayhot_nodes_hotlist.class_node}, public]),
    {Data,TodayData} = dao_todayhot:load(),
    MaxId = dao_todayhot:load_sys_id(),
	ets:insert(?ETS_TODAYHOT, Data),
	ets:insert(?ETS_TODAYHOT_TODAY, TodayData),
	erlang:send_after(?db_sec*1000, self(), up_db),
	Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
	?INFO("finish mgr_todayhot",[]),
    {ok, #mgr_todayhot{aid = MaxId}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, NState} ->
			{reply, Reply, NState};
		Reason ->
			?ERR("mgr_todayhot Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_todayhot Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_todayhot Request ~w Reason ~w",[Info, Reason]),
			{noreply, State}
	end.

terminate(_Reason, #mgr_todayhot{changes = Changes, aid = Aid, up_nodes=UpNodes}) ->
	dao_todayhot:up_news_db(Changes, Aid),
	dao_todayhot:up_db_nodes(UpNodes),
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

do_handle_info(zero_up, State=#mgr_todayhot{}) ->
	api_todayhot:up_today_to_other(),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State};
do_handle_info(up_db, State=#mgr_todayhot{changes = Changes, aid=AId, up_nodes=UpNodes}) ->
	dao_todayhot:up_news_db(Changes, AId),
	dao_todayhot:up_db_nodes(UpNodes),
	erlang:send_after(?db_sec*1000, self(), up_db),
	{noreply, State#mgr_todayhot{changes=[], up_nodes=[]}};
do_handle_info({up_news, Class, NodeId, AddNews, Time}, State=#mgr_todayhot{changes = Changes, aid = AId, up_nodes=UpNodes}) ->
		?INFO("up_news len ~w Class~w NodeId~w aid~w", [length(AddNews), Class, NodeId, AId]),
	NodeIdL = lists:delete(NodeId, cfg_news_source:news_source_class(Class)),
	{NNode, NAId1, NChangesL} = case api_todayhot:get_today_node(Class, NodeId) of
		Node = #todayhot_nodes{news = NewsL} ->
			{NNewsL, NAId, NAddNews} = add_news(NewsL, Class, NodeIdL, AddNews, AId ,Changes),
			{Node#todayhot_nodes{news = NNewsL, up_time = Time}, NAId, NAddNews};
		_ ->
			{NNewsL, NAId, NAddNews} = add_news([], Class, NodeIdL, AddNews, AId,Changes),
			Node = #todayhot_nodes{class_node={Class, NodeId}, news = NNewsL, add_time=util:now(), up_time = Time},
			dao_todayhot:up_db_nodes([Node]),
			{Node, NAId, NAddNews}
    end,		
	api_todayhot:insert_today(NNode),
	{noreply, State#mgr_todayhot{aid = NAId1, changes=NChangesL, up_nodes=[NNode#todayhot_nodes{news=[]}|lists:keydelete({Class, NodeId}, #todayhot_nodes.class_node, UpNodes)]}}.


add_news(NewsL, Class, NodeIdL, AddNews, AId,Changes) ->
		Fun = fun(News, {Acc, MaxId, NewsAcc}) ->
				News1 = News#todayhot_news{id=MaxId+1},
				{News2, TN} = is_same_title(News1, Class, NodeIdL),
				NAddNewsL = ?IF(TN==false, [News2|NewsAcc], [News2, TN|lists:keydelete(TN#todayhot_news.id, #todayhot_news.id, NewsAcc)]),
				{[News2|Acc], MaxId+1, NAddNewsL}
		end,
		{NNewsL, NAId, NAddNews} = lists:foldl(Fun, {NewsL, AId, Changes}, AddNews),
		{NNewsL, NAId, NAddNews}.

%% 简单的匹配标题相似度
is_same_title(News, _, []) -> {News, false};
is_same_title(News, Class, [NodeId|T]) ->
	case api_todayhot:get_today_node(Class, NodeId) of
		Node = #todayhot_nodes{news = NewsL} ->
			case is_same_title(News, NewsL) of
				false -> is_same_title(News, Class, T);
				{AId, TN} -> ?INFO("same_title AId~w",[AId]),
					NNewsL = [TN|lists:keydelete(AId, #todayhot_news.id, NewsL)],
					api_todayhot:insert_today(Node#todayhot_nodes{news=NNewsL}),
					{News#todayhot_news{same_id=AId}, TN}
			end;
		_ ->
			is_same_title(News, Class, T)
	end.	
is_same_title(_News, []) -> false;
is_same_title(News=#todayhot_news{title=Title, id=Id}, [TN=#todayhot_news{id=AId, same_id = 0, title=CTitle, sub_news=SIds}|NewsL]) ->
	case is_same_title_f1(Title, CTitle) of
		true -> 
			?INFO("same_title Title ~ts CTitle~ts",[Title, CTitle]),
			{AId, TN#todayhot_news{sub_news=SIds++[Id]}};
		_ -> is_same_title(News, NewsL)
	end;
is_same_title(News, [_|NewsL]) -> 
	is_same_title(News, NewsL).

%% 70%相同列为相似标题
is_same_title_f1(Title, CTitle) ->
	UTitle = unicode:characters_to_list(Title),
	UCTitle = unicode:characters_to_list(CTitle),
	% ?INFO("UTitle ~w UCTitle~w", [UTitle, UCTitle]),
	SameNum = is_same_title_f2(UTitle, UCTitle, 0),
	LenU = length(UTitle),
	LenUC = length(UCTitle),
	Len = ?IF(LenU>LenUC, LenU, LenUC),
	SameP = trunc(SameNum / Len * 100),
	SameP >= 70.

is_same_title_f2([], _UCTitle, SameNum) ->
	SameNum;
is_same_title_f2([U|T], UCTitle, SameNum) ->
	case lists:member(U, UCTitle) of
		true -> is_same_title_f2(T, lists:delete(U, UCTitle), SameNum+1);
		_ -> is_same_title_f2(T, UCTitle, SameNum)
	end.





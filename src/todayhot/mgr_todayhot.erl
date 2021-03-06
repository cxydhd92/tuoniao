%% ------------------------------------------------------------------
%% todayhot管理进程
%% ------------------------------------------------------------------
-module(mgr_todayhot).
-behaviour(gen_server).

-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-include("cfg_news_class.hrl").

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
	ets:new(?ETS_TODAYHOT, [named_table, {keypos, #todayhot_nodes.node_id}]),
	ets:new(?ETS_TODAYHOT_NEWS, [named_table, {keypos, #todayhot_node_news.node}]),
	ets:new(?ETS_TODAYHOT_TODAY, [named_table, {keypos, #todayhot_node_news.node}]),
	ets:new(?ETS_TODAYHOT_HOTLIST, [named_table, {keypos, #todayhot_node_news.node}, public]),
	ets:new(?ETS_CFG_NODE, [named_table, {keypos, #cfg_news_source.source_id}, public]),
	ets:new(?ETS_CFG_CLASS, [named_table, {keypos, #cfg_news_class.id}, public]),
	ets:insert(?ETS_CFG_CLASS, dao_todayhot:load_cfg_class()),
	ets:insert(?ETS_CFG_NODE, dao_todayhot:load_cfg_node()),
    Nodes = dao_todayhot:load1(),
    erlang:garbage_collect(),
    {Data,TodayData} = dao_todayhot:load2(),
    erlang:garbage_collect(),
    HotList = dao_todayhot:load3(),
    erlang:garbage_collect(),
    MaxId = dao_todayhot:load_sys_id(),
	ets:insert(?ETS_TODAYHOT, Nodes),
	ets:insert(?ETS_TODAYHOT_NEWS, Data),
	ets:insert(?ETS_TODAYHOT_TODAY, TodayData),
	ets:insert(?ETS_TODAYHOT_HOTLIST, HotList),
	erlang:send_after(?db_sec*1000, self(), up_db),
	Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
	?INFO("finish mgr_todayhot",[]),
	gc_timer(),
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
	dao_todayhot:up_hotlist_db(),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State};
do_handle_info(up_db, State=#mgr_todayhot{changes = Changes, aid=AId, up_nodes=UpNodes}) ->
	dao_todayhot:up_news_db(Changes, AId),
	dao_todayhot:up_db_nodes(UpNodes),
	erlang:send_after(?db_sec*1000, self(), up_db),
	{noreply, State#mgr_todayhot{changes=[], up_nodes=[]}};
do_handle_info({up_news, Class, NodeId, AddNews, Time}, State=#mgr_todayhot{changes = Changes, aid = AId}) ->
		?INFO("up_news len ~w Class~w NodeId~w aid~w", [length(AddNews), Class, NodeId, AId]),
	NodeIdL = lists:delete(NodeId, cfg_news_source:news_source_class(Class)),
	Today = util:today(),
	{NNode, NAId1, NChangesL} = case api_todayhot:get_today_node_false(NodeId, Today) of
		Node = #todayhot_node_news{news = NewsL} ->
			{NNewsL, NAId, NAddNews} = add_news(NewsL, Today, NodeIdL, AddNews, AId ,Changes),
			{Node#todayhot_node_news{news = NNewsL, up_time = Time}, NAId, NAddNews};
		_ ->
			{NNewsL, NAId, NAddNews} = add_news([], Today, NodeIdL, AddNews, AId,Changes),
			NodeNews = #todayhot_node_news{node={NodeId, Today}, news = NNewsL, up_time = Time},
			Node = #todayhot_nodes{node_id=NodeId, add_time=util:now()},
			dao_todayhot:up_db_nodes([Node]),
			{NodeNews, NAId, NAddNews}
    end,		
	api_todayhot:insert_today(NNode),
	erlang:garbage_collect(),
	{noreply, State#mgr_todayhot{aid = NAId1, changes=NChangesL}};
do_handle_info({update_cfg_class, Cfg}, State=#mgr_todayhot{}) -> ?INFO("ccccccccccccccccccc"),
	ets:insert(?ETS_CFG_CLASS, Cfg),
	dao_todayhot:cfg_class_update(Cfg),
	{noreply, State};
do_handle_info({del_cfg_class, CfgId}, State=#mgr_todayhot{}) ->
	ets:delete(?ETS_CFG_CLASS, CfgId),
	dao_todayhot:cfg_class_del(CfgId),
	{noreply, State};
do_handle_info({update_cfg_node, Cfg}, State=#mgr_todayhot{}) ->
	?INFO("xxxxxxxxxxxCfg~w",[Cfg]),
	ets:insert(?ETS_CFG_NODE, Cfg),
	dao_todayhot:cfg_node_update(Cfg),
	{noreply, State};
do_handle_info({del_cfg_node, CfgId}, State=#mgr_todayhot{}) ->
	ets:delete(?ETS_CFG_NODE, CfgId),
	dao_todayhot:cfg_node_del(CfgId),
	{noreply, State};
do_handle_info(gc_loop_timer, State=#mgr_todayhot{}) ->
	erlang:garbage_collect(),
	?INFO("gc_loop_timer",[]),
	gc_timer(),
	{noreply, State};	
do_handle_info(_Msg, State) ->
	?INFO("xxxxxxxxxxxCfg~w",[_Msg]),
	{noreply, State}.

gc_timer() ->
	erlang:send_after(900*1000, self(), gc_loop_timer),
	ok.

add_news(NewsL, Today, NodeIdL, AddNews, AId,Changes) ->
		Fun = fun(News=#todayhot_news{abstract = _Abstract}, {Acc, MaxId, NewsAcc}) ->
				News1 = News#todayhot_news{id=MaxId+1},
				{News2, TN} = is_same_title(News1, Today, NodeIdL),
				NAddNewsL = ?IF(TN==false, [News2|NewsAcc], [News2, TN|lists:keydelete(TN#todayhot_news.id, #todayhot_news.id, NewsAcc)]),
				{[News2|Acc], MaxId+1, NAddNewsL}
		end,
		{NNewsL, NAId, NAddNews} = lists:foldl(Fun, {NewsL, AId, Changes}, AddNews),
		{NNewsL, NAId, NAddNews}.

%% 简单的匹配标题相似度
is_same_title(News, _, []) -> {News, false};
is_same_title(News, Today, [NodeId|T]) ->
	case api_todayhot:get_today_node_false(NodeId, Today) of
		Node = #todayhot_node_news{news = NewsL} ->
			case is_same_title(News, NewsL) of
				false -> is_same_title(News, Today, T);
				{AId, TN} -> ?INFO("same_title AId~w",[AId]),
					NNewsL = [TN|lists:keydelete(AId, #todayhot_news.id, NewsL)],
					api_todayhot:insert_today(Node#todayhot_node_news{news=NNewsL}),
					{News#todayhot_news{same_id=AId}, TN}
			end;
		_ ->
			is_same_title(News, Today, T)
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
	case lists:member(U, UCTitle) andalso  not lists:member(U, ?REMOVE_CHAR) of
		true -> is_same_title_f2(T, lists:delete(U, UCTitle), SameNum+1);
		_ -> is_same_title_f2(T, UCTitle, SameNum)
	end.





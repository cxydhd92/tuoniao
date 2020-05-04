%% ------------------------------------------------------------------
%% 好奇心日报站点管理进程
%% ------------------------------------------------------------------
-module(mgr_toutiao).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(spider_sec, 56*60).

-include("common.hrl").
-include("todayhot.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(mgr_toutiao, {
	list = []
	,time = 0
}).
-define(URL, "https://api3-normal-c-lq.snssdk.com/api/feed/hotboard_online/v1/?category=hotboard_online&count=50&extra=%7B%22CardStyle%22%3A0%2C%22JumpToWebList%22%3Atrue%2C%22IsHotTabChannel%22%3Atrue%7D&style_type=18&client_extra_params=%7B%22hot_board_source%22%3A%22hot_board%22%2C%22style_id%22%3A%2210006%22%7D&iid=1591722989326911&device_id=62801537157&ac=wifi&mac_address=A4%3A50%3A46%3A36%3A5E%3A67&channel=xiaomi&aid=13&app_name=news_article&version_code=770&version_name=7.7.0&device_platform=android&ab_version=1587644%2C1587702%2C662176%2C1651977%2C1656556%2C1419034%2C668775%2C1640910%2C1529252%2C1655239%2C1190524%2C1157750%2C1413880%2C1419598%2C1629530%2C1469498%2C1484964%2C1576657%2C1593455%2C1637863%2C668779%2C1417599%2C662099%2C1640175%2C1610260%2C668774%2C1658111%2C1633732%2C660830%2C1660586%2C1647326%2C1639416&ab_feature=94563%2C102749&ssmix=a&device_type=MIX+3&device_brand=Xiaomi&language=zh&os_api=29&os_version=10&openudid=91435e9c985ccb59&manifest_version_code=7700&resolution=1080*2210&dpi=440&update_version_code=77011&_rticket=1587872944750&plugin=18762&pos=5r_-9Onkv6e_eCQieCoDeCUfv7G_8fLz-vTp6Pn4v6esrK6zq62krqupsb_x_On06ej5-L-nr66zrKWorK-psb_88Pzt3vTp5L-nv3gkIngqA3glH7-xv_zw_O3R8vP69Ono-fi_p6ysrrOrraSuq6mxv_zw_O3R_On06ej5-L-nr66zrKWorK-p4A%3D%3D&host_abi=armeabi-v7a&tma_jssdk_version=1.61.0.7&rom_version=miui_v11_v11.0.2.0.qeecnxm&cdid=7c874361-527b-45a8-a5d8-2b505f140491&oaid=85de4e63985d056f").
-define(HOST, "https://www.toutiao.com/a").
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
	?INFO("start mgr_toutiao",[]),
	erlang:send_after(28*1000, self(), start_spider),
	?INFO("finish mgr_toutiao",[]),
    List = api_todayhot:get_node_news(?todayhot_class_compre, ?todayhot_node_toutiao),
	% ?INFO("Time ~w",[Time]),
	Today = util:today(),
	Sec = Today+86400 - util:now(),
	erlang:send_after(Sec*1000, self(), zero_up),
    {ok, #mgr_toutiao{list = List}}.

handle_call(Request, From, State) ->
	case catch do_handle_call(Request, From, State) of
		{reply, Reply, State} ->
			{reply, Reply, State};
		Reason ->
			?ERR("mgr_toutiao Request ~w Reason ~w",[Request, Reason]),
			{reply, error, State}
	end.

handle_cast(Msg, State) ->
	case catch do_handle_cast(Msg, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_toutiao Request ~w Reason ~w",[Msg, Reason]),
			{noreply, State}
	end.

handle_info(Info, State) ->
	case catch do_handle_info(Info, State) of
		{noreply, NState} ->
			{noreply, NState};
		Reason ->
			?ERR("mgr_toutiao Request ~w Reason ~w",[Info, Reason]),
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
do_handle_info(zero_up, State=#mgr_toutiao{list=List}) ->
	EndTime = util:today() - ?LIST_END_TIME,
	NList = api_todayhot:get_end_time_news(List, EndTime, []),
	erlang:send_after(86400*1000, self(), zero_up),
	{noreply, State#mgr_toutiao{list=NList}};	
do_handle_info(start_spider, State=#mgr_toutiao{list=List}) ->
	Now = util:now(),
	do_start_spider(State, List, Now);
do_handle_info(_Msg, State) ->
	{noreply, State}.

do_start_spider(State, List, Now) ->
    case ibrowse:send_req(?URL, [], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			{NNews, Time} = parse_body(BodyTerm, List, Now),
			?IF(length(NNews)>0, mgr_todayhot:send({up_news, ?todayhot_class_compre, ?todayhot_node_toutiao, NNews, Time}), ignored),
			?INFO("toutiao NNews len ~w", [length(NNews)]),
			erlang:send_after(?spider_sec*1000, self(), start_spider),
			{noreply, State#mgr_toutiao{list = List++NNews}};
		_Err ->
			?ERR("fail ~w", [_Err]),
			erlang:send_after(10*1000, self(), start_spider),
			{noreply, State}
	end.

    
parse_body(Body, List, Now) ->
	case proplists:get_value(<<"data">>, Body, undefined) of
		Data when is_list(Data) ->
			% Feeds  = proplists:get_value(<<"content">>, Data, []),
			do_parse_data(Data, List, [], Now);
		_ ->
			{[], Now}
	end.
	
do_parse_data([], _List, News, Time) ->
	{News, Time};
do_parse_data([OData|T], OList, News, Now) ->
	Data0 = proplists:get_value(<<"content">>, OData),
	Data1 = jsx:decode(Data0),
	Data = proplists:get_value(<<"raw_data">>, Data1),
	% PInfo = proplists:get_value(<<"preload_info">>, Data),
	Title = proplists:get_value(<<"title">>, Data),
	
	case api_todayhot:is_exist(Title, OList)  of
		false -> %% 最新的新闻才存储
			Url = ?l2b(?HOST ++ ?i2l(proplists:get_value(<<"id">>, Data1))),
			?INFO("Url~ts", [Url]),?INFO("Title~ts",[Title]),
			% Url = ?l2b(?Host ++ "/" ++ Param),
			% Title = proplists:get_value(<<"title">>, Data),
			Abstract = proplists:get_value(<<"intro">>, Data, <<"">>),
			Source = proplists:get_value(<<"source">>, Data, <<"">>),
			Img = proplists:get_value(<<"url">>, Data, <<"">>),
			TNews = #todayhot_news{
				class = ?todayhot_class_compre,
				node_id = ?todayhot_node_toutiao, sub_news=[#todayhot_sub_news{title = Title, url=Url, time=Now, source=Source}]
				, abstract = Abstract, time=Now, img = Img
			},
			do_parse_data(T, OList, [TNews|News], Now);
		_ ->
			do_parse_data(T, OList, News, Now)
	end.
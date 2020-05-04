-module(api_todayhot).
-include("common.hrl").
-include("todayhot.hrl").
-export([
		is_exist/2, get_node/2, get_other_node/2, get_today_node/2, insert_today/1, insert_other/1,  
		up_today_to_other/0
		,get_node_news/2, get_node_news/3,get_node_up_time/2,
		get_end_time_news/3,get_node_news_num/3,
		insert_new_hot/3
]).


insert_new_hot(Class, NodeId, NewsL) ->
	case ets:lookup(?ETS_TODAYHOT_HOTLIST, {Class, NodeId}) of
		[TNode] ->
			ets:insert(?ETS_TODAYHOT_HOTLIST, TNode#todayhot_nodes_hotlist{news = NewsL});
		_ -> 
			ets:insert(?ETS_TODAYHOT_HOTLIST, #todayhot_nodes_hotlist{class_node={Class,NodeId}, news = NewsL})
	end.

is_exist(_Title, []) -> false;
is_exist(Title, [#todayhot_news{sub_news=SubNews}|T]) ->
    case lists:keymember(Title, #todayhot_sub_news.title, SubNews) of
        true -> true;
        _ ->
            is_exist(Title, T)
	end.
	
get_node(Class, NodeId) ->
	Node = get_today_node(Class, NodeId),
	ONode = get_other_node(Class, NodeId),
	case Node=/=[] of
		true when ONode=/=[] ->
			Node#todayhot_nodes{news= ONode#todayhot_nodes.news++Node#todayhot_nodes.news};
		true -> Node;
		_ when ONode=/=[] -> ONode;
		_ -> []
	end.

get_node_news_num(Class, NodeId, Num) ->
	case api_todayhot:get_today_node(Class, NodeId) of
		#todayhot_nodes{news = News} when Num > 0  ->
			TNum = length(News),
			case TNum >= Num of
				true ->
					lists:sublist(News, Num);
				_ ->
					SNum = Num - TNum,
					#todayhot_nodes{news = ONews} = get_other_node(Class, NodeId),
					lists:sublist(News, TNum) ++ lists:sublist(ONews, SNum)
			end;
		#todayhot_nodes{news = News} ->
			News;
		_ -> []
    end.

get_node_news(Class, NodeId) ->
	EndTime = util:today() - ?LIST_END_TIME,
	get_node_news(Class, NodeId, EndTime).
get_node_news(Class, NodeId, Time) ->
	case api_todayhot:get_node(Class, NodeId) of
		#todayhot_nodes{news = News} when Time > 0 ->
			get_end_time_news(News, Time, []);
		#todayhot_nodes{news = News} ->
			News;
		_ -> []
    end.
    
get_node_up_time(Class, NodeId) ->
	case api_todayhot:get_node(Class, NodeId) of
		#todayhot_nodes{up_time = Time} ->
			Time;
		_ -> 0
	end.

get_end_time_news([], _E, ENewsL) ->ENewsL;
get_end_time_news([TN=#todayhot_news{time=Time}|NewsL], EndTime, ENewsL) when Time>=EndTime ->
	get_end_time_news(NewsL, EndTime, [TN|ENewsL]);
get_end_time_news(_, _EndTime, ENewsL) ->
	ENewsL.

get_other_node(Class, NodeId) ->
	case ets:lookup(?ETS_TODAYHOT, {Class, NodeId}) of
		[TNode] -> TNode;
		_ -> []
	end.

get_today_node(Class, NodeId) ->
	case ets:lookup(?ETS_TODAYHOT_TODAY, {Class, NodeId}) of
		[TNode] -> TNode;
		_ -> []
	end.

insert_today(Node) ->
	ets:insert(?ETS_TODAYHOT_TODAY, Node).

insert_other(Node) ->
	ets:insert(?ETS_TODAYHOT, Node).

get_today_data() ->
	ets:tab2list(?ETS_TODAYHOT_TODAY).

up_today_to_other() ->
	TodayData = get_today_data(),
	Fun = fun(TN=#todayhot_nodes{class_node={Class, NodeId}, news=TodayNewsL}) ->
		case get_other_node(Class, NodeId) of
			#todayhot_nodes{news=NewsL} ->
				NTN = TN#todayhot_nodes{news = lists:reverse(lists:keysort(#todayhot_news.id, NewsL++TodayNewsL))},
				insert_today(TN#todayhot_nodes{news=[]}),
				insert_other(NTN);
			_ ->
				insert_today(TN#todayhot_nodes{news=[]}),
				insert_other(TN)
		end
	end,
	lists:foreach(Fun, TodayData).
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


insert_new_hot(_Class, NodeId, NewsL) ->
	Today = util:today(),
	case ets:lookup(?ETS_TODAYHOT_HOTLIST, {NodeId, Today}) of
		[TNode] ->
			ets:insert(?ETS_TODAYHOT_HOTLIST, TNode#todayhot_nodes_hotlist{news = NewsL});
		_ -> 
			ets:insert(?ETS_TODAYHOT_HOTLIST, #todayhot_nodes_hotlist{node={NodeId,Today}, news = NewsL})
	end.

is_exist(_Title, []) -> false;
is_exist(Title, [#todayhot_news{sub_news=SubNews}|T]) ->
    case lists:keymember(Title, #todayhot_sub_news.title, SubNews) of
        true -> true;
        _ ->
            is_exist(Title, T)
	end.
	
get_node(NodeId, Zero) ->
	Node = get_today_node(NodeId, Zero),
	ONode = get_other_node(NodeId, Zero),
	case Node=/=[] of
		true when ONode=/=[] ->
			Node#todayhot_node_news{news= ONode#todayhot_node_news.news++Node#todayhot_node_news.news};
		true -> Node;
		_ when ONode=/=[] -> ONode;
		_ -> []
	end.

get_node_news_num(NodeId, Zero, Num) ->
	case api_todayhot:get_today_node(NodeId, Zero) of
		#todayhot_node_news{news = News} when Num > 0  ->
			TNum = length(News),
			case TNum >= Num of
				true ->
					lists:sublist(News, Num);
				_ ->
					SNum = Num - TNum,
					#todayhot_node_news{news = ONews} = get_other_node(NodeId, Zero),
					lists:sublist(News, TNum) ++ lists:sublist(ONews, SNum)
			end;
		#todayhot_node_news{news = News} ->
			News;
		_ -> []
    end.

get_node_news(NodeId, Zero) ->
	EndTime = util:today() - ?LIST_END_TIME,
	get_node_news(NodeId, Zero, EndTime).
get_node_news(NodeId, Zero, Time) ->
	case api_todayhot:get_node(NodeId, Zero) of
		#todayhot_node_news{news = News} when Time > 0 ->
			get_end_time_news(News, Time, []);
		#todayhot_node_news{news = News} ->
			News;
		_ -> []
    end.
    
get_node_up_time(NodeId, Zero) ->
	case api_todayhot:get_node(NodeId, Zero) of
		#todayhot_node_news{up_time = Time} ->
			Time;
		_ -> 0
	end.

get_end_time_news([], _E, ENewsL) ->ENewsL;
get_end_time_news([TN=#todayhot_news{time=Time}|NewsL], EndTime, ENewsL) when Time>=EndTime ->
	get_end_time_news(NewsL, EndTime, [TN|ENewsL]);
get_end_time_news(_, _EndTime, ENewsL) ->
	ENewsL.

get_other_node(NodeId, Today) ->
	case ets:lookup(?ETS_TODAYHOT_NEWS, {NodeId, Today}) of
		[TNode] -> TNode;
		_ -> []
	end.

get_today_node(NodeId, Today) ->
	case ets:lookup(?ETS_TODAYHOT_TODAY, {NodeId, Today}) of
		[TNode] -> TNode;
		_ -> []
	end.

insert_today(Node) ->
	ets:insert(?ETS_TODAYHOT_TODAY, Node).

insert_other(Node) ->
	ets:insert(?ETS_TODAYHOT_NEWS, Node).

get_today_data() ->
	ets:tab2list(?ETS_TODAYHOT_TODAY).

up_today_to_other() ->
	TodayData = get_today_data(),
	Fun = fun(TN=#todayhot_node_news{node={NodeId, Today}, news=TodayNewsL}) ->
		case get_other_node(NodeId, Today) of
			#todayhot_node_news{news=NewsL} ->
				NTN = TN#todayhot_node_news{news = lists:reverse(lists:keysort(#todayhot_news.id, NewsL++TodayNewsL))},
				insert_today(TN#todayhot_node_news{news=[]}),
				insert_other(NTN);
			_ ->
				insert_today(TN#todayhot_node_news{news=[]}),
				insert_other(TN)
		end
	end,
	lists:foreach(Fun, TodayData).
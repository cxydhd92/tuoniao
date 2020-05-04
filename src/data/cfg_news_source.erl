%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
-module(cfg_news_source).
-include("common.hrl").
-export([get/1, list_key/0]).
-export([
		news_source_class/1
	]).

-include("cfg_news_source.hrl").

get(10001) ->
	#cfg_news_source{
		source_id = 10001,
		class = 1,
		sub_class = 1,
		name = <<"好奇心日报"/utf8>>,
		icon_name = <<"qdaily"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10002) ->
	#cfg_news_source{
		source_id = 10002,
		class = 1,
		sub_class = 2,
		name = <<"澎湃新闻.热闻"/utf8>>,
		icon_name = <<"thepaper"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10003) ->
	#cfg_news_source{
		source_id = 10003,
		class = 1,
		sub_class = 3,
		name = <<"今日头条.热榜"/utf8>>,
		icon_name = <<"toutiao"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10004) ->
	#cfg_news_source{
		source_id = 10004,
		class = 1,
		sub_class = 4,
		name = <<"网易新闻.热榜"/utf8>>,
		icon_name = <<"163"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20001) ->
	#cfg_news_source{
		source_id = 20001,
		class = 2,
		sub_class = 1,
		name = <<"readhub"/utf8>>,
		icon_name = <<"readhub"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20002) ->
	#cfg_news_source{
		source_id = 20002,
		class = 2,
		sub_class = 1,
		name = <<"虎嗅.最新"/utf8>>,
		icon_name = <<"huxiu"/utf8>>,
		url = <<"https://rss.huxiu.com/"/utf8>>,
		type = 3,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20003) ->
	#cfg_news_source{
		source_id = 20003,
		class = 2,
		sub_class = 1,
		name = <<"36kr.热榜"/utf8>>,
		icon_name = <<"36kr"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20004) ->
	#cfg_news_source{
		source_id = 20004,
		class = 2,
		sub_class = 1,
		name = <<"donews.热榜"/utf8>>,
		icon_name = <<"donews"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20005) ->
	#cfg_news_source{
		source_id = 20005,
		class = 2,
		sub_class = 1,
		name = <<"网易科技.排行"/utf8>>,
		icon_name = <<"163"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20006) ->
	#cfg_news_source{
		source_id = 20006,
		class = 2,
		sub_class = 1,
		name = <<"钛媒体.最新"/utf8>>,
		icon_name = <<"tmt"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20007) ->
	#cfg_news_source{
		source_id = 20007,
		class = 2,
		sub_class = 1,
		name = <<"钛媒体.热门"/utf8>>,
		icon_name = <<"tmt"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20008) ->
	#cfg_news_source{
		source_id = 20008,
		class = 2,
		sub_class = 1,
		name = <<"腾讯科技"/utf8>>,
		icon_name = <<"qqtech"/utf8>>,
		url = <<"https://pacaio.match.qq.com/irs/rcd?cid=137&token=d0f13d594edfc180f5bf6b845456f3ea&id=&ext=tech&num=20"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"vurl"/utf8>>,
		desc = <<"intro"/utf8>>,
		author = <<"source"/utf8>>,
		img = <<"bimg"/utf8>>,
		count = <<"view_count"/utf8>>,
		time = <<"update_time"/utf8>>,
		time_type = 2
	};

get(60001) ->
	#cfg_news_source{
		source_id = 60001,
		class = 6,
		sub_class = 1,
		name = <<"知乎热榜"/utf8>>,
		icon_name = <<"zhihu"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(60002) ->
	#cfg_news_source{
		source_id = 60002,
		class = 6,
		sub_class = 1,
		name = <<"知乎日报"/utf8>>,
		icon_name = <<"zhihu"/utf8>>,
		url = <<""/utf8>>,
		type = 0,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<""/utf8>>,
		link_a = <<""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110001) ->
	#cfg_news_source{
		source_id = 110001,
		class = 11,
		sub_class = 1,
		name = <<"微博热搜"/utf8>>,
		icon_name = <<"weibo"/utf8>>,
		url = <<"https://s.weibo.com/top/summary?cate=realtimehot"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://s.weibo.com"/utf8>>,
		data = <<"<tbody>([\\s\\S]+?)</tbody>"/utf8>>,
		container = <<"<tr.+?class=\"\">\\s+?<td.+?class=\"td-\\d+.+?ranktop\">([\\s\\S]+?)</tr>"/utf8>>,
		title = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span>([\\s\\S]+?)</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(_Id) -> 
	false.

list_key() ->
	[
		10001,
		10002,
		10003,
		10004,
		20001,
		20002,
		20003,
		20004,
		20005,
		20006,
		20007,
		20008,
		60001,
		60002,
		110001
	].news_source_class(1) ->
	[10001,10002,10003,10004];

news_source_class(2) ->
	[20001,20002,20003,20004,20005,20006,20007,20008];

news_source_class(6) ->
	[60001,60002];

news_source_class(11) ->
	[110001];

news_source_class(_) -> []. 


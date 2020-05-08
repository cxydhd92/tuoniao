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
		class = 2,
		sub_class = 1,
		name = <<"好奇心日报"/utf8>>,
		icon_name = <<"qdaily"/utf8>>,
		url = <<"https://www.qdaily.com/tags/tagmore/29/"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.qdaily.com/articles/"/utf8>>,
		data = <<"data|feeds"/utf8>>,
		container = <<"post"/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"id"/utf8>>,
		desc = <<"description"/utf8>>,
		author = <<""/utf8>>,
		img = <<"image"/utf8>>,
		count = <<""/utf8>>,
		time = <<"publish_time"/utf8>>,
		time_type = 2
	};

get(10002) ->
	#cfg_news_source{
		source_id = 10002,
		class = 1,
		sub_class = 2,
		name = <<"澎湃.今日热榜"/utf8>>,
		icon_name = <<"thepaper"/utf8>>,
		url = <<"https://www.thepaper.cn/"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.thepaper.cn/"/utf8>>,
		data = <<"<ul.+?class=\"list_hot\".+?id=\"listhot0\">([\\s\\S]+?)</ul>"/utf8>>,
		container = <<"<li[^>]+?>([\\s\\S]+?)</li>"/utf8>>,
		title = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
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
		class = 11,
		sub_class = 3,
		name = <<"今日头条.热榜"/utf8>>,
		icon_name = <<"toutiao"/utf8>>,
		url = <<"https://api3-normal-c-lq.snssdk.com/api/feed/hotboard_online/v1/?category=hotboard_online&count=50"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.toutiao.com/a"/utf8>>,
		data = <<"data"/utf8>>,
		container = <<"content|raw_data"/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"hot_item_id"/utf8>>,
		desc = <<"hot_desc"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<"hot_value"/utf8>>,
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
		url = <<"https://api.readhub.cn/topic?lastCursor=&pageSize=20"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<"https://www.readhub.cn/topics/"/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"id"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"createdAt"/utf8>>,
		time_type = 3
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
		url = <<"https://36kr.com/hot-list/catalog"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://36kr.com"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<a.+?class=\"article-item-title weight-bold\"[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<a.+?class=\"article-item-title weight-bold\"[^>]+?href=\"([^\"]+?)\""/utf8>>,
		desc = <<"<a.+?class=\"article-item-description ellipsis-2\"[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
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
		name = <<"少数派.热榜"/utf8>>,
		icon_name = <<"sspai"/utf8>>,
		url = <<"https://sspai.com/api/v1/article/tag/page/get?limit=10&offset=0&tag=热门文章&released=false"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://sspai.com/post/"/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"id"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<"author|nickname"/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"released_time"/utf8>>,
		time_type = 1
	};

get(20005) ->
	#cfg_news_source{
		source_id = 20005,
		class = 2,
		sub_class = 1,
		name = <<"虎嗅.热榜"/utf8>>,
		icon_name = <<"huxiu"/utf8>>,
		url = <<"https://www.huxiu.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.huxiu.com"/utf8>>,
		data = <<"class=\"focus-item-right-wrap\"[^>]+?>([\\s\\S]+?)</div>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
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
		url = <<"https://www.tmtpost.com/httpsserver/common/get?url=/v1/lists/home&data=offset=2&limit="/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"short_url"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time_published_utc"/utf8>>,
		time_type = 1
	};

get(20007) ->
	#cfg_news_source{
		source_id = 20007,
		class = 2,
		sub_class = 1,
		name = <<"钛媒体.热榜"/utf8>>,
		icon_name = <<"tmt"/utf8>>,
		url = <<"https://www.tmtpost.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.tmtpost.com"/utf8>>,
		data = <<"class=\"icon-hot_post\">([\\s\\S]+?)</ol>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
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
		name = <<"腾讯.科技"/utf8>>,
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
		name = <<"知乎.热榜"/utf8>>,
		icon_name = <<"zhihu"/utf8>>,
		url = <<"https://www.zhihu.com/billboard"/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"initialState|topstory|hotList"/utf8>>,
		container = <<"target"/utf8>>,
		title = <<"titleArea|text"/utf8>>,
		link_a = <<"link|url"/utf8>>,
		desc = <<"excerptArea|text"/utf8>>,
		author = <<""/utf8>>,
		img = <<"imageArea|url"/utf8>>,
		count = <<"metricsArea|text"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0,
		json_data = <<"{\"initialState\"((.|\n)*?)\"subAppName\":\"mobile\"}"/utf8>>
	};

get(60002) ->
	#cfg_news_source{
		source_id = 60002,
		class = 6,
		sub_class = 1,
		name = <<"知乎.日报"/utf8>>,
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
		name = <<"微博.热搜榜"/utf8>>,
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
	].
news_source_class(1) ->
	[10001,10002,10003,10004];

news_source_class(2) ->
	[20001,20002,20003,20004,20005,20006,20007,20008];

news_source_class(6) ->
	[60001,60002];

news_source_class(11) ->
	[110001];

news_source_class(_) -> []. 


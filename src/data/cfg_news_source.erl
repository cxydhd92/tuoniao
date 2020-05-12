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
		type = 4,
		is_top = 1,
		link_pre = <<"https://www.huxiu.com/article/"/utf8>>,
		data = <<"news|articleHot"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"aid"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<"user_info|username"/utf8>>,
		img = <<""/utf8>>,
		count = <<"count_info|viewnum"/utf8>>,
		time = <<"dateline"/utf8>>,
		time_type = 1,
		json_data = <<"{\"route\":((.|\n)*?)\"isBidRelated\":false}}"/utf8>>
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

get(20009) ->
	#cfg_news_source{
		source_id = 20009,
		class = 2,
		sub_class = 1,
		name = <<"IT之家.日榜"/utf8>>,
		icon_name = <<"ithome"/utf8>>,
		url = <<"https://www.ithome.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<h4>24小时阅读榜</h4><ul>([\\s\\S]+?)</ul>"/utf8>>,
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

get(20010) ->
	#cfg_news_source{
		source_id = 20010,
		class = 2,
		sub_class = 1,
		name = <<"科普中国.热榜"/utf8>>,
		icon_name = <<"kepuchina"/utf8>>,
		url = <<"https://www.kepuchina.cn/tech/"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<h2>热点排行</h2>[\\s\\S]+?<ul class=\"list\">([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2><a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<h2><a href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<em>([\\s\\S]+?)</em>"/utf8>>,
		img = <<"src=\"([^\"]+?)\""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20011) ->
	#cfg_news_source{
		source_id = 20011,
		class = 2,
		sub_class = 1,
		name = <<"爱范儿.最新"/utf8>>,
		icon_name = <<"ifanr"/utf8>>,
		url = <<"https://sso.ifanr.com/api/v5/wp/web-feed/?limit=20&offset=0"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"objects"/utf8>>,
		container = <<""/utf8>>,
		title = <<"post_title"/utf8>>,
		link_a = <<"post_url"/utf8>>,
		desc = <<"post_excerpt"/utf8>>,
		author = <<"created_by|name"/utf8>>,
		img = <<"post_cover_image"/utf8>>,
		count = <<""/utf8>>,
		time = <<"created_at"/utf8>>,
		time_type = 1
	};

get(20012) ->
	#cfg_news_source{
		source_id = 20012,
		class = 2,
		sub_class = 1,
		name = <<"威锋网.最新"/utf8>>,
		icon_name = <<"feng"/utf8>>,
		url = <<"https://www.feng.com/news/all"/utf8>>,
		type = 2,
		is_top = 0,
		link_pre = <<"https://www.feng.com"/utf8>>,
		data = <<"<ul class=\"home-list-wrap\"[^>]+?>([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
		desc = <<"<div class=\"desc\"[^>]+?>([\\s\\S]+?)</div>"/utf8>>,
		author = <<"<span class=\"name pointer\"[^>]+?>([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20013) ->
	#cfg_news_source{
		source_id = 20013,
		class = 2,
		sub_class = 1,
		name = <<"AppSolution.最新"/utf8>>,
		icon_name = <<"ifanr"/utf8>>,
		url = <<"https://sso.ifanr.com/api/v5/wp/article/?post_type=app&limit=1&offset=0"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"objects"/utf8>>,
		container = <<""/utf8>>,
		title = <<"post_title"/utf8>>,
		link_a = <<"post_url"/utf8>>,
		desc = <<"post_excerpt"/utf8>>,
		author = <<"created_by|name"/utf8>>,
		img = <<"post_cover_image"/utf8>>,
		count = <<""/utf8>>,
		time = <<"created_at"/utf8>>,
		time_type = 1
	};

get(20015) ->
	#cfg_news_source{
		source_id = 20015,
		class = 2,
		sub_class = 1,
		name = <<"IT之家.周榜"/utf8>>,
		icon_name = <<"ithome"/utf8>>,
		url = <<"https://www.ithome.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<h4>周榜</h4><ul>([\\s\\S]+?)</ul>"/utf8>>,
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

get(20016) ->
	#cfg_news_source{
		source_id = 20016,
		class = 2,
		sub_class = 1,
		name = <<"科普中国.最新"/utf8>>,
		icon_name = <<"kepuchina"/utf8>>,
		url = <<"https://www.kepuchina.cn/more/index.shtml"/utf8>>,
		type = 2,
		is_top = 0,
		link_pre = <<"https://www.kepuchina.cn"/utf8>>,
		data = <<"<h2>滚动</h2>([\\s\\S]+?)<h2>热点排行</h2>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2><a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<h2><a href=\"[.]([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<p><span>([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20017) ->
	#cfg_news_source{
		source_id = 20017,
		class = 2,
		sub_class = 1,
		name = <<"IT之家.月榜"/utf8>>,
		icon_name = <<"ithome"/utf8>>,
		url = <<"https://www.ithome.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<h4>月榜</h4><ul>([\\s\\S]+?)</ul>"/utf8>>,
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

get(20019) ->
	#cfg_news_source{
		source_id = 20019,
		class = 2,
		sub_class = 1,
		name = <<"IT之家.7天热评"/utf8>>,
		icon_name = <<"ithome"/utf8>>,
		url = <<"https://www.ithome.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<h4>7天热评</h4><ul>([\\s\\S]+?)</ul>"/utf8>>,
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
		url = <<"https://daily.zhihu.com/"/utf8>>,
		type = 2,
		is_top = 0,
		link_pre = <<"https://daily.zhihu.com"/utf8>>,
		data = <<"<div class=\"box\">([\\s\\S]+?)</div>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<span class=\"title\">([\\s\\S]+?)</span>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"src=\"([^\"]+?)\""/utf8>>,
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
		20009,
		20010,
		20011,
		20012,
		20013,
		20015,
		20016,
		20017,
		20019,
		60001,
		60002,
		110001
	].
news_source_class(1) ->
	[10001,10002,10003,10004];

news_source_class(2) ->
	[20001,20002,20003,20004,20005,20006,20007,20008,20009,20010,20011,20012,20013,20015,20016,20017,20019];

news_source_class(6) ->
	[60001,60002];

news_source_class(11) ->
	[110001, 10003];

news_source_class(_) -> []. 


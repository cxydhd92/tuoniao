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
		name = <<"好奇心日报.Top15"/utf8>>,
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
		class = 1,
		sub_class = 3,
		name = <<"今日头条.热榜"/utf8>>,
		icon_name = <<"toutiao"/utf8>>,
		url = <<"https://api3-normal-c-lq.snssdk.com/api/feed/hotboard_online/v1/?category=hotboard_online&count=50"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://is-lq.snssdk.com/search/?keyword=%2523"/utf8>>,
		data = <<"data"/utf8>>,
		container = <<"content|raw_data"/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"title"/utf8>>,
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
		name = <<"新浪新闻.总热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"http://top.news.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=www_www_all_suda_suda&top_show_num=50&top_order=DESC&js_var=all_1_data01&top_time="/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time"/utf8>>,
		time_type = 4,
		json_data = <<"{\"conf\":((.|\n)*?)}]}"/utf8>>
	};

get(10005) ->
	#cfg_news_source{
		source_id = 10005,
		class = 1,
		sub_class = 4,
		name = <<"微信.24H热榜"/utf8>>,
		icon_name = <<"wechat"/utf8>>,
		url = <<"https://m.gsdata.cn/Mobile_Articles_wxarticle.html"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p class=\"article__name\">([\\s\\S]+?)</p>"/utf8>>,
		link_a = <<"url=([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<span class=\"r from\">([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span class=\"l see\"><i></i>([\\s\\S]+?)\\+</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10006) ->
	#cfg_news_source{
		source_id = 10006,
		class = 1,
		sub_class = 4,
		name = <<"人民网.观点24H热榜"/utf8>>,
		icon_name = <<"people"/utf8>>,
		url = <<""/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"http://opinion.people.com.cn"/utf8>>,
		data = <<"<ol>([\\s\\S]+?)</ol>"/utf8>>,
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

get(10007) ->
	#cfg_news_source{
		source_id = 10007,
		class = 1,
		sub_class = 4,
		name = <<"微信-时事.24H热榜"/utf8>>,
		icon_name = <<"wechat"/utf8>>,
		url = <<"https://m.gsdata.cn/Mobile_Articles_wxarticle.html?post_time=2&types=时事&industry=all&industry_type=&industry2=全部"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p class=\"article__name\">([\\s\\S]+?)</p>"/utf8>>,
		link_a = <<"url=([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<span class=\"r from\">([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span class=\"l see\"><i></i>([\\s\\S]+?)\\+</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10008) ->
	#cfg_news_source{
		source_id = 10008,
		class = 1,
		sub_class = 4, 
		name = <<"新浪新闻.社会热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"http://top.news.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=news_society_suda&top_show_num=20&top_order=DESC&js_var=news_&top_time="/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time"/utf8>>,
		time_type = 4,
		json_data = <<"{\"conf\":((.|\n)*?)}]}"/utf8>>
	};

get(10009) ->
	#cfg_news_source{
		source_id = 10009,
		class = 1,
		sub_class = 4, 
		name = <<"梨视频.总榜"/utf8>>,
		icon_name = <<"pearvideo"/utf8>>,
		url = <<"https://www.pearvideo.com/popular_loading.jsp?reqType=1&categoryId=&start=0&sort=0"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.pearvideo.com/video_"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2[^>]+?>([\\s\\S]+?)</h2>"/utf8>>,
		link_a = <<"data-id=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(10010) ->
	#cfg_news_source{
		source_id = 10010,
		class = 1,
		sub_class = 1,
		name = <<"凤凰资讯.热榜"/utf8>>,
		icon_name = <<"ifeng"/utf8>>,
		url = <<"http://news.ifeng.com/c/api/content/graphic/article"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"img"/utf8>>,
		count = <<"num"/utf8>>,
		time = <<"publishTimestamp"/utf8>>,
		time_type = 1
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
		link_pre = <<"https://www.readhub.cn/topic/"/utf8>>,
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
		url = <<"https://sso.ifanr.com/api/v5/wp/article/?post_type=app&limit=2&offset=0"/utf8>>,
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

get(20014) ->
	#cfg_news_source{
		source_id = 20014,
		class = 2,
		sub_class = 1,
		name = <<"威锋网.社区热门"/utf8>>,
		icon_name = <<"feng"/utf8>>,
		url = <<"https://beta-api.feng.com/v1/flow/topic/feed?topicId=0&feedTyp=hot&pageCount=6"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.feng.com/post/"/utf8>>,
		data = <<"data|dataList"/utf8>>,
		container = <<"thread"/utf8>>,
		title = <<"subject"/utf8>>,
		link_a = <<"tid"/utf8>>,
		desc = <<""/utf8>>,
		author = <<"author"/utf8>>,
		img = <<"newsIcon"/utf8>>,
		count = <<"views"/utf8>>,
		time = <<"dateline"/utf8>>,
		time_type = 1,
		head = [{"X-Request-Id", "JRwMHTWcL7zDg5snIl2ffyKRa8SS3hLmq56YI/WpkfnS5MFaBXKvPk9JovDwtuiCcKt++EHpJKVQt9QVeN4ivQ=="}]
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

get(20018) ->
	#cfg_news_source{
		source_id = 20018,
		class = 2,
		sub_class = 1,
		name = <<"威锋网.72h热榜"/utf8>>,
		icon_name = <<"feng"/utf8>>,
		url = <<"https://beta-api.feng.com/v1/flow/topic/feed?topicId=0&feedTyp=new_hot&pageCount=6"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.feng.com/post/"/utf8>>,
		data = <<"data|dataList"/utf8>>,
		container = <<"thread"/utf8>>,
		title = <<"subject"/utf8>>,
		link_a = <<"tid"/utf8>>,
		desc = <<""/utf8>>,
		author = <<"author"/utf8>>,
		img = <<"newsIcon"/utf8>>,
		count = <<"views"/utf8>>,
		time = <<"dateline"/utf8>>,
		time_type = 1,
		head = [{"X-Request-Id", "JRwMHTWcL7zDg5snIl2ffyKRa8SS3hLmq56YI/WpkfkVG7SKctpbUL64UY6BpxeZdUoeqg9PmBAHxkXxVu7zwg=="}]
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

get(20020) ->
	#cfg_news_source{
		source_id = 20020,
		class =2,
		sub_class = 1,
		name = <<"极客公园.最新"/utf8>>,
		icon_name = <<"geekpark"/utf8>>,
		url = <<"https://mainssl.geekpark.net/api/v2?page=1"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<"https://www.geekpark.net/news/"/utf8>>,
		data = <<"homepage_posts"/utf8>>,
		container = <<"post"/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"id"/utf8>>,
		desc = <<"abstract"/utf8>>,
		author = <<"authors|nickname"/utf8>>,
		img = <<"cover_url"/utf8>>,
		count = <<""/utf8>>,
		time = <<"published_timestamp"/utf8>>,
		time_type = 1
	};

get(20021) ->
	#cfg_news_source{
		source_id = 20021,
		class = 2,
		sub_class = 1,
		name = <<"36Kr.最新"/utf8>>,
		icon_name = <<"36kr"/utf8>>,
		url = <<"https://36kr.com/information/web_news/latest"/utf8>>,
		type = 4,
		is_top = 0,
		link_pre = <<"https://36kr.com/p/"/utf8>>,
		data = <<"information|informationList|itemList"/utf8>>,
		container = <<"templateMaterial"/utf8>>,
		title = <<"widgetTitle"/utf8>>,
		link_a = <<"itemId"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<"authorName"/utf8>>,
		img = <<"widgetImage"/utf8>>,
		count = <<""/utf8>>,
		time = <<"publishTime"/utf8>>,
		time_type = 5,
		json_data = <<"{\"navigator\"((.|\n)*?)\"isCheckedUserInfo\":false}"/utf8>>
	};

get(20022) ->
	#cfg_news_source{
		source_id = 20022,
		class = 2,
		sub_class = 1,
		name = <<"数字尾巴.最新"/utf8>>,
		icon_name = <<"dgtle"/utf8>>,
		url = <<"https://www.dgtle.com/rss/dgtle.xml"/utf8>>,
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

get(20023) ->
	#cfg_news_source{
		source_id = 20023,
		class = 2,
		sub_class = 1,
		name = <<"凤凰科技.24H必读"/utf8>>,
		icon_name = <<"ifeng"/utf8>>,
		url = <<"https://tech.ifeng.com/24h/"/utf8>>,
		type = 4,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"initNewsData"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<"source"/utf8>>,
		img = <<"thumbnail"/utf8>>,
		count = <<""/utf8>>,
		time = <<"newsTime"/utf8>>,
		time_type = 2,
		json_data = <<"{\"nav\"((.|\n)*?)\"}]}"/utf8>>
	};

get(20024) ->
	#cfg_news_source{
		source_id = 20024,
		class = 2,
		sub_class = 1,
		name = <<"新浪科技.最新"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"https://feed.mix.sina.com.cn/api/roll/get?pageid=372&lid=2431&k=&num=50&page=1"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"result|data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"wapurl"/utf8>>,
		desc = <<"summary"/utf8>>,
		author = <<"author"/utf8>>,
		img = <<"img|u"/utf8>>,
		count = <<""/utf8>>,
		time = <<"ctime"/utf8>>,
		time_type = 1
	};

get(20025) ->
	#cfg_news_source{
		source_id = 20025,
		class = 2,
		sub_class = 1,
		name = <<"新浪科技.热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"https://tech.sina.com.cn"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<ul class=\"rank-con\" id=\"rcon1\">([\\s\\S]+?)</ul>"/utf8>>,
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

get(20026) ->
	#cfg_news_source{
		source_id = 20026,
		class = 2,
		sub_class = 1,
		name = <<"雷科技.最新"/utf8>>,
		icon_name = <<"leikeji"/utf8>>,
		url = <<"https://www.leikeji.com"/utf8>>,
		type = 2,
		is_top = 0,
		link_pre = <<"https://www.leikeji.com"/utf8>>,
		data = <<"<div class=\"feed-list\">([\\s\\S]+?)>更多</a>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2><a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<h2><a[^>]+?href=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p class=\"item-desc\">([\\s\\S]+?)</p>"/utf8>>,
		author = <<"<div class=\"user-info\"><a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		img = <<"data-original=\"([^\"]+?)\""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20027) ->
	#cfg_news_source{
		source_id = 20027,
		class = 2,
		sub_class = 1,
		name = <<"雷科技.热榜"/utf8>>,
		icon_name = <<"leikeji"/utf8>>,
		url = <<"https://www.leikeji.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.leikeji.com"/utf8>>,
		data = <<"<ul class=\"aside-article-list\">([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h3>([\\s\\S]+?)</h3>"/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"data-original=\"([^\"]+?)\""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20028) ->
	#cfg_news_source{
		source_id = 20028,
		class = 2,
		sub_class = 4,
		name = <<"微信-科技.24H热榜"/utf8>>,
		icon_name = <<"wechat"/utf8>>,
		url = <<"https://m.gsdata.cn/Mobile_Articles_wxarticle.html?post_time=2&types=科技&industry=all&industry_type=&industry2=全部"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p class=\"article__name\">([\\s\\S]+?)</p>"/utf8>>,
		link_a = <<"url=([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<span class=\"r from\">([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span class=\"l see\"><i></i>([\\s\\S]+?)\\+</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(20029) ->
	#cfg_news_source{
		source_id = 20029,
		class = 2,
		sub_class = 4,
		name = <<"梨视频.科技热榜"/utf8>>,
		icon_name = <<"pearvideo"/utf8>>,
		url = <<"https://www.pearvideo.com/popular_loading.jsp?reqType=1&categoryId=8&start=0&sort=0"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.pearvideo.com/video_"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2[^>]+?>([\\s\\S]+?)</h2>"/utf8>>,
		link_a = <<"data-id=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(30001) ->
	#cfg_news_source{
		source_id = 30001,
		class = 3,
		sub_class = 4,
		name = <<"微信娱乐.24H热榜"/utf8>>,
		icon_name = <<"wechat"/utf8>>,
		url = <<"https://m.gsdata.cn/Mobile_Articles_wxarticle.html?post_time=2&types=科技&industry=all&industry_type=&industry2=全部"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p class=\"article__name\">([\\s\\S]+?)</p>"/utf8>>,
		link_a = <<"url=([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<"<span class=\"r from\">([\\s\\S]+?)</span>"/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span class=\"l see\"><i></i>([\\s\\S]+?)\\+</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(30002) ->
	#cfg_news_source{
		source_id = 30002,
		class = 3,
		sub_class = 4,
		name = <<"新浪娱乐.热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"http://top.ent.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=ent_suda&top_show_num=20&top_order=DESC&js_var=channel_&top_time="/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time"/utf8>>,
		time_type = 4,
		json_data = <<"{\"conf\":((.|\n)*?)}]}"/utf8>>
	};

get(30003) ->
	#cfg_news_source{
		source_id = 30003,
		class = 3,
		sub_class = 4,
		name = <<"梨视频.娱乐热榜"/utf8>>,
		icon_name = <<"pearvideo"/utf8>>,
		url = <<"https://www.pearvideo.com/popular_loading.jsp?reqType=1&categoryId=4&start=0&sort=0"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.pearvideo.com/video_"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2[^>]+?>([\\s\\S]+?)</h2>"/utf8>>,
		link_a = <<"data-id=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(30004) ->
	#cfg_news_source{
		source_id = 30004,
		class = 3,
		sub_class = 1,
		name = <<"凤凰娱乐.热榜"/utf8>>,
		icon_name = <<"ifeng"/utf8>>,
		url = <<"http://ent.ifeng.com/c/api/content/graphic/article"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"img"/utf8>>,
		count = <<"num"/utf8>>,
		time = <<"publishTimestamp"/utf8>>,
		time_type = 1
	};

get(50001) ->
	#cfg_news_source{
		source_id = 50001,
		class = 5,
		sub_class = 4,
		name = <<"新浪财经.热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"http://top.finance.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=finance_0_suda&top_show_num=20&top_order=DESC&js_var=channel_&top_time="/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time"/utf8>>,
		time_type = 4,
		json_data = <<"{\"conf\":((.|\n)*?)}]}"/utf8>>
	};

get(50002) ->
	#cfg_news_source{
		source_id = 50002,
		class = 5,
		sub_class = 4,
		name = <<"梨视频.财富热榜"/utf8>>,
		icon_name = <<"pearvideo"/utf8>>,
		url = <<"https://www.pearvideo.com/popular_loading.jsp?reqType=1&categoryId=3&start=0&sort=0"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.pearvideo.com/video_"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2[^>]+?>([\\s\\S]+?)</h2>"/utf8>>,
		link_a = <<"data-id=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50003) ->
	#cfg_news_source{
		source_id = 50003,
		class = 5,
		sub_class = 4,
		name = <<"第1财经.新闻周榜"/utf8>>,
		icon_name = <<"yicai"/utf8>>,
		url = <<"https://www.yicai.com/api/ajax/getranklistbykeys?keys=newsRank%2CvideoRank%2CimageRank%2CliveRank"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.yicai.com"/utf8>>,
		data = <<"newsRank|week"/utf8>>,
		container = <<""/utf8>>,
		title = <<"NewsTitle"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50004) ->
	#cfg_news_source{
		source_id = 50004,
		class = 5,
		sub_class = 4,
		name = <<"第1财经.视频周榜"/utf8>>,
		icon_name = <<"yicai"/utf8>>,
		url = <<"https://www.yicai.com/api/ajax/getranklistbykeys?keys=newsRank%2CvideoRank%2CimageRank%2CliveRank"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.yicai.com"/utf8>>,
		data = <<"videoRank|week"/utf8>>,
		container = <<""/utf8>>,
		title = <<"NewsTitle"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50005) ->
	#cfg_news_source{
		source_id = 50005,
		class = 5,
		sub_class = 4,
		name = <<"雪球网.热门文章"/utf8>>,
		icon_name = <<"xueqiu"/utf8>>,
		url = <<"https://xueqiu.com/statuses/hots.json?a=1&count=10&page=1&meigu=0&scope=day&type=news"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://xueqiu.com"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"target"/utf8>>,
		desc = <<""/utf8>>,
		author = <<"user|screen_name"/utf8>>,
		img = <<"pic"/utf8>>,
		count = <<"reply_count"/utf8>>,
		time = <<"created_at"/utf8>>,
		time_type = 5,
		head = [{"Cookie", "xqat=ea139be840cf88ff8c30e6943cf26aba8ad77358"}]
	};

get(50006) ->
	#cfg_news_source{
		source_id = 50006,
		class = 5,
		sub_class = 4,
		name = <<"第1财经.图集周榜"/utf8>>,
		icon_name = <<"yicai"/utf8>>,
		url = <<"https://www.yicai.com/api/ajax/getranklistbykeys?keys=newsRank%2CvideoRank%2CimageRank%2CliveRank"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://www.yicai.com"/utf8>>,
		data = <<"videoRank|week"/utf8>>,
		container = <<""/utf8>>,
		title = <<"NewsTitle"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50007) ->
	#cfg_news_source{
		source_id = 50007,
		class = 5,
		sub_class = 4,
		name = <<"雪球网.热门公告"/utf8>>,
		icon_name = <<"xueqiu"/utf8>>,
		url = <<"https://xueqiu.com/statuses/hots.json?a=1&count=10&page=1&meigu=0&scope=day&type=notice"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://xueqiu.com"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"target"/utf8>>,
		desc = <<"description"/utf8>>,
		author = <<"user|screen_name"/utf8>>,
		img = <<"pic"/utf8>>,
		count = <<"reply_count"/utf8>>,
		time = <<"created_at"/utf8>>,
		time_type = 5,
		head = [{"Cookie", "xqat=ea139be840cf88ff8c30e6943cf26aba8ad77358"}]
	};

get(50008) ->
	#cfg_news_source{
		source_id = 50008,
		class = 5,
		sub_class = 4,
		name = <<"雪球网.热贴"/utf8>>,
		icon_name = <<"xueqiu"/utf8>>,
		url = <<"https://xueqiu.com/statuses/hot/listV2.json"/utf8>>,
		type = 1,
		is_top = 0,
		link_pre = <<"https://xueqiu.com"/utf8>>,
		data = <<"items"/utf8>>,
		container = <<"original_status"/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"target"/utf8>>,
		desc = <<""/utf8>>,
		author = <<"user|screen_name"/utf8>>,
		img = <<""/utf8>>,
		count = <<"reply_count"/utf8>>,
		time = <<"created_at"/utf8>>,
		time_type = 5,
		head = [{"Cookie", "xqat=ea139be840cf88ff8c30e6943cf26aba8ad77358"}]
	};

get(50009) ->
	#cfg_news_source{
		source_id = 50009,
		class = 5,
		sub_class = 1,
		name = <<"凤凰财经.热榜"/utf8>>,
		icon_name = <<"ifeng"/utf8>>,
		url = <<"https://finance.ifeng.com/studio"/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"hotNews"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"img"/utf8>>,
		count = <<"num"/utf8>>,
		time = <<"publishTimestamp"/utf8>>,
		time_type = 1,
		json_data = <<"{\"nav\"((.|\n)*?)\"}]}"/utf8>>
	};

get(50010) ->
	#cfg_news_source{
		source_id = 50010,
		class = 5,
		sub_class = 1,
		name = <<"英为财情.热门文章"/utf8>>,
		icon_name = <<"investing"/utf8>>,
		url = <<"https://cn.investing.com/news/most-popular-news"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://cn.investing.com"/utf8>>,
		data = <<"<section[\\s]*id=\"leftColumn\"[\\s]*>([\\s\\S]+?)</section>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title=\"([^\"]+?)\""/utf8>>,
		link_a = <<"<a href=\"([^\"]+?)\" class=\"img\"[\\s]*>"/utf8>>,
		desc = <<"<p>([\\s\\S]+?)</p>"/utf8>>,
		author = <<"<span>([\\s\\S]+?)</span>"/utf8>>,
		img = <<"data-src=\"([^\"]+?)\""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50011) ->
	#cfg_news_source{
		source_id = 50011,
		class = 5,
		sub_class = 1,
		name = <<"英为财情.热门分析"/utf8>>,
		icon_name = <<"investing"/utf8>>,
		url = <<"https://cn.investing.com/analysis/most-popular-analysis"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://cn.investing.com"/utf8>>,
		data = <<"<section[\\s]*id=\"leftColumn\"[\\s]*>([\\s\\S]+?)</section>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title=\"([^\"]+?)\""/utf8>>,
		link_a = <<"<a href=\"([^\"]+?)\" class=\"img\"[\\s]*>"/utf8>>,
		desc = <<"<p>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<"data-src=\"([^\"]+?)\""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50012) ->
	#cfg_news_source{
		source_id = 50012,
		class = 5,
		sub_class = 1,
		name = <<"同花顺.财经要闻"/utf8>>,
		icon_name = <<"10jqka"/utf8>>,
		url = <<"http://news.10jqka.com.cn/today_list/"/utf8>>,
		type = 2,
		is_top = 0,
		link_pre = <<""/utf8>>,
		data = <<"<span[\\s]*class=\"arc-title\">([\\s\\S]+?)</span>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title=\"([^\"]+?)\""/utf8>>,
		link_a = <<"href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(50013) ->
	#cfg_news_source{
		source_id = 50013,
		class = 5,
		sub_class = 1,
		name = <<"同花顺.日榜"/utf8>>,
		icon_name = <<"10jqka"/utf8>>,
		url = <<"http://news.10jqka.com.cn/today_list/"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<ul[\\s]*class=\"item zxrank\">([\\s\\S]+?)</ul>"/utf8>>,
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

get(50014) ->
	#cfg_news_source{
		source_id = 50014,
		class = 2,
		sub_class = 1,
		name = <<"股吧.48H热榜"/utf8>>,
		icon_name = <<"ithome"/utf8>>,
		url = <<"http://guba.eastmoney.com/"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"http://guba.eastmoney.com/"/utf8>>,
		data = <<"48小时股吧热门文章排行</h5>([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<a href[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<a href=\"([^\"]+?)\""/utf8>>,
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
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
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

get(100001) ->
	#cfg_news_source{
		source_id = 100001,
		class = 10,
		sub_class = 4,
		name = <<"新浪体育.热榜"/utf8>>,
		icon_name = <<"sina"/utf8>>,
		url = <<"http://top.sports.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=sports_suda&top_show_num=20&top_order=DESC&js_var=channel_&top_time="/utf8>>,
		type = 4,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<"time"/utf8>>,
		time_type = 4,
		json_data = <<"{\"conf\":((.|\n)*?)}]}"/utf8>>
	};

get(100002) ->
	#cfg_news_source{
		source_id = 100002,
		class = 10,
		sub_class = 4,
		name = <<"梨视频.体育热榜"/utf8>>,
		icon_name = <<"pearvideo"/utf8>>,
		url = <<"https://www.pearvideo.com/popular_loading.jsp?reqType=1&categoryId=9&start=0&sort=0"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://www.pearvideo.com/video_"/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<h2[^>]+?>([\\s\\S]+?)</h2>"/utf8>>,
		link_a = <<"data-id=\"([^\"]+?)\""/utf8>>,
		desc = <<"<p[^>]+?>([\\s\\S]+?)</p>"/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(100003) ->
	#cfg_news_source{
		source_id = 100003,
		class = 10,
		sub_class = 1,
		name = <<"凤凰体育.热榜"/utf8>>,
		icon_name = <<"ifeng"/utf8>>,
		url = <<"http://sports.ifeng.com/c/api/content/graphic/article"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"data"/utf8>>,
		container = <<""/utf8>>,
		title = <<"title"/utf8>>,
		link_a = <<"url"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<"img"/utf8>>,
		count = <<"num"/utf8>>,
		time = <<"publishTimestamp"/utf8>>,
		time_type = 1
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
		link_pre = <<"https://s.weibo.com/weibo?q=%2523"/utf8>>,
		data = <<"<tbody>([\\s\\S]+?)</tbody>"/utf8>>,
		container = <<"<tr.+?class=\"\">\\s+?<td.+?class=\"td-\\d+.+?ranktop\">([\\s\\S]+?)</tr>"/utf8>>,
		title = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<"<span>([\\s\\S]+?)</span>"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110002) ->
	#cfg_news_source{
		source_id = 110002,
		class = 11,
		sub_class = 1,
		name = <<"百度.热榜"/utf8>>,
		icon_name = <<"baidu"/utf8>>,
		url = <<"https://www.baidu.com"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"\"pure_title\":\\s*\"([^\"]+?)\""/utf8>>,
		link_a = <<"\"linkurl\":\\s*\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>,
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<"\"heat_score\": \"([^\"]+?)\""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110003) ->
	#cfg_news_source{
		source_id = 110003,
		class = 11,
		sub_class = 1,
		name = <<"头条搜索.热搜"/utf8>>,
		icon_name = <<"baidu"/utf8>>,
		url = <<"https://ib.snssdk.com/api/suggest_words/?business_id=10017"/utf8>>,
		type = 1,
		is_top = 1,
		link_pre = <<"https://so.toutiao.com/search/?keyword="/utf8>>,
		data = <<"data|words"/utf8>>,
		container = <<""/utf8>>,
		title = <<"word"/utf8>>,
		link_a = <<"word"/utf8>>,
		desc = <<""/utf8>>, 
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<"params|fake_click_cnt"/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110004) ->
	#cfg_news_source{
		source_id = 110004,
		class = 11,
		sub_class = 1,
		name = <<"搜狗.热搜"/utf8>>,
		icon_name = <<"sogou"/utf8>>,
		url = <<"https://top.sogou.com/hot/shishi_1.html"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://so.toutiao.com/search/?keyword="/utf8>>,
		data = <<"<ul class=\"pub-list\">([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<p class=\"[^\"]+?\"><a[^>]+?>([\\s\\S]+?)</a>"/utf8>>,
		link_a = <<"<p class=\"[^\"]+?\"><a href=\"([^\"]+?)\""/utf8>>,
		desc = <<""/utf8>>, 
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110005) ->
	#cfg_news_source{
		source_id = 110005,
		class = 11,
		sub_class = 1,
		name = <<"360搜索.热搜"/utf8>>,
		icon_name = <<"360"/utf8>>,
		url = <<"https://news.so.com/hotnews?src=onebox"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<""/utf8>>,
		data = <<"<ul class=\"[\\s\\S]+?\">([\\s\\S]+?)</ul>"/utf8>>,
		container = <<""/utf8>>,
		title = <<"<span class=\"title\">([\\s\\S]+?)</span>"/utf8>>,
		link_a = <<"href=\"([\\s\\S]+?)\""/utf8>>,
		desc = <<""/utf8>>, 
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
		time = <<""/utf8>>,
		time_type = 0
	};

get(110006) ->
	#cfg_news_source{
		source_id = 110006,
		class = 11,
		sub_class = 1,
		name = <<"神马搜索.热搜"/utf8>>,
		icon_name = <<"360"/utf8>>,
		url = <<"https://m.sm.cn/s?q=神马新闻榜单"/utf8>>,
		type = 2,
		is_top = 1,
		link_pre = <<"https://m.sm.cn/s?q="/utf8>>,
		data = <<""/utf8>>,
		container = <<""/utf8>>,
		title = <<"<span class=\"title-text\">([\\s\\S]+?)</span>"/utf8>>,
		link_a = <<"<span class=\"title-text\">([\\s\\S]+?)</span>"/utf8>>,
		desc = <<""/utf8>>, 
		author = <<""/utf8>>,
		img = <<""/utf8>>,
		count = <<""/utf8>>,
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
		10005,
		10006,
		10007,
		10008,
		10009,
		10010,
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
		20014,
		20015,
		20016,
		20017,
		20018,
		20019,
		20020,
		20021,
		20022,
		20023,
		20024,
		20025,
		20026,
		20027,
		20028,
		20029,
		30001,
		30002,
		30003,
		30004,
		50001,
		50002,
		50003,
		50004,
		50005,
		50006,
		50007,
		50008,
		50009,
		50010,
		50011,
		50012,
		50013,
		50014,
		60001,
		60002,
		100001,
		100002,
		100003,
		110001,
		110002,
		110003,
		110004,
		110005,
		110006
	].
news_source_class(1) ->
	[10002,10003,10004,10005,10006,10007,10008,10009,10010];

news_source_class(2) ->
	[10001,20001,20002,20003,20004,20005,20006,20007,20008,20009,20010,20011,20012,20013,20014,20015,20016,20017,20018,20019,20020,20021,20022,20023,20024,20025,20026,20027,20028,20029];

news_source_class(3) ->
	[30001,30002,30003,30004];

news_source_class(5) ->
	[50001,50002,50003,50004,50005,50006,50007,50008,50009,50010,50011,50012,50013,50014];

news_source_class(6) ->
	[60001,60002];

news_source_class(10) ->
	[100001,100002,100003];

news_source_class(11) ->
	[110001,110002,110003,110004,110005,110006];

news_source_class(_) -> []. 


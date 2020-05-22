%%% ----------------------------------------------------------
%%% 
%%% ----------------------------------------------------------
-module(cfg_news_class).
-include("common.hrl").
-export([get/1, list_key/0]).
-include("cfg_news_class.hrl").

get(1) ->
	#cfg_news_class{
		id = 1,
		sort = 1,
		name = <<"时事.新闻"/utf8>>
	};

get(2) ->
	#cfg_news_class{
		id = 2,
		sort = 2,
		name = <<"科技.互联网"/utf8>>
	};

get(3) ->
	#cfg_news_class{
		id = 3,
		sort = 3,
		name = <<"娱乐八卦"/utf8>>
	};

get(4) ->
	#cfg_news_class{
		id = 4,
		sort = 4,
		name = <<"汽车"/utf8>>
	};

get(5) ->
	#cfg_news_class{
		id = 5,
		sort = 5,
		name = <<"财经"/utf8>>
	};

get(6) ->
	#cfg_news_class{
		id = 6,
		sort = 6,
		name = <<"社区.博客"/utf8>>
	};

get(7) ->
	#cfg_news_class{
		id = 7,
		sort = 7,
		name = <<"知识.阅读"/utf8>>
	};

get(8) ->
	#cfg_news_class{
		id = 8,
		sort = 8,
		name = <<"购物"/utf8>>
	};

get(9) ->
	#cfg_news_class{
		id = 9,
		sort = 9,
		name = <<"影音.小说"/utf8>>
	};

get(10) ->
	#cfg_news_class{
		id = 10,
		sort = 10,
		name = <<"体育"/utf8>>
	};

get(11) ->
	#cfg_news_class{
		id = 11,
		sort = 11,
		name = <<"热搜"/utf8>>
	};

get(_Id) -> 
	false.

list_key() ->
	[
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		10,
		11
	].

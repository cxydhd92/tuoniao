-record(cfg_news_source, {
	'source_id',
	'class',
	'sub_class',
	'name',
	'summry',
	'url',
	'url_type' = 0,
	'type',
	'is_top',
	'link_pre' = <<"">>,
	'data' = <<"">>,
	'container' = <<"">>,
	'title' = <<"">>,
	'link_a' = <<"">>,
	'desc' = <<"">>,
	'author' = <<"">>,
	'img' = <<"">>,
	'count' = <<"">>,
	'time' = <<"">>,
	'time_type' = 1,
	'json_data' = <<"">>,
	'head' = [],
	'check_num' = 50  %% 检测重复标题所保存的最新数量
	,'fake_id' = <<"">> %% 公众号标识
}).



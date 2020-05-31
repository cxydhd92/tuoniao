%% 每日热点
-define(LIST_END_TIME, 48*86400). %% 列表最多保持最近3天

-define(ETS_TODAYHOT, todayhot_ets).
-define(ETS_TODAYHOT_NEWS, todayhot_news_ets). %%资讯数据
-define(ETS_TODAYHOT_TODAY, todayhot_today_ets). %%当天数据
-define(ETS_TODAYHOT_HOTLIST, todayhot_hot_list_ets). %%当天最新榜单数据

-define(REMOVE_CHAR, [$<, $>, $s, $p, $a, $n, $/, $b, $l, $u, $e,$", $c,$l]).

-record(todayhot, {
    compre = []  %% 综合新闻
    ,tech = []   %% 科技新闻
    ,recreation = []  %% 娱乐
    ,videos = []      %% 视频
    ,time = 0     %% 添加时间
}).

-record(todayhot_nodes, {
    node_id = 0 
    ,node_name    %% 节点名
    ,count = 0    %% 订阅数
    ,add_time = 0    
    ,users = []    %% 所属用户列表
    ,time_list = []  %% 时间戳列表
}).

-record(todayhot_node_news, {
    node = {0, 0}      %% {node_id, timezero}
    ,news = []    %% 新闻列表 [#todayhot_news{}]
    ,up_time   
}).

-record(todayhot_nodes_hotlist, {
    node = {0, 0}      %% {node_id, timezero}
    ,news = []         %% 最新榜单数据  
}).

-record(todayhot_news, {
    id = 1    
    ,class = 1
    ,node_id = 1 
    ,abstract = <<"">>
    ,title
    ,url 
    ,source  = <<"">>
    ,count = <<"">>
    ,sub_news = []   %% [news_id]
    ,same_id = 0    %% 相似标题对于新闻id
    ,img = <<"">>
    ,news_time = 0 %% 新闻时间
    ,time = 0    %% 抓取时间
}).
-record(todayhot_sub_news, {
    title
    ,url 
    ,source  = <<"">>
    ,count = <<"">>
    ,time = 0
}).

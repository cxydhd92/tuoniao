%% 每日热点
-define(LIST_END_TIME, 48*86400). %% 列表最多保持最近3天

-define(ETS_TODAYHOT, todayhot_ets).
-define(ETS_TODAYHOT_NEWS, todayhot_news_ets). %%资讯数据
-define(ETS_TODAYHOT_TODAY, todayhot_today_ets). %%当天数据
-define(ETS_TODAYHOT_HOTLIST, todayhot_hot_list_ets). %%当天最新榜单数据
-define(ETS_TODAYHOT_USER_SESSION, todayhot_use_session_ets). %% 用户session数据
-define(ETS_TODAYHOT_USER, todayhot_use_ets). %% 用户数据
-define(ETS_CFG_NODE, ets_cfg_node). %% 节点配置
-define(ETS_CFG_CLASS, ets_cfg_class). %% 分类配置

-define(REMOVE_CHAR, [$<, $>, $s, $p, $a, $n, $/, $b, $l, $u, $e,$", $c,$l]).

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

%% 用户相关结构
-define(todayhot_user_ver, 1).
-record(todayhot_user, {
    ver = ?todayhot_user_ver
    ,account       %% 用户id
    ,name = <<"">>
    ,password
    ,source_list = [] %% 订阅源列表
}).

%% 用户订阅源
-define(todayhot_user_source, 1).
-record(todayhot_user_source, {
    ver = ?todayhot_user_source
    , id = 1       %% 源id
    , filters = [] %% 过滤关键词列表
    , tracks = []  %% 追踪关键词列表
    , time = 0     %% 添加时间
}).

-define(todayhot_user_session_ver, 1).
-record(todayhot_user_session, {
    ver = ?todayhot_user_session_ver
    ,session = 1      %% 
    ,account = 0     
    ,time = 0            %% 到期时间
}).

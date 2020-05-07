%% 每日热点
-define(LIST_END_TIME, 48*86400). %% 列表最多保持最近3天

%% 节点分类
-define(todayhot_class_compre, 1).     %% 综合
-define(todayhot_class_tech, 2).       %% 科技
-define(todayhot_class_answer, 3).     %% 问答
-define(todayhot_class_hotso, 4).      %% 热搜热词
-define(todayhot_class_yl, 5).         %% 娱乐
-define(todayhot_class_qc, 6).         %% 汽车
-define(todayhot_class_gx, 7).         %% 搞笑
-define(todayhot_class_rb, 8).         %% 日报周刊

-define(todayhot_class_list,[
    ?todayhot_class_compre
    ,?todayhot_class_tech
    ,?todayhot_class_hotso
    ,?todayhot_class_answer
    ,?todayhot_class_yl
    ,?todayhot_class_qc
    ,?todayhot_class_gx
    ,?todayhot_class_rb
]).

%% 热搜/热词
-define(todayhot_node_weibo , 40001).     %% 微博热搜榜

%% 综合
-define(todayhot_node_qdaily , 10001).      %% 好奇心
-define(todayhot_node_paper , 10010).       %% 澎湃新闻热闻
-define(todayhot_node_toutiao , 10030).     %% 头条热榜
-define(todayhot_node_163news , 10040).     %% 网易新闻24小时热榜
-define(todayhot_node_163pic , 10041).      %% 网易图集24小时热榜

%% 问答
-define(todayhot_node_zhihu , 30001).       %% 知乎热榜
%% 日报周刊
-define(todayhot_node_zhihu_daily , 80001). %% 知乎日报

-define(todayhot_compre_notes, [
    ?todayhot_node_qdaily
    ,?todayhot_node_paper
    ,?todayhot_node_zhihu
    ,?todayhot_node_zhihu_daily
    ,?todayhot_node_toutiao
    ,?todayhot_node_163news
    ,?todayhot_node_163pic
]).

%% 科技
-define(todayhot_node_readhub , 20001).     %% 
-define(todayhot_node_36kr_1 , 20010).     %% 36kr快讯
-define(todayhot_node_36kr_2 , 20011).     %% 36kr24小时热榜
-define(todayhot_node_qqtech, 20020).     %% 腾讯科技
-define(todayhot_node_huxiu , 20030).     %% 虎嗅热榜
-define(todayhot_node_donews , 20040).     %% donews热榜
-define(todayhot_node_163tech , 20050).     %% 网易科技排行
-define(todayhot_node_tmt , 20060).     %% 钛媒体
-define(todayhot_node_tmt_hot , 20061).     %% 钛媒体热门新闻


-define(todayhot_tech_notes, [
    ?todayhot_node_readhub,
    ?todayhot_node_36kr_1,
    ?todayhot_node_36kr_2,
    ?todayhot_node_qqtech,
    ?todayhot_node_huxiu,
    ?todayhot_node_donews
    ,?todayhot_node_163tech
    ,?todayhot_node_tmt
    ,?todayhot_node_tmt_hot
]).

-define(ETS_TODAYHOT, todayhot_ets).
-define(ETS_TODAYHOT_TODAY, todayhot_today_ets). %%当天数据
-define(ETS_TODAYHOT_HOTLIST, todayhot_hot_list_ets). %%当天最新榜单数据

-record(todayhot, {
    compre = []  %% 综合新闻
    ,tech = []   %% 科技新闻
    ,recreation = []  %% 娱乐
    ,videos = []      %% 视频
    ,time = 0     %% 添加时间
}).



-record(todayhot_nodes, {
    ,node_id = 0 
    ,node_name    %% 节点名
    ,count = 0    %% 订阅数
    ,add_time = 0    
    ,users = []    %% 所属用户列表
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

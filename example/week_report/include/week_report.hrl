%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个



%% 经过初步处理的文件中的数据(通常是csv文件中间行列的内容)
-record(data_tuple, 
        {zh_newsjyh_lastday_datas :: [term()],  % [#zh_newsjyh_lastday_data{}],
         zh_sjyh_lastday_datas :: [term()],     %[#zh_sjyh_lastday_data{}], 
         zh_sjyh_lastday_excel_datas :: [term()]% [#zh_sjyh_lastday_excel_data{}]
        }).


%% zh_newsjyh_lastday*文件中得到的数据
-record(zh_newsjyh_lastday_data,
        {date :: calendar:date(),                  % 日期
         cpu_datas :: [{server1_id(), value()}],    % cpu使用率列表源数据
         mem_datas :: [{server1_id(), value()}]     % 内存使用率列表源数据
        }).
-type server1_id() :: string().                     % 机器编号"sjyhapp13" | "sjyhapp12" | "sjyhapp11" | "sjyhwebjr12" | "sjyhwebjr11" | "sjyhmg2" | "sjyhmg1" | "sjyhcj2" | "sjyhcj1" | "sjyhpic2" | "sjyhpic1" | "sjyhmem4" | "sjyhmem3" | "sjyhmem2" | "sjyhmem1" | "sjyhapp10" | "sjyhapp9" | "sjyhapp8" | "sjyhapp7" | "sjyhapp6" | "sjyhapp2" | "sjyhwebjr8" | "sjyhapp5" | "sjyhapp1" | "sjyhwebjr7" | "sjyhapp4" | "sjyhwebjr10" | "sjyhwebjr6" | "sjyhapp3" | "sjyhwebjr9" | "sjyhwebjr5" | "sjyhwebjr4" | "sjyhwebjr3" | "sjyhwebjr2" | "sjyhwebjr1" 


%% zh_sjyh_lastday*文件中得到的数据
-record(zh_sjyh_lastday_data,
        {date :: calendar:date(),                   % 日期
         datas :: [{trans_login_id(), value()}]     % 数据列表
        }).
-type trans_login_id() :: string().                 % 编号"金融交易数"|"非金融交易数"|"登陆用户数"|"金融交易用户数"|"新增用户数"


%% zh_sjyh_lastday_excel*文件中得到的数据
-record(zh_sjyh_lastday_excel_data,
        {date :: calendar:date(),                   % 日期
         cpu_datas :: [{server2_id(), value()}],    % cpu使用率列表源数据
         mem_datas :: [{server2_id(), value()}],    % 内存使用率列表源数据
         ts_datas :: [{ts_id(), value()}]           % 表空间使用情况列表源数据
        }).
-type server2_id() :: string().                     % 机器编号"sjdb1" | "sjdb2" | "sjlog" | "sjmg1" | "sjmg2"
-type ts_id() :: string().                          % 表空间编号"HXLOG" | "INDEX_LOG" | "INDEX_MOBILE" | "HXMOBILE"
-type value() :: string().                          % 数值



%% 得到编号相关数据(通常是csv文件的第一行内容)

% web接入机器编号列表
-define(WEB_IDS, ["sjyhwebjr1", "sjyhwebjr2", "sjyhwebjr3", "sjyhwebjr4", 
                    "sjyhwebjr5", "sjyhwebjr6", "sjyhwebjr7", "sjyhwebjr8", 
                    "sjyhwebjr9", "sjyhwebjr10", "sjyhwebjr11", "sjyhwebjr12"]).

% 应用机器编号列表
-define(APP_IDS, ["sjyhapp1", "sjyhapp2", "sjyhapp3", "sjyhapp4", 
                    "sjyhapp5", "sjyhapp6", "sjyhapp7", "sjyhapp8", 
                    "sjyhapp9", "sjyhapp10", "sjyhapp11", "sjyhapp12", 
                    "sjyhapp13", "sjyhapp14"]).

% 数据库机器编号列表
-define(DB_IDS, ["sjdb1", "sjdb2"]). 


% 其余的机器编号列表
-define(THE_REST_IDS, ["sjlog", "sjmg1", "sjmg2", "sjyhmem1", "sjyhmem2", 
                      "sjyhmem3", "sjyhmem4", "sjyhpic1", "sjyhpic2", 
                      "sjyhcj1", "sjyhcj2", "sjyhmg1", "sjyhmg2"]).

% 其他的机器编号列表
-define(OTHER_IDS, ?DB_IDS ++ ?THE_REST_IDS).

% 表空间标号列表
-define(TS_IDS, ["HXMOBILE", "HXLOG", "INDEX_MOBILE", "INDEX_LOG"]).

% 交易曲线标号列表
-define(TRAN_IDS, ["金融交易数", "非金融交易数", "交易总数"]).

% 登陆数标号列表
-define(LOGIN_IDS, ["登陆用户数", "金融交易用户数", "新增用户数"]).



%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     程序负责
%%%         生成周报
%%% @end
%%%-------------------------------------------------------------------
-module(week_report).

-export([run/1, run/2]).

-include("week_report.hrl").

%% 调试用
%-compile(export_all).


%%--------------------------------------------------------------------
%% @doc
%% 运行函数
%% @end
-spec run(Params) -> ok when
    Params :: list().   % 形如,[week_report|month_report, In_dir :: atom(), Out_dir :: atom()]
%%--------------------------------------------------------------------
run([Report_flag, In_dir, Out_dir]) ->
    case file:consult("example/week_report/config/week_report.config") of
        {ok, Conf1} ->
            Conf2 = update_in_out_dir(In_dir, Out_dir, Conf1),
            run(Report_flag, Conf2);
        {Error, Reason} ->
            throw({Error, "example/week_report/config/week_report.config", Reason}) 
    end.





%%--------------------------------------------------------------------
%% @doc
%% 手动调用的运行函数
%% @end
-spec run(Report_flag, Conf) -> ok when
    Report_flag :: week_report|month_report,        % 报告标志
    Conf:: list().                                  % 配置
%%--------------------------------------------------------------------
run(Report_flag, Conf) ->
    Data_tuple = get_data_tuple(Conf),                                  % 得到经过初步处理的文件中的数据(通常是csv文件中间行列的内容)
    Dates = get_dates(Data_tuple),                                      % 得到日期列表(通常是csv文件的第一列内容)
    Matrixe_tuple = get_matrixe_tuple(Conf, Dates, Data_tuple),         % 得到经过初步处理的文件中的数据对应的矩阵(以方便计算)
    ok = generate_report_csv(Report_flag, Conf, Dates, Matrixe_tuple),  % 生成报告csv文件
    io:format("report finish !!~n"),                                    % 打印成功
    io:format("in_dir is ~p~n",[proplists:get_value(in_dir, Conf)]),
    io:format("out_dir is ~p~n",[proplists:get_value(out_dir, Conf)]),
    ok.


% 代替配置中的in_dir,out_dir
update_in_out_dir(undefined, undefined, Conf1) ->
    Conf1;
update_in_out_dir(In_dir, Out_dir, Conf1) ->
    In_dir1 = atom_to_list(In_dir),
    Out_dir1 = atom_to_list(Out_dir),
    Conf2 = proplists:delete(out_dir, proplists:delete(in_dir, Conf1)),
    [{in_dir, In_dir1}, {out_dir, Out_dir1} | Conf2].

    
%% 得到经过初步处理的文件中的数据(通常是csv文件中间行列的内容)
get_data_tuple(Conf) ->
    #data_tuple{zh_newsjyh_lastday_datas = zh_newsjyh_lastday:get_datas(Conf), 
                zh_sjyh_lastday_datas = zh_sjyh_lastday:get_datas(Conf), 
                zh_sjyh_lastday_excel_datas = zh_sjyh_lastday_excel:get_datas(Conf)
               }.


%% 得到日期列表(通常是csv文件的第一列内容)
get_dates(#data_tuple{zh_newsjyh_lastday_datas = ZNL_dates,
                      zh_sjyh_lastday_datas = ZSL_dates,
                      zh_sjyh_lastday_excel_datas = ZSLE_dates}) ->   
    Dates1 = lists:sort([D#zh_newsjyh_lastday_data.date || D <- ZNL_dates]),    % 得到日期列表
    Dates2 = lists:sort([D#zh_sjyh_lastday_data.date || D <- ZSL_dates]),       % 得到日期列表
    Dates3 = lists:sort([D#zh_sjyh_lastday_excel_data.date || D <- ZSLE_dates]),% 得到日期列表
    Dates = Dates1 = Dates2 = Dates3,                                           % 核对这3个排序的日期列表应该一样
    Dates.                                                                      % 返回


%% 得到经过初步处理的文件中的数据对应的矩阵
get_matrixe_tuple(Conf, Dates,
                  #data_tuple{zh_newsjyh_lastday_datas = ZNL_datas,
                              zh_sjyh_lastday_excel_datas = ZSLE_datas,
                              zh_sjyh_lastday_datas = ZSL_datas}) ->
    Cpu_matrix = get_cpu_matrix(ZNL_datas, ZSLE_datas),                         % 得到cpu使用率的矩阵
    Mem_matrix = get_mem_matrix(ZNL_datas, ZSLE_datas),                         % 得到mem使用率的矩阵
    Ts_matrix = get_ts_matrix(ZSLE_datas),                                      % 得到表空间使用率的矩阵
    Tran_and_login_matrix = get_tran_and_login_matrix(ZSL_datas, Dates),        % 得到交易曲线和登陆数的矩阵
    File_sys_matrix = get_input_matrix(Conf, "web_file_sys_matrix_input.data")
                      ++ get_input_matrix(Conf, "app_file_sys_matrix_input.data")
                      ++ get_input_matrix(Conf, "other_file_sys_matrix_input.data"),  % 得到文件系统使用率的矩阵
    {Cpu_matrix, Mem_matrix, Ts_matrix, Tran_and_login_matrix, File_sys_matrix}.


%% 生成报告csv文件
generate_report_csv(week_report, Conf, Dates, 
                    {Cpu_matrix, Mem_matrix, Ts_matrix, 
                     _Tran_and_login_matrix, File_sys_matrix}) ->   % 生成周报告csv文件
    In_dir = proplists:get_value(in_dir, Conf),                         % 得到输入目录
    Out_dir = proplists:get_value(out_dir, Conf),                       % 得到输出目录
    case  file:consult(filename:absname_join(In_dir, "week_report_input.data")) of
        {ok, Input} ->
            Data1 = get_week_data(web_report, Conf, Input, ?WEB_IDS, Dates, 
                                  [Cpu_matrix, Mem_matrix, File_sys_matrix]),   % 周报"手机银行金融前置"csv数据
            Data2 = get_week_data(app_report, Conf, Input, ?APP_IDS, Dates, 
                                  [Cpu_matrix, Mem_matrix, File_sys_matrix]),   % 周报"手机银行金融应用"csv数据
            Data3 = get_week_data(the_rest_report, Conf, Input, ?THE_REST_IDS, Dates, 
                                  [Cpu_matrix, Mem_matrix, File_sys_matrix]),   % 周报""csv数据
            Data4 = get_week_data(db_report, Conf, Input, ?DB_IDS, Dates, 
                                  [Cpu_matrix, Mem_matrix, File_sys_matrix]),   % 周报""csv数据
            Data5 = get_week_data(ts_report, Conf, Input, ?TS_IDS, Dates, 
                                  [Ts_matrix]),                                 % 周报""csv数据
            Data6 = get_week_data(record_report, Conf, Input, [], [], []),      % 周报""csv数据
            Blank = get_week_data_blank(),                                      % 周报csv中的空行
            Iodata = Data1 ++ Blank ++ Data2 ++ Blank ++ Data3 ++ Blank 
                     ++ Data4 ++ Blank ++ Data5 ++ Blank ++ Data6,                % 合成最终的csv数据 
            ok = file:write_file(filename:absname_join(Out_dir, "week_report.csv"), Iodata);
        {Error, Reason} ->
            throw({Error, filename:absname_join(In_dir, "week_report_input.data"), Reason}) 
    end;
generate_report_csv(month_report, Conf, Dates,
                    {_Cpu_matrix, _Mem_matrix, Ts_matrix, 
                     Tran_and_login_matrix, File_sys_matrix} = Matrixe_tuple) ->    % 生成月报告csv文件 
    ok = generate_matrix_csv(Conf, Dates, Matrixe_tuple),                               % 生成矩阵对应csv文件
    In_dir = proplists:get_value(in_dir, Conf),                                         % 得到输入目录
    Out_dir = proplists:get_value(out_dir, Conf),                                       % 得到输出目录
    {ok, Template} = file:read_file(
            filename:absname_join(In_dir, "month_report_template.txt")),                % 得到月报模板
    Data1 = get_moth_tran_data(Dates, Tran_and_login_matrix),       % 得到交易量峰值(多少万),日期,周几
    Data2 = get_moth_login_data(Dates, Tran_and_login_matrix),      % 得到客户登录峰值(多少万),日期,周几,金融交易客户数占登录客户数百分比
    Data3 = get_file_sys_data(Dates, File_sys_matrix),              % 得到主机日志空间和日志服务器使用率
    Data4 = get_ts_data(Dates, Ts_matrix),                          % 得到数据库表空间 HXLOG,HXMOBILE,INDEX_MOBILE,INDEX_LOG使用率和周增长率
    Data = Data1 ++ Data2 ++ Data3 ++ Data4,
    Iodata = io_lib:format(Template, Data),                                             % 填充数据
    ok = file:write_file(filename:absname_join(Out_dir, "month_report.txt"), Iodata),
    ok.


%% 得到cpu使用率的矩阵
get_cpu_matrix(ZNL_datas, ZSLE_datas) ->
    Matrix1 = simple_matrix:new(),                      % 创建一个矩阵
    Fun1 =
        fun(#zh_newsjyh_lastday_data{date = Date1, cpu_datas = Datas1}, 
            Matrix_in1) -> 
            _Matrix_out1 = put_matrix(Date1, Datas1, Matrix_in1)
        end,
    Matrix2 = lists:foldl(Fun1, Matrix1, ZNL_datas),    % 把数据全放入矩阵
    Fun2 =
        fun(#zh_sjyh_lastday_excel_data{date = Date2, cpu_datas = Datas2}, 
            Matrix_in2) -> 
            _Matrix_out2 = put_matrix(Date2, Datas2, Matrix_in2)
        end,
    Matrix3 = lists:foldl(Fun2, Matrix2, ZSLE_datas),   % 把数据全放入矩阵
    Matrix3.


%% 得到mem使用率的矩阵
get_mem_matrix(ZNL_datas, ZSLE_datas) ->
    Matrix1 = simple_matrix:new(),                      % 创建一个矩阵
    Fun1 =
        fun(#zh_newsjyh_lastday_data{date = Date1, mem_datas = Datas1}, 
            Matrix_in1) -> 
            _Matrix_out1 = put_matrix(Date1, Datas1, Matrix_in1)
        end,
    Matrix2 = lists:foldl(Fun1, Matrix1, ZNL_datas),    % 把数据全放入矩阵
    Fun2 =
        fun(#zh_sjyh_lastday_excel_data{date = Date2, mem_datas = Datas2}, 
            Matrix_in2) -> 
            _Matrix_out2 = put_matrix(Date2, Datas2, Matrix_in2)
        end,
    Matrix3 = lists:foldl(Fun2, Matrix2, ZSLE_datas),   % 把数据全放入矩阵
    Matrix3.


%% 得到表空间使用率的矩阵
get_ts_matrix(ZSLE_datas) ->
    Matrix1 = simple_matrix:new(),                      % 创建一个矩阵
    Fun =
        fun(#zh_sjyh_lastday_excel_data{date = Date, ts_datas= Datas}, 
            Matrix_in) -> 
            _Matrix_out = put_matrix(Date, Datas, Matrix_in)
        end,
    Matrix2 = lists:foldl(Fun, Matrix1, ZSLE_datas),    % 把数据全放入矩阵
    Matrix2.


%% 得到交易曲线和登陆数的矩阵
get_tran_and_login_matrix(ZSL_datas, Dates) ->
    Matrix1 = simple_matrix:new(),                      % 创建一个矩阵
    Fun =
        fun(#zh_sjyh_lastday_data{date = Date, datas= Datas}, 
            Matrix_in) -> 
            _Matrix_out = put_matrix(Date, Datas, Matrix_in)
        end,
    Matrix2 = lists:foldl(Fun, Matrix1, ZSL_datas),     % 把数据全放入矩阵
    Clumns_fun = 
        fun(V1, V2) -> 
            integer_to_list(simple_matrix:to_number(V1) + simple_matrix:to_number(V2)) 
        end,
    _Matrix3 = simple_matrix:clumns_fun(["金融交易数", "非金融交易数"], "交易总数", 
                                        Dates, Clumns_fun, Matrix2). % 把"金融交易数", "非金融交易数"相加更新到"交易总数"


%% 周报"手机银行金融前置"csv数据
get_week_data(Name, Conf, Input, Ids, Dates, Max_ms) ->
    {_, Head, Clumns, Rows, V_list} = lists:keyfind(Name, 1, Conf),         % 得到配置中的矩阵值
    Matrix1 = simple_matrix:values_list_to_matrix(Clumns, Rows, V_list),    % 转成矩阵以便计算
    Matrix2 = week_report_put(Name, Input, Ids, Dates, Max_ms, Matrix1),    % 填写矩阵的"巡检指标"和说明
    _Csv_data = get_week_csv_data(Head, Clumns, Rows, Matrix2).


%% 周报csv中的空行数据
get_week_data_blank() ->
    "\n".

%% 填写矩阵的"巡检指标"
week_report_put(web_report = Name, Input, Ids, Dates, Max_ms, Matrix1) ->
    Matrix2 = week_report_put_CMF("logs", Ids, Dates, Max_ms, Matrix1),
    _Matrix3 = week_report_put_swap(Name, Input, Matrix2);
week_report_put(app_report = Name, Input, Ids, Dates, Max_ms, Matrix1) ->
    Matrix2 = week_report_put_CMF("logs", Ids, Dates, Max_ms, Matrix1),
    Matrix3 = week_report_put_swap(Name, Input, Matrix2),
    _Matrix4 = week_report_put_conn_count(Input, Matrix3);
week_report_put(the_rest_report = Name, Input, Ids, Dates, Max_ms, Matrix1) ->
    Matrix2 = week_report_put_CMF("logs", Ids, Dates, Max_ms, Matrix1),
    _Matrix3 = week_report_put_swap(Name, Input, Matrix2);
week_report_put(db_report = Name, Input, Ids, Dates, Max_ms, Matrix1) ->
    Matrix2 = week_report_put_CMF("oracle", Ids, Dates, Max_ms, Matrix1),
    _Matrix3 = week_report_put_swap(Name, Input, Matrix2);
week_report_put(ts_report = _Name, _Input, _Ids, Dates, [Ts_matrix], Matrix1) ->
    _Matrix2 = week_report_put_by_max(["HXLOG"], Dates, {"HXLOG", Ts_matrix}, 
                                      Matrix1);
week_report_put(record_report = _Name, Input, _, _, _, Matrix1) ->
    Clumns = ["记录数"],                                                            % 要填写的列编号
    Rows = ["HXMOBILE.MA_POS_MERCHANT","HXMOBILE.MA_SMAG", 
                "HXMOBILE.MY_OPER_LOGS_HISTORY","HXMOBILE.MY_TRAN_LOGS_HISTORY"],   % 要填写的行编号
    V_list = [[V] || V <- proplists:get_value(record_report, Input)],               % 要填写的值列表
    _Matrix2 = simple_matrix:put_values_list(Clumns, Rows, V_list, Matrix1).        % 更新矩阵 


%% 填写"CPU使用率","内存使用率", "文件空间使用率/XX"
week_report_put_CMF(File_sys, Ids, Dates, [Cpu_matrix, Mem_matrix, File_sys_matrix], 
                    Matrix1) ->
    RMs = [{"CPU使用率", Cpu_matrix}, 
           {"内存使用率", Mem_matrix}, 
           {"文件空间使用率/" ++ File_sys, File_sys_matrix}],       % [{行编号, 对应的矩阵}]
    Fun = fun(RM, M_in) -> week_report_put_by_max(Ids, Dates, RM, M_in) end,
    _Matrix2 = lists:foldl(Fun, Matrix1, RMs).                      % 依次根据行编号对应的矩阵的最大值填写"巡检指标"和"说明"


%% 填写"交换空间使用率"
week_report_put_swap(Name, Input, Matrix1) ->
    Clumns = ["巡检指标", "说明"],                                              % 要填写的列编号
    Rows = [Row = "交换空间使用率"],                                            % 要填写的行编号
    MaxV = proplists:get_value(Name, proplists:get_value(swap, Input)),         % 得到输入的值
    Warn = get_warn("预警指标", Row, Matrix1),                              % 得到"预警指标"中的数值
    V_list =                                                                    % 要填进"巡检指标"的内容
        case MaxV < Warn of                 % 判断最大值是否小于预警
            true ->                         % 如果是则"说明"填正常
                [[lists:concat([MaxV, "%"]), "正常"]];
            false ->                        % 如果否则"说明"填异常
                [[lists:concat([MaxV, "%"]), "异常"]]
        end,
    _Matrix2 = simple_matrix:put_values_list(Clumns, Rows, V_list, Matrix1).    % 更新矩阵 


%% 填写"应用服务器有效连接数"
week_report_put_conn_count(Input, Matrix1) ->
    Clumns = ["巡检指标", "说明"],                                              % 要填写的列编号
    Rows = ["应用服务器有效连接数"],                                            % 要填写的行编号
    {MinV, MaxV} = proplists:get_value(app_conn_count, Input),                  % 得到输入的值
    V_list = [[lists:concat([MinV, "-", MaxV]), "正常"]],                       % 要填进"巡检指标"的内容
    _Matrix2 = simple_matrix:put_values_list(Clumns, Rows, V_list, Matrix1).    % 更新矩阵 


%% 根据行编号对应的矩阵的最大值填写"巡检指标"和"说明"
week_report_put_by_max(Ids, Dates, {R, M}, M_in) ->
    Warn = get_warn("预警指标", R, M_in),                                       % 得到"预警指标"中的数值
    [Xj, Sm] = get_xj_sm(Warn, Ids, Dates, M),                                  % 得到"巡检指标"和"说明"
    _M_out = simple_matrix:put_values_list(["巡检指标", "说明"], [R], [[Xj, Sm]], M_in). % 更新矩阵


%% 得到"巡检指标"和"说明"
get_xj_sm(Warn, Ids, Dates, M) ->
    [{Id, Date, MaxV}|_] = simple_matrix:get_number_max(Ids, Dates, M), % 得到行对应矩阵中最大值的元素
    case simple_matrix:to_number(MaxV) < Warn of                        % 判断最大值是否小于预警
        true ->                         % 如果是则"说明"填正常
            [lists:concat([MaxV, "%"]), "正常"];
        false ->                        % 如果否则"说明"填异常并指出异常日期和机器编号
            [lists:concat([MaxV, "%"]), lists:concat(["异常:", Date, ":", Id])]
    end.


%% 得到矩阵中制定元素的数值中的数值
get_warn(C, R, M_in) ->
    Warn_str = simple_matrix:get_elem_value(C, R, M_in),                            % 得到制定元素
    {match,[Warn|_]} = re:run(Warn_str, "\\d+\\.\\d+|\\d+",[{capture,all,list}]),   % 找到其中的整数或浮点数
    simple_matrix:to_number(Warn).                                                  % 转成数字


%% 得到周报的csv数据
get_week_csv_data(Head, Clumns, Rows, Matrix) ->
    V_list1 = simple_matrix:matrix_to_values_list(Clumns, Rows, Matrix),% 得到数值列表
    V_list2 = simple_matrix:values_list_add_clums_rows(
                Head, Clumns, Rows, V_list1),                           % 把矩阵对应的值列表加上行列(以方便转成csv)
    _Csv_data = csv_util:format(V_list2).                               % 生成写入csv文件的内容(即加逗号以分隔格子内容,并再每行加/n)
 

%% 把Datas中的元素组装成矩阵元素放入矩阵
put_matrix(Row, Datas, Matrix_in) ->
    Fun =
        fun({Clumn, Value}, M_in) ->
            _M_out = simple_matrix:put_elem({Clumn, Row, Value}, M_in)
        end,
    _Matrix_out = lists:foldl(Fun, Matrix_in, Datas).


%% 得到输入文件中的矩阵
get_input_matrix(Conf, Filename) ->
    In_dir = proplists:get_value(in_dir, Conf),                                                 % 得到输入目录
    case file:consult(filename:absname_join(In_dir, Filename)) of                               % 读出数据
        {ok, [{_, Clumns, Rows, V_list}]} ->
            _Matrix1 = simple_matrix:values_list_to_matrix(Clumns, Rows, V_list);               % 转成矩阵以便计算
        {Error, Reason} ->
            throw({Error, filename:absname_join(In_dir, Filename), Reason}) 
    end.


%% 生成矩阵对应csv文件
generate_matrix_csv(Conf, Dates, 
                    {Cpu_matrix, Mem_matrix, Ts_matrix, Tran_and_login_matrix, 
                     File_sys_matrix}) ->
    Out_dir = proplists:get_value(out_dir, Conf),                       % 得到输出目录
    % 生成web接入机器
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "web_cpu.csv"), 
                                    ?WEB_IDS, Dates, Cpu_matrix),     % cpu使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "web_mem.csv"), 
                                    ?WEB_IDS, Dates, Mem_matrix),     % mem使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "web_file_sys.csv"), 
                                    ?WEB_IDS, Dates, File_sys_matrix), % 文件系统使用率的矩阵相关csv文件
    % 生成应用机器
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "app_cpu.csv"), 
                                    ?APP_IDS, Dates, Cpu_matrix),     % cpu使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "app_mem.csv"), 
                                    ?APP_IDS, Dates, Mem_matrix),     % mem使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "app_file_sys.csv"), 
                                    ?APP_IDS, Dates, File_sys_matrix), % 文件系统使用率的矩阵相关csv文件
    % 生成其他机器
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "other_cpu.csv"), 
                                    ?OTHER_IDS, Dates, Cpu_matrix), % cpu使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "other_mem.csv"), 
                                    ?OTHER_IDS, Dates, Mem_matrix), % mem使用率的矩阵相关csv文件
    % 
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "ts.csv"), 
                                    ?TS_IDS, Dates, Ts_matrix),                     % 生成表空间使用率的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "tran.csv"), 
                                    ?TRAN_IDS, Dates, Tran_and_login_matrix),     % 生成交易曲线的矩阵相关csv文件
    ok = simple_matrix:generate_csv(filename:absname_join(Out_dir, "login.csv"), 
                                    ?LOGIN_IDS, Dates, Tran_and_login_matrix),   % 生成登陆数的矩阵相关csv文件
    ok.


%% 得到交易量峰值(多少万),日期,周几
get_moth_tran_data(Dates, Tran_and_login_matrix) ->
    _Vale_md_day = get_value_md_day("交易总数", Dates, Tran_and_login_matrix).


%% 得到客户登录峰值(多少万),日期,周几,金融交易客户数占登录客户数百分比
get_moth_login_data(Dates, Tran_and_login_matrix) ->
    Vale_md_day = get_value_md_day("登陆用户数", Dates, Tran_and_login_matrix), % 得到客户登录峰值(多少万),日期,周几
    Percentage = get_moth_login_percentage(Dates, Tran_and_login_matrix),       % 得到金融交易客户数占登录客户数百分比
    Vale_md_day ++ [Percentage].


%% 得到主机日志空间和日志服务器使用率
get_file_sys_data(Dates, File_sys_matrix) ->
    [get_web_app_percentage(Dates, File_sys_matrix),        % 得到web和app日志空间使用率的的平均值
     get_log_percentage(Dates, File_sys_matrix)].           % 得到日志服务器日志空间使用率的的平均值


%% 得到数据库表空间 HXLOG,HXMOBILE,INDEX_MOBILE,INDEX_LOG使用率和周增长率
get_ts_data(Dates, Ts_matrix) ->
    [A, B, C, D] = [get_ts_growth_rate(Id, Dates, Ts_matrix) || Id <- ?TS_IDS], % 得到编号对应的用率和周增长率
    A ++ B ++ C ++ D.                                                           % 把4个列表合成一个列表


%% 得到编号对应的用率和周增长率
get_ts_growth_rate(Id, Dates, Ts_matrix) ->
    Used = simple_matrix:get_elem_value(Id, lists:last(Dates), Ts_matrix),                      % 得到最后一天的使用率
    From_rows = [lists:last(Dates), hd(Dates)],
    Clumns = [Id],
    Rows_fun = 
        fun(V1, V2) -> 
             (simple_matrix:to_number(V1) - simple_matrix:to_number(V2)) / length(Dates) * 7    % 周增长率 = (尾日使用率-首日使用率)/总共天数*7
        end,
    [[Growth_rate1]] = simple_matrix:rows_fun_v_list(From_rows, Clumns, Rows_fun, Ts_matrix),   % 得到周增长率
    [Growth_rate2]= io_lib:format("~.2f", [Growth_rate1]),                                      % (小数点后保留两位)
    [Used ++ "%", Growth_rate2 ++ "%"].



%% 得到web和app日志空间使用率的的平均值
get_web_app_percentage(Dates, File_sys_matrix) ->
    From_clumns = ?WEB_IDS ++ ?APP_IDS,
    To_clumn = "web和app列相加之和",
    Rows = Dates,
    Clumns_fun = 
        fun(V1, V2) -> 
             simple_matrix:to_number(V1) + simple_matrix:to_number(V2) 
        end,
    Matrix2 = simple_matrix:clumns_fun(From_clumns, To_clumn, 
                                       Rows, Clumns_fun, File_sys_matrix),              % 得到"web和app列相加之和"
    From_rows = Dates,
    Clumns = [To_clumn],
    Rows_fun = 
        fun(V1, V2) -> 
             simple_matrix:to_number(V1) + simple_matrix:to_number(V2)
        end,
    [[Sum]] = simple_matrix:rows_fun_v_list(From_rows, Clumns, Rows_fun, 
                                            Matrix2),                                   % 得到web和app之和
    [Ave] = io_lib:format("~.2f", [Sum / (length(From_rows) * length(From_clumns))]),   % 得到web和app百分比的平均值(小数点后保留两位)
    Ave ++ "%".


%% 得到日志服务器日志空间使用率的的平均值
get_log_percentage(Dates, File_sys_matrix) ->
    From_rows = Dates,
    Clumns = ["sjlog"],
    Rows_fun = 
        fun(V1, V2) -> 
             simple_matrix:to_number(V1) + simple_matrix:to_number(V2)
        end,
    [[Sum]] = simple_matrix:rows_fun_v_list(From_rows, Clumns, Rows_fun, 
                                            File_sys_matrix),               % 得总和和
    [Ave] = io_lib:format("~.2f", [Sum / length(From_rows)]),               % 得到百分比平均值(小数点后保留两位)
    Ave ++ "%".



%% 指定列的峰值(多少万),日期,周几
get_value_md_day(Clumn, Dates, Matrix) ->
    [{_, Str_date, Value}|_] = simple_matrix:get_number_max(
            [Clumn], Dates, Matrix),                                    % 得到交易量峰值的日期和数值
    Value1 = lists:sublist(Value, 1, length(Value)-4),                  % 得到交易量峰值的万位数
    {Y, M, D} = files_handler_util:string_to_date(Str_date, "."),       % 得到日期
    MD = lists:concat([M, "月", D, "日"]),                              % 得到字符串日期
    Day = files_handler_util:day_of_the_week({Y, M, D}),                % 得到周几
    [Value1, MD, Day].


%% 得到金融交易客户数占登录客户数百分比
get_moth_login_percentage(Dates, Matrix1) ->
    From_clumns =  ["金融交易用户数", "登陆用户数"],
    To_clumn = "金融交易客户数占登录客户数比值",
    Rows = Dates,
    Clumns_fun = 
        fun(V1, V2) -> 
             simple_matrix:to_number(V1) / simple_matrix:to_number(V2) 
        end,
    Matrix2 = simple_matrix:clumns_fun(From_clumns, To_clumn, 
                                       Rows, Clumns_fun, Matrix1),      % 得到"金融交易客户数占登录客户数比值"列
    From_rows = Dates,
    Clumns = ["金融交易客户数占登录客户数比值"],
    Rows_fun = 
        fun(V1, V2) -> 
             simple_matrix:to_number(V1) + simple_matrix:to_number(V2)
        end,
    [[Sum]] = simple_matrix:rows_fun_v_list(From_rows, Clumns, Rows_fun, 
                                            Matrix2),                   % 得到"金融交易客户数占登录客户数比值"列的和
    [Ave] = io_lib:format("~.2f", [Sum / length(From_rows) * 100]),     % 得到金融交易客户数占登录客户数的平均值百分比(小数点后保留两位)
    Ave ++ "%".




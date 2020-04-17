%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     华夏巡检周报生成脚本模块
%%% @end
-module(zh_sjyh_lastday_excel_script).

-export([filename_fun/1, 
         is_start_fun1/1, 
         is_end_fun1/1, 
         is_start_fun2/1, 
         is_end_fun2/1, 
         content_fun1_2/1,
         is_start_fun3/1, 
         is_end_fun3/1, 
         content_fun3/1
        ]).




%%%===================================================================
%%% API
%%%===================================================================


%% 处理文件路径,自己扩展的"文件数据"键值对列表
filename_fun(Filename) ->
    Basename = filename:basename(Filename),                                         % 得到文件名
    case re:split(Basename, "(\\d{8})", [{return, list}]) of                        % 根据日期把文件名切分成"前缀","日期","其余部分"
        [_Prefix, Date, _] ->                                                       % 如果切分成功
            [{date, files_handler_util:get_before_day(Date)}];                      % 返回日期(由于文件名中的日期是之后的一天,所以日期要前移一天)
        _ ->                                                                        % 如果切分失败
            []                                                                      % 返回空
    end.


%% 判断当前行是否是第1章开头的函数
is_start_fun1(Line) ->
    files_handler_util:is_cantain(Line, "CPU峰值").


%% 判断当前行是否是第1章结尾的函数
is_end_fun1(Line) ->
    files_handler_util:is_cantain(Line, "MEM峰值").


%% 处理第1,2章
content_fun1_2(Line) ->
    case files_handler_util:is_cantain(Line, 
            "sjdb1|sjdb2|sjlog|sjmg1|sjmg2") of % 判断否头包含这这些字符串
        false ->                                % 如不
            {[], []};                               % 不处理,不累加
        true->                                  % 如是
            {[handle_line1_2(Line)], []}               % 处理,累加
    end.


%% 判断当前行是否是第2章开头的函数
is_start_fun2(Line) ->
    files_handler_util:is_cantain(Line, "MEM峰值").


%% 判断当前行是否是第2章结尾的函数
is_end_fun2(Line) ->
    files_handler_util:is_cantain(Line, "文件空间使用情况").


%% 判断当前行是否是第3章开头的函数
is_start_fun3(Line) ->
    files_handler_util:is_cantain(Line, "表空间使用情况").


%% 判断当前行是否是第3章结尾的函数
is_end_fun3(Line) ->
    files_handler_util:is_cantain(Line, ",,,,,,,,,,").


%% 处理第3章
content_fun3(Line) ->
    case files_handler_util:is_cantain(Line, 
            "HXLOG|INDEX_LOG|INDEX_MOBILE|HXMOBILE") of % 判断否头包含这这些字符串
        false ->                                % 如不
            {[], []};                               % 不处理,不累加
        true->                                  % 如是
            {[handle_line3(Line)], []}               % 处理,累加
    end.


% 初步处理1,2章行数据
handle_line1_2(Line) ->
    Line1 = files_handler_util:replace(Line, ":KUX", ""),   % 去除":KUX"
    Line2 = files_handler_util:replace(Line1, ":LZ", ""),   % 去除":LZ"
    [_, ID,_,Number,_] = string:tokens(Line2, ","),         % 用逗号分割行内容,得到机器编号和数值
    {ID, Number}.


% 初步处理3章行数据
handle_line3(Line) ->
    [_, ID,Number,_] = string:tokens(Line, ","),         % 用逗号分割行内容,得到编号和数值
    {ID, Number}.



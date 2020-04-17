%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     华夏巡检周报生成脚本模块
%%% @end
-module(zh_newsjyh_lastday_script).

-export([filename_fun/1, 
         is_start_fun1/1, 
         is_end_fun1/1, 
         is_start_fun2/1, 
         is_end_fun2/1, 
         content_fun/1]).




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
    files_handler_util:is_cantain(Line, "各主机CPU使用率TOP10峰值").


%% 判断当前行是否是第1章结尾的函数
is_end_fun1(Line) ->
    files_handler_util:is_cantain(Line, "各主机内存使用率TOP5峰值").


%% 处理第1章
content_fun(Line) ->
    case files_handler_util:is_cantain(Line, 
        "发生时间|利用率|各主机" % 包含"发生时间""利用率""各主机"这些字符串
        ++ "|^\n$"               % 或空行
        ++ "|\\d{2}:\\d{2}"      % 或时间形式
    ) of                                                % 判断是否是4种不累加的行内容
        true ->                                         % 如是
            {[], []};                                       % 不累加
        false->                                         % 如不
            {[handle_line(Line)], []}                       % 初步处理并累加
    end.

%% 判断当前行是否是第2章开头的函数
is_start_fun2(Line) ->
    files_handler_util:is_cantain(Line, "各主机内存使用率TOP5峰值").


%% 判断当前行是否是第2章结尾的函数
is_end_fun2(Line) ->
    files_handler_util:is_cantain(Line, "（三）文件系统使用情况").


%% 初步处理行数据
handle_line(Line) ->
    Line1 = files_handler_util:replace(Line, "\n", ""),         % 除换行
    Line2 = files_handler_util:replace(Line1, " ", ""),         % 除空格
    Line3 = files_handler_util:replace(Line2, ":LZ", ""),       % 除":LZ"
    Line3.



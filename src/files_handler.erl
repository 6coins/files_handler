%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     程序负责
%%%         批量处理文件;
%%%         把文件处理成"文件数据","文件数据"其中包含"章数据"列表等数据,这些数据都是erlang数据;
%%%         (这些数据可以供其他erlang模块使用,如分析,过滤,存储,拼成xml或json等)
%%% @end
%%%-------------------------------------------------------------------
-module(files_handler).

-export([files_to_file_datas/1]).
-include("files_handler.hrl").

%% 调试用
%-compile(export_all).

%% 公共类型说明
%% (这里数据结构用[proplists:property()]而不用record()的原因是,[proplists:property()]可以自定义扩展)
-type config() :: [{config_kv()}].  % 配置
-type config_kv() ::                % 配置键值对
    {dir, string()} |               % (必选)目录(里面是要处理的文件)
    {reg_exp, string()} |           % (可选)搜索文件用的正则表达式(正则表达式描述见erlang中的re模块)(如果不选此项,默认所有文件)
    {recursive, boolean()} |        % (可选)是否递归的去子目录搜索文件(如果不选此项,默认不递归)
    {filename_fun, filename_fun()} |% (可选)处理文件名的函数(如果不选此项,不处理文件路径)
    {chapters_conf, chapters_conf()} |   % (必选)此类文件对应的"章数据"列表配置
    proplists:property().           % (可选)其他的可扩展项

-type file_datas() :: [file_data()].% "文件数据"列表
-type file_data() :: [file_data_kv()].  % "文件数据"
-type file_data_kv() ::             % "文件数据"键值对
    {filename, file:name()} |       % (被返回内容占用)文件路径
    {chapters, chapters()} |        % (被返回内容占用)"章数据"配置列表
    {handle_info, #handle_info{}} |  % 处理的章信息(用于关联chapters()和chapters_conf())
    proplists:property().           % (可选)其他的可扩展项

-type chapters_conf() :: [{chapter_conf_number(), chapter_conf()}].    % "章数据"配置列表
-type chapter_conf() :: [chapter_conf_kv()].    % "章数据"配置
-type chapter_conf_kv() ::                      % "章数据"配置键值对
    {is_start_fun, is_start_fun()} |% (配置必选)
    {is_end_fun, is_end_fun()} |    % (配置必选)
    {content_fun, content_fun()} |  % (配置必选)
    proplists:property().           % (可选)其他的可扩展项

-type chapters() :: [{chapter_number(), chapter()}].    % "章数据"列表
-type chapter() :: [chapter_kv()].  % "章数据"
-type chapter_kv() ::               % "章数据"键值对
    {content_datas, [content_data()]} |  % (content_fun返回必须包含这项)
    proplists:property().           % (可选)其他的可扩展项


-type filename_fun() :: function(). % 处理文件路径函数
                                    % 即{M,F}, 形如M:F(file:name()) -> [proplists:property()].
                                    % 返回内容将更新到当前"文件数据"
-type line() :: string().           % 文件中一行的内容
-type is_start_fun() :: function(). % 判断当前行是否是一章开头的函数(即{M, F}, 形如M:F(line()) -> boolean().)
-type is_end_fun() :: function().   % 判断当前行是否是一章结尾的函数(即{M, F}, 形如M:F(line()) -> boolean().)
-type content_fun() :: function().  % 处理章开头以外内容的函数
                                    % 即{M, F}, 形如M:F(line()) -> {[content_data()] | [], [proplists:property()]}. 
                                    % 其返回中[content_data()] | []项将会累加到当前"章数据"的content_datas项,再更新到当前"章数据"
                                    % 其返回中其余项将更新到当前"章数据"
-type content_data() :: term().     % 行内容处理完的结果
-type chapter_conf_number() :: integer(). % 章配置编号
-type chapter_number() :: integer().        % 章编号



%% 导出供其他模块使用的公共类型说明
-export_type([config/0, file_data/0, line/0]).   


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 批量的把文件处理成erlang数据
%% @end
-spec files_to_file_datas(config()) -> {ok, file_datas()}.
%%--------------------------------------------------------------------
files_to_file_datas(Config) ->
    {ok, File_datas1} = init_file_datas(Config),                                    % 初始化"文件数据"列表(一般是根据文件名初始化)
    {ok, File_datas2} = handle_file_datas(File_datas1, Config),                     % 处理"文件数据"列表
    {ok, File_datas2}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 得到目录中文件的列表,并处理成"文件数据"列表
%% @end
-spec init_file_datas(config()) -> {ok, file_datas()}.
%%--------------------------------------------------------------------
init_file_datas(Config) ->
    Dir = proplists:get_value(dir, Config),                                         % 得到配置,其含义见公共类型说明config_kv()
    Reg_exp = proplists:get_value(reg_exp, Config, ".*"),                           % 同上
    Recursive = proplists:get_value(recursive, Config, false),                      % 同上
    Fun = fun(Filename, AccIn) ->                                                   % 这fold函数的fun
            File_data1 = [{filename, Filename}],                                    % 拼成"文件数据"
            File_data2 = 
                case proplists:get_value(filename_fun, Config) of                       % 得到处理文件路径
                    {M, F} ->                                                               % 如果有filename_fun项
                        File_data_kvs = apply(M, F, [Filename]),                                % 处理文件路径得到键值对列表
                        files_handler_util:proplists_update(File_data_kvs, File_data1);         % 把键值对更新到"文件数据"
                    undefined ->                                                            % 如果没有filename_fun项
                        File_data1                                                              % 返回
                end,
            [File_data2 | AccIn]                                                        % 把"文件数据"加到累加器中
          end,
    File_datas1 = filelib:fold_files(Dir, Reg_exp, Recursive, Fun, []),             % 得到指定目录的文件,把每个文件路径交给fun处理,返回结果
    File_datas2 = lists:reverse(File_datas1),                                       % 修正fold处理后的倒序
    {ok, File_datas2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理"文件数据"列表
%% @end
-spec handle_file_datas(File_datas1 :: file_datas(), config()) -> {ok, File_datas2 :: file_datas()}.
%%--------------------------------------------------------------------
handle_file_datas(File_datas1, Config) -> 
    File_datas2 = 
        [file_handler:handle_file_data(File_data, Config) || File_data <- File_datas1],          % 处理每个"文件数据"
    {ok, File_datas2}.




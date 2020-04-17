################################################################################ 
## 依赖:
################################################################################ 
    linux服务器,其安装了unoconv-0.8.2和LibreOffice_5.3.7.2
    (安装文档参考files_handler_xxx/example/week_report/doc/unoconv_用于doc_xls_pdf_txt等文件相互转换_20180109.txt)
        

################################################################################ 
## 生成巡检周报的步骤:
################################################################################ 
    (行发同事会给你一个目录里边是数据文件,如20180122-20180128/
        里边文件如下:
        zh_newsjyh_lastday_日期xxxx.doc(7个)
        zh_sjyh_lastday_日期xxxx.doc(7个)
        zh_sjyh_lastday_excel_日期xxxx_currently.xls(7个)
    )

    1.填写输入文件(例子文件见files_handler_xxx/example/week_report/input/)
        web_file_sys_matrix_input.data和app_file_sys_matrix_input.data和other_file_sys_matrix_input.data
            列编号    
                填机器编号(一般无需修改)
            行编号
                填zh_newsjyh_lastday_日期xxxx.doc中文件名日期的前一天,如zh_newsjyh_lastday_2018012303.doc填如["20180122",
            数值列表 
                在zh_newsjyh_lastday_日期xxxx.doc中全局搜"文件系统使用情况",找到机器编号对应的/logs红色部分的的数值,如[12,12,12,12,12,26,12,12,1,1,12,12],    % 20180122的数据
                但日志服务器和数据库要在zh_sjyh_lastday_日期xxxx.doc中全局搜"（三）文件系统使用情况：",找到机器编号对应的/logs和/oracle红色部分的的数值
            
        week_report_input.data
            各组机器最大交换空间使用率
                在生产各个机器执行top(AIX系统是atop)命令, 输出中的用Swap中的used/total即得到了数值,找到到各组机器的最大填写即可
            应用服务器有效连接数的最小值和最大值
                在生产所有backend机器执行命令netstat -na|grep ESTABLISHED|wc -l,得到最小和最大值填写即可
            记录数
                在生产qlsql执行如下sql,根据结果填写即可(注意要按顺序HXMOBILE.MA_POS_MERCHANT,HXMOBILE.MA_SMAG,HXMOBILE.MY_OPER_LOGS_HISTORY,HXMOBILE.MY_TRAN_LOGS_HISTORY填写)
                    select 'HXMOBILE.MA_POS_MERCHANT'  名称,count(1)  数量
                    from hxmobile.ma_pos_merchant t
                    union
                    select 'HXMOBILE.MA_SMAG' 名称,count(1)  数量
                    from hxmobile.ma_smag t
                    union
                    select 'HXMOBILE.MY_OPER_LOGS_HISTORY' 名称,count(1)  数量
                    from hxmobile.my_oper_logs_history t
                    union
                    select 'HXMOBILE.MY_TRAN_LOGS_HISTORY' 名称,count(1)  数量
                    from hxmobile.my_tran_logs_history t

    2.把doc和xls文件转成文本文件
        web_file_sys_matrix_input.data,app_file_sys_matrix_input.data,other_file_sys_matrix_input.data,week_report_input.data放入20180122-20180128/
        在20180122-20180128/用word或wps打开每个doc文件,在任意位置加一个行再保存(这是为了解决文件有冗余内容导致无法转换的问题)
        把20180122-20180128/传到linux服务器,执行如下命令
            cd 20180122-20180128/
            unoconv -f txt *.doc ; unoconv -f csv *.xls
            (无报错即转化成功,会在此目录下生成同名的但扩展名不同的文本文件)

    3.生成周报
        cd files_handler_xxx/
        make 
        erl -pa ebin -s week_report run week_report /tmp/20180122-20180128 /tmp/20180122-20180128/out -s init stop
        (注意:不要有中文,Erlang/OTP 17.0之后的shell才能输入中文)
        (week_report run week_report后是输入目录和输出目录)
        如果运行成功会输出"report finish !!"和输入输出目录
        输出目录会生成week_report.csv即是周报中的表格内容

    4.保存结果(以便生成周报时使用)
	    cd /tmp/
        zip -r 20180122-20180128ai.zip 20180122-20180128/
        把周报和20180122-20180128ai.zip传到目录:svn://103.160.182.123:3691/运维/05.行方汇报/周报/

################################################################################ 
## 生成巡检月报的步骤:
################################################################################ 
    1.整理周报输入文件
        如我想生成201801的周报,需要创建一个目录201801/,里边放周报生成的文件,如
            zh_newsjyh_lastday_20180102**.txt 到 zh_newsjyh_lastday_20180201**.txt
            zh_sjyh_lastday_20180102**.txt 到 zh_sjyh_lastday_20180201**.txt
            zh_sjyh_lastday_excel_20180102**_currently.csv 到 zh_sjyh_lastday_excel_20180201**_currently.csv
        并把之前周报如下中的文件汇总成一个文件
            app_file_sys_matrix_input.data  
            other_file_sys_matrix_input.data
            web_file_sys_matrix_input.data
        复制修改(如果需要)周报模板
            cp files_handler_xxx/example/example/week_report/input/month_report_template.txt /tmp/201801/
        
    2.生成月报
        cd files_handler_xxx/
        make 
	    erl -pa ebin -s week_report run month_report /tmp/201801/ /tmp/201801/out/ -s init stop
        (注意:不要有中文,Erlang/OTP 17.0之后的shell才能输入中文)

    3.填写月报
        /tmp/201801/out/month_report.txt生成的是月报"1、交易量"到"6、系统其他情况"的内容
        再把/tmp/201801/out/*.csv文件内容填到<<月报生成模版-2018年1月.xlsx>>把生成的图片填到"二、巡检详细信息"





################################################################################ 
## 将问题和解决文档的文本文件导成csv文件步骤
################################################################################ 
    cd files_handler_xxx/
    make 
    erl -pa ebin -s faq_export run example/faq_export/input/faq.txt example/faq_export/output/faq.csv -s init stop
    (注意:不要有中文,Erlang/OTP 17.0之后的shell才能输入中文)

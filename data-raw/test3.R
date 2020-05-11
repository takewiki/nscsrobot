library(nrcsrobot)
read_kflog_new() -> bbd
View(bbd)

txt <- bbd$FkfLog;
#head(txt)

#删除----


txt <- tsdo::str_delRows(txt,"单聊")
txt <- tsdo::str_delRows(txt,"群聊")
txt <- tsdo::str_delRows(txt,"======")
txt <- tsdo::str_delRows(txt,"------")
has_datetime <- tsdo::str_HasDateTime(txt)
has_datetime
mydata <- data.frame(FLog =txt,FLag=has_datetime,stringsAsFactors = F)

mydata2 <- tsdo::df_combineRows(data = mydata,var_txt = 'FLog',var_flag = 'FLag')


mydata2$log_datetime <- tsdo::str_extractDateTime(mydata2$FLog)

mydata2$log_date <- tsdo::str_extractDate(mydata2$FLog)

mydata2$log_time <- tsdo::str_extractTime(mydata2$FLog)

#提取相关信息


View(mydata2)






  mydata3 <- tsdo::df_splitByCol(data = mydata2,var_txt='FLog',var_split='log_datetime',var_left='author',left_skip=2,var_right='content',right_skip=4)


View(mydata3)

mydata4 <- tsdo::df_setLabel(data = mydata3,var_txt = 'FLog',keyword = '捷豹路虎官方旗舰店',var_flag = 'FIsA')


View(mydata4)


library(nrcsrobot)
mydata <- read_kflog2_new()
View(mydata)



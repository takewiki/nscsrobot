library(nrcsrobot);
library(tsdo);
library(tsda);
library(dplyr);


data <-read_kflog(file = 'data-raw/input/2019-07-16.txt');
log <- data$FkfLog;
#' 针对数据进行删除处理，不包括------
log <- log_delBash(log);
# 针对日志进行分解，分为2列，日志内容及作者时间
log_split <- str_splitByRighttBrace(x = log,suffix = ":");
authorTime <- log_split[[1]];
logContent <- log_split[[2]];

#分离作者与datetime信息
aut_datetime <- str_splitByLeftBrace(authorTime);
aut_id <- aut_datetime[[1]];
dlg_datetime <- aut_datetime[[2]];

res <- data.frame(aut_id,dlg_datetime,logContent,stringsAsFactors = F);
#View(res);
#处理异常数据，删除日期为NA为的记录
res <- res %>% log_delNA();
#删除所有聊天记录为亲的记录
keys_equal <-
res <- res[res$logContent != '亲',];
res <- res[res$logContent != "  亲",];
#删除ID异常记录,
#目前发现地址信息混合到ID中
res <- res[len(res$aut_id) <=30,]
#删除所有聊天记录为"'的记录
res <- res[res$logContent != "  ",];
res$isA <- str_contain(res$aut_id,'捷豹路虎')
res$dlg_date <- left(res$dlg_datetime,10);
res$dlg_hms <-right(res$dlg_datetime,8);
#判断会话的分组
res$gp_id <-res$aut_id;
res$gp_id[res$isA ==TRUE] <- "";
res$gp_id <- str_copyPrevRow(res$gp_id);
res$session_id <- getSessionId(res$isA);
#按列筛选数据
res <- res %>% df_selectCol(c('dlg_date','gp_id','session_id','isA','logContent'))
res$action_id <- res$session_id *2;
res$action_id[res$isA == FALSE] <- res$action_id[res$isA == FALSE] -1;

# 数据进行分组处理
g <-res$action_id;
res <- split(res,g);
# 针对多行数据进行合并；
res <- lapply(res, log_combine_gp);

#将结合调整回一个数据框；
res <- do.call("rbind",res);

#将数据进一步进行处理
#删除actionId
res <- res[,c( "dlg_date","gp_id","session_id","isA","logContent")]
res_q <- res[res$isA == FALSE,];
res_q$question <- res_q$logContent;
res_a <- res[res$isA == TRUE,];
res_a$answer <- res_a$logContent;
res <- df_innerJoin_bySameColNames(res_q,res_a,"session_id");
res_name_sel <-c("dlg_date.x","gp_id.x","session_id","question","answer")
res<-res[,res_name_sel];
names(res) <-c("dlg_date","gp_id","session_id","question","answer")
View(res);



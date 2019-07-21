library(nrcsrobot);
library(tsdo);
library(tsda);

data <-read_kflog(file = 'data-raw/input/2019-07-16.txt');
log <- data$FkfLog;
#' 针对数据进行删除处理，不包括------
log <-log[left(log,6) != '------'];
# 数据处理
log[1:10];


log_split <-splitStr(log,"\\):");
list_count <- length(log_split);
authorTime <- character(list_count);
logCotent <- character(list_count);
for (i in 1:list_count){
  authorTime[i] <- log_split[[i]][1];
  logCotent[i] <- log_split[[i]][2];
}
#分离作者与datetime信息
#authorTime;
author_split <- splitStr(authorTime,"\\(");
aut_count <- length(author_split);
aut_id <-character(aut_count);
dlg_datetime <- character(aut_count);
for (j in 1:aut_count){
  aut_id[j] <- author_split[[j]][1];
  dlg_datetime[j] <- author_split[[j]][2];
}

res <- data.frame(aut_id,dlg_datetime,logCotent,stringsAsFactors = F);
#View(res);
#处理异常数据，删除日期为NA为的记录
res <- res[!is.na(res$dlg_datetime),];
#删除所有聊天记录为亲的记录
res <- res[res$logCotent != '亲',];
res <- res[res$logCotent != "  亲",];
#删除ID异常记录,
#目前发现地址信息混合到ID中
res <- res[len(res$aut_id) <=30,]
#删除所有聊天记录为"'的记录
res <- res[res$logCotent != "  ",];
res$isA <- str_contain(res$aut_id,'捷豹路虎')
res$dlg_date <- left(res$dlg_datetime,10);
res$dlg_hms <-right(res$dlg_datetime,8);
#判断会话的分组
res$gp_id <-res$aut_id;
res$gp_id[res$isA ==TRUE] <- "";
res$gp_id <- str_copyPrevRow(res$gp_id);
View(res);



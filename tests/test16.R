library(nrcsrobot);
library(tsdo);
library(tsda);
library(readxl)
#加载匹配表
source('./tests/mt.R',encoding = 'utf-8');
data_tpl <- read_excel("data-raw/output/知识点导入模板.xlsx",
                       col_names = FALSE)
data_tpl <- tbl_as_df(data_tpl);
data_tpl <- data_tpl[1:2,];
col_count <- ncol(data_tpl);
col_nickName <-LETTERS[1:col_count];
names(data_tpl) <- col_nickName;


res <- dealLog2(file = "data-raw/input/2019-07-16.txt",
                file_filter_equal,file_filter_like,
                question_filter_equal,question_filter_like,
                answer_filter_equal,answer_filter_like,
                row_sep = row_sep);
row_count <- nrow(res);
A <- paste("捷豹路虎/",res$dlg_date,sep="");
B <- res$question;
C <- res$question;
D <- res$answer;
E <- rep("1",row_count);
F <-rep("",row_count);
G <-rep("",row_count);
H <- rep("1",row_count);
I <-rep("",row_count);
res_formatted <- data.frame(A,B,C,D,E,F,G,H,I,stringsAsFactors = FALSE);

res_output <- rbind(data_tpl,res_formatted);
View(res_output);
openxlsx::write.xlsx(res_output,"res_output16.xlsx");





library(readxl);
library(tsda);
library(tsdo);

#定义文件级的过滤条件-------

file_filter <- read_excelBooks("data-raw/file_filter/");
file_filter_equal <- unique(file_filter$精确匹配[!is.na(file_filter$精确匹配)]);

file_filter_like <- unique(file_filter$近似匹配[!is.na(file_filter$近似匹配)]);
#多行分隔符；
row_sep = "";

#View(file_filter);

#问题的BL------
question_filter <- read_excelBooks("data-raw/question_filter/");
question_filter_equal <- unique(question_filter$精确匹配[!is.na(question_filter$精确匹配)]);

question_filter_like <- unique(question_filter$近似匹配[!is.na(question_filter$近似匹配)]);

#答案的BL------
answer_filter <- read_excelBooks("data-raw/answer_filter/");
answer_filter_equal <- unique(answer_filter$精确匹配[!is.na(answer_filter$精确匹配)]);

answer_filter_like <- unique(answer_filter$近似匹配[!is.na(answer_filter$近似匹配)]);



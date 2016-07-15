nThreads <- 28
enable_wordcut_read <- FALSE
enable_word2vec_train_per_file <- FALSE
enable_word2vec_dis_calc <- TRUE
enable_lda_per_file <- FALSE
work_dir <- "/home/rstudio2/barrage"
print_lbls <- c("评论总数","有效评论数","广告评论数")
class_lbls <- c("无效评论数","有效评论数","广告评论数")
data_list_file <- "bar_file_list.txt"
data_file_suffix <- c("未审核","审核未通过","审核通过")
data_tags_file <- "input_file_tags.txt"
topic_list_file <- "topic_file_list.txt"
topic_tags_file <- "topic_file_tags.txt"
AdPhFile <- "AdPhrases.txt"
FoulPhFile <- "FoulPhrases.txt"
StopWordsFile <- "StopWords.txt"
SynWordsFile <- "h_SynonymWords.txt"
SegWordsFile <- "SegmentWords.txt"
NounWordsFile <- "NounWords.txt"
dict_dir <-
  "/usr/lib64/R/library/Rwordseg/dict"
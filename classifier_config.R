in_file <- "~/barrage/第三周综合全utf8.txt"

barrage_rdata_file <- "~/barrage/wmjq_week3_barrage.RData"

StopWordsFile <- "StopWords.txt"

nThreads <- 28

enable_smote <- FALSE

s_portion <- 0.1 # sample portion of all input

mul_thrd_seg_words_line_limit <- 20000 # minimum number of lines to apply multi-threaded segmentCN function

enable_p_samp_portion <- TRUE

p_samp_portion <- 0.5 # positive sample portion

t_portion <- 0.8 # train portion of samples

sel_n <- 4000 # feature vector size
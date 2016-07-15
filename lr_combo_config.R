in_file <- "~/barrage/第三周综合全utf8.txt"

barrage_rdata_file <- "~/barrage/wmjq_week3_barrage.RData"

StopWordsFile <- "StopWords.txt"

nThreads <- 28

enable_smote <- FALSE

tr_s_portion <- 0.01 # sample portion of all input

mul_thrd_line_process_limit <- 10000 # minimum number of lines to apply multi-threaded processing

enable_lr_combo <- TRUE

lr_model_num <- 2

lr_model_label_file <- "lr_model_label.txt"

enable_p_samp_portion <- TRUE # disable/invalid if enable_lr_combo <- TRUE

lr_p_p_samp_portion <- 0.7 # positive sample portion for positive sample recognition lr model

lr_n_p_samp_portion <- 0.3 # positive sample portion for negtive sample recognition lr model

te_s_portion <- 0.01 # test samples' sampling portion of all samples

te_p_portion <- 0.5 # test sample portion of positive sample

t_portion <- 0.8 # train portion of samples, 

sel_n <- 3000 # feature vector size
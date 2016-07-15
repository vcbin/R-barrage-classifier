source("classifier_config.R")

gen_word_voc <- function(sing_line) {
  line <- unlist(strsplit(sing_line,"\\s+"))
  word_voc <- NULL
  w_len <- length(line)
  for (i in 1:w_len) {
    if (line[i] %in% names(word_voc)) {
      word_voc[line[i]] <- word_voc[line[i]] + 1
      #browser()
    }
    else {
      #browser()
      word_voc <- c(word_voc,1)
      names(word_voc)[length(word_voc)] <- line[i]
    }
  }
  word_voc
}

# mapping input word vector to feature vector
gen_feature_vec <- function(text_vec, sel_words){
  feat_len <- length(sel_words)
  feat_vec <- vector("list",feat_len)
  names(feat_vec) <- sel_words
  text_len <- length(text_vec)
  word_voc <- gen_word_voc(text_vec)
  word_names <- names(word_voc)
  for (i in 1:feat_len){
    if (sel_words[i] %in% word_names){
      #browser()
      feat_vec[i] <- word_voc[sel_words[i]]
    } else {
      feat_vec[i] <- 0
      # browser()
    }
  }
  row_sum <- sum(unlist(feat_vec))
  if (row_sum){
    # normalize
    feat_vec <- lapply(feat_vec, function(x){x/row_sum})
  }
  #browser()
  as.data.frame(feat_vec)
}

rnd_test_samp <- sample(input_all_samp,floor(s_portion*length(input_all_samp)))

pfield.start.time <- Sys.time()
library(snow)
clus <- makeCluster(nThreads)
clusterExport(clus,"sel_col")
rnd_test_y <- parLapply(clus,rnd_test_samp,function(x) {
  return (ifelse(sel_col(x,1) == "审核未通过",1L,0L))
})
stopCluster(clus)
pfield.end.time <- Sys.time()
pfield.time.taken <- pfield.end.time - pfield.start.time
pfield.time.taken

pfield.start.time <- Sys.time()
library(snow)
clus <- makeCluster(nThreads)
clusterExport(clus,"sel_col")
rnd_samp_content <- parLapply(clus,rnd_test_samp,function(x) {
  return (sel_col(x,6))
})
stopCluster(clus)
pfield.end.time <- Sys.time()
pfield.time.taken <- pfield.end.time - pfield.start.time
pfield.time.taken

segword.start.time <- Sys.time()
library(Rwordseg)
segment.options(isNameRecognition = TRUE) # 设置人名识别
rnd_samp_words <-
  lapply(rnd_samp_content, segmentCN, returnType = 'tm', nosymbol = TRUE)
segword.end.time <- Sys.time()
segword.time.taken <- segword.end.time - segword.start.time
cat("分词计算耗时:")
print(segword.time.taken)
cat("\n")

removeStopWords = function(line,word_list) {
  #browser()
  ret = character(0)
  index <- 1
  line <- strsplit(line[[1]],"\\s+")
  it_max <- length(line)
  #       cat(it_max)
  #       cat("\n")
  while (index <= it_max) {
    if (length(word_list[word_list == line[index]]) < 1)
      ret <- c(ret,line[index])
    index <- index + 1
  }
  #line <- paste(line,collapse = " ")
  ret
}
swords.start.time <- Sys.time()
#rnd_samp_clean <- lapply( rnd_samp_words, removeStopWords, mystopwords )
clus <- makeCluster(nThreads)
exp_fun <- c("removeStopWords")
clusterExport(clus,exp_fun)
snow.time( rnd_samp_clean <- parLapply(clus, rnd_samp_words, removeStopWords, mystopwords) )
## remove empty entries and empty rows
rnd_samp_clean <-
  parLapply(clus, rnd_samp_clean, function(x)
    x <- unlist(x))
stopCluster(clus)
rnd_non_emp_idx <- lapply(rnd_samp_clean,length)>0
rnd_samp_clean <- rnd_samp_clean[rnd_non_emp_idx]
swords.end.time <- Sys.time()
swords.time.taken <- swords.end.time - swords.start.time
cat("删除停用词耗时:")
print(swords.time.taken)
cat("\n")

rnd_test_y <- rnd_test_y[rnd_non_emp_idx]

swords.start.time <- Sys.time()
clus <- makeCluster(nThreads)
exp_fun <- c("gen_word_voc","gen_feature_vec")
clusterExport(clus,exp_fun)
snow.time( rnd_test_l <- parLapply(clus, rnd_samp_clean, gen_feature_vec, sel_words) )
swords.end.time <- Sys.time()
swords.time.taken <- swords.end.time - swords.start.time
cat("生成特征向量耗时:")
print(swords.time.taken)
cat("\n")

system.time(rnd_test_df <- data.frame(do.call(rbind, rnd_test_l))) # time consuming part !!

r_p <- predict(fm,rnd_test_df)
r_p_res = exp(r_p) / (1 + exp(r_p))
r_predict_value <- unlist(lapply(r_p_res,function(x) {
  ifelse(x > 0.5,1,0)
}))

rnd_test_y_len <- length(rnd_test_y)

r_high_conf_limit <- 0.4
r_predict_conf <- unlist(lapply(r_p_res,function (x) {abs(x-0.5)}))
r_high_conf_p_idx <- which(r_predict_conf > r_high_conf_limit)
r_high_conf_true_n <- sum(r_predict_value[r_high_conf_p_idx]==rnd_test_y[r_high_conf_p_idx])
r_high_conf_predict_accu <- r_high_conf_true_n/length(r_high_conf_p_idx)
hist(r_predict_conf, labels = T, main = "test sample predict confidence distribution")
r_mean_predict_conf <- mean(r_predict_conf)

r_low_conf_limit <- 0.1
r_low_conf_p_idx <- which(r_predict_conf < r_low_conf_limit)
r_low_conf_true_n <- sum(r_predict_value[r_low_conf_p_idx]==rnd_test_y[r_low_conf_p_idx])
r_low_conf_predict_accu <- r_low_conf_true_n/length(r_low_conf_p_idx)
r_low_conf_p_len <- length(r_low_conf_p_idx)
r_low_conf_p_ratio <- r_low_conf_p_len/ rnd_test_y_len
r_low_conf_predict_accu <- r_low_conf_true_n/ r_low_conf_p_len

cat(
  "高置信度区间","(>",r_high_conf_limit,")","预测正确率",high_conf_predict_accu * 100,"%","\t",
  "低置信度区间","(<",r_low_conf_limit,")","预测正确率",r_low_conf_predict_accu *100,"%","\n", sep = ""
)

rnd_test_equ_res <- rnd_test_y == r_predict_value
rnd_tab_pred_comp <- table(rnd_test_equ_res)
rnd_t_p_count <- rnd_tab_pred_comp[names(rnd_tab_pred_comp) == TRUE]
rnd_f_p_count <-
  rnd_tab_pred_comp[names(rnd_tab_pred_comp) == FALSE]
rnd_accu_rate <- rnd_t_p_count / (rnd_t_p_count + rnd_f_p_count)

rnd_samp_len <- length(rnd_samp_clean)
r_y_tab <- table(unlist(rnd_test_y))
r_p_samp_count <- r_y_tab[2]
r_n_samp_count <- r_y_tab[1]
r_p_samp_ratio <- r_p_samp_count/(length(rnd_test_y))

rnd_test_y <- unlist(rnd_test_y)
r_test_p_samp_idx <- which(rnd_test_y == 1)
r_p_predict_accu <- sum(r_predict_value[r_test_p_samp_idx]==rnd_test_y[r_test_p_samp_idx])/length(r_test_p_samp_idx)
r_test_n_samp_idx <- which(rnd_test_y == 0)
r_n_predict_accu <- sum(r_predict_value[r_test_n_samp_idx]==rnd_test_y[r_test_n_samp_idx])/length(r_test_n_samp_idx)

cat(
  paste0(
    "随机抽样", rnd_samp_len, "个样本 正样本数", r_p_samp_count, "(", r_p_samp_ratio * 100, "%)",
    " 负样本数", r_n_samp_count, "(", (1 - r_p_samp_ratio) * 100, "%)", "\n"
  )
)
cat(
  paste0(
    "预测正确率", rnd_accu_rate * 100, "%",
    " 正样本预测正确率", r_p_predict_accu * 100, "%", " 负样本预测正确率", r_n_predict_accu * 100, "%", "\n"
  )
)
cat(paste0(
  "低置信度预测个数", r_low_conf_p_len, "(", r_low_conf_p_ratio * 100, "%)","\n"
))

rnd_FP_idx <-
  unlist(lapply(seq_along(1:length(rnd_test_y)),function (i) {
    return (r_predict_value[i] == 1 && rnd_test_y[i] == 0)
  }))
rnd_FN_idx <-
  unlist(lapply(seq_along(1:length(rnd_test_y)),function (i) {
    return (r_predict_value[i] == 0 && rnd_test_y[i] == 1)
  }))


r_true_value <- unlist(rnd_test_y)
r_retrieved = sum(r_predict_value)
TP <- sum(r_predict_value & r_true_value)
FP <- r_retrieved - TP
TN <- sum(!(r_predict_value | rnd_test_y))
FN <- sum(!r_predict_value) - TN
r_precision = TP / r_retrieved
r_recall =  TP / sum(r_true_value)
r_F_measure = 2 * r_precision * r_recall / (r_precision + r_recall) #计算Recall，Precision和F-measure
cat(
  "随机抽取样本测试 retrieved=",r_retrieved,"\t","precision=",r_precision*100,"%","\t",
  "recall=",r_recall*100,"%","\t","F_measure=",r_F_measure*100,"%","\n",sep = ""
)

write.table(as.matrix(sample.clean[which(!rnd_test_equ_res)]),"test_cla_err_samp.txt")
write.table(as.matrix(sample.clean[which(rnd_FP_idx)]),"test_cla_fp_samp.txt")
write.table(as.matrix(sample.clean[which(rnd_FN_idx)]),"test_cla_fn_samp.txt")
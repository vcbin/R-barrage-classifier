source("classifier_config.R")
setwd('~/barrage/')
prog.start.time <- Sys.time()

if (!exists(barrage_rdata_file) ||
    file.info(barrage_rdata_file)$size==0) {
  if (!exists("input_file") ||
      object.size("input_file") == 0) {
    read.start.time <- Sys.time()
    con <- file(in_file)
    input_file <- readLines(con,encoding = "UTF-8")
    #   input_file <-
    #   read.table(in_file,quote = "\"",fileEncoding = "UTF-8")
    read.end.time <- Sys.time()
    read.time.taken <- read.end.time - read.start.time
    read.time.taken
    close(con)
    nInLen <- length(input_file)
    length(input_file)
  }
  
  if (!exists("input_all") ||
      object.size("input_all") == 0) {
    pfield.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    input_all <- parLapply(clus,input_file,function(x) {
      m <- gregexpr(pattern = '".*?"', x)
      x <- gsub('"', '', regmatches(x, m)[[1]])
      #content[i] <- x[6]
      ret <- NULL
      for (i in 1:length(x)) {
        ret <- c(ret,x[i])
      }
      ret
    })
    stopCluster(clus)
    #colnames(input_all) <- input_all[1] # Error in `colnames<-`(`*tmp*`, value = list(c("type", "date_time", "ip",  :
    # attempt to set 'colnames' on an object with less than two dimensions
    input_all[1] <- NULL
    pfield.end.time <- Sys.time()
    pfield.time.taken <- pfield.end.time - pfield.start.time
    pfield.time.taken
  }
  
  
  sel_col <- function(x,col_num) {
    x[col_num]
  }
  
  # # 去掉未审核内容
  # get_content <- function(x) {
  #   if (sel_col(x,1) == "未审核") {
  #     return character(0)
  #   }
  #   else {
  #     return(as.character(sel_col(x,6)))
  #   }
  # }
  
  find_rec_num <-
    function(row_idx, input_all, col_idx, filter_con) {
      if (sel_col(input_all[[row_idx]], col_idx) == filter_con) {
        return (row_idx)
      } else {
        return (0)
      }
    }
  if (!exists("input_all_samp") ||
      object.size("input_all_samp") == 0) {
    pfield.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    clusterExport(clus,c("find_rec_num","sel_col"))
    snow.time(ill_row_l <-
                parLapply(
                  clus, seq_along(1:length(input_all)), find_rec_num, input_all, 1, "未审核"
                ))
    ill_row_l <- unique(sort(unlist(ill_row_l)))
    stopCluster(clus)
    pfield.end.time <- Sys.time()
    pfield.time.taken <- pfield.end.time - pfield.start.time
    pfield.time.taken
    ill_row_l <- ill_row_l[-1]
    input_all_samp <- input_all[-c(ill_row_l)]
  }
  
  if (!exists("input_all_p_samp") ||
      object.size("input_all_p_samp") == 0) {
    pfield.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    clusterExport(clus,c("find_rec_num","sel_col"))
    snow.time(
      p_row_idx_l <-
        parLapply(
          clus, seq_along(1:length(input_all_samp)), find_rec_num, input_all_samp, 1, "审核未通过"
        )
    )
    p_row_idx_l <- unique(sort(unlist(p_row_idx_l)))
    stopCluster(clus)
    pfield.end.time <- Sys.time()
    pfield.time.taken <- pfield.end.time - pfield.start.time
    pfield.time.taken
    p_row_idx_l <- p_row_idx_l[-1]
    input_all_p_samp <- input_all_samp[c(p_row_idx_l)]
    all_p_samp_len <- length(input_all_p_samp)
  }
  
  if (!exists("input_all_n_samp") ||
      object.size("input_all_n_samp") == 0) {
    pfield.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    clusterExport(clus,c("find_rec_num","sel_col"))
    snow.time(
      n_row_idx_l <-
        parLapply(
          clus, seq_along(1:length(input_all_samp)), find_rec_num, input_all_samp, 1, "审核通过"
        )
    )
    n_row_idx_l <- unique(sort(unlist(n_row_idx_l)))
    stopCluster(clus)
    pfield.end.time <- Sys.time()
    pfield.time.taken <- pfield.end.time - pfield.start.time
    pfield.time.taken
    n_row_idx_l <- n_row_idx_l[-1]
    input_all_n_samp <- input_all_samp[c(n_row_idx_l)]
    all_n_samp_len <- length(input_all_n_samp)
  }
  
  if (!exists("input_content") ||
      object.size("input_content") == 0) {
    pfield.start.time <- Sys.time()
    library(snow)
    clus <- makeCluster(nThreads)
    clusterExport(clus,"sel_col")
    input_content <-
      parLapply(clus,input_all_samp, sel_col, 6)
    stopCluster(clus)
    in_all_non_emp_idx <- nchar(input_content) > 0
    input_content <- input_content[in_all_non_emp_idx]
    pfield.end.time <- Sys.time()
    pfield.time.taken <- pfield.end.time - pfield.start.time
    pfield.time.taken
  }
  save.image(file = barrage_rdata_file)
} else {
  load(barrage_rdata_file)
}

input_samp <- NULL
samp_size <- floor(length(input_all_samp) * s_portion)
if (enable_p_samp_portion) {
  p_samp_size <- floor(samp_size * p_samp_portion)
  n_samp_size <- samp_size - p_samp_size
  input_n_samp <- sample(input_all_n_samp, n_samp_size)
  input_p_samp <- sample(input_all_p_samp, p_samp_size)
  input_samp <- append(input_n_samp,input_p_samp)
} else {
  input_samp <- sample(input_all_samp, samp_size)
}
# generate y labels
pfield.start.time <- Sys.time()
library(snow)
clus <- makeCluster(nThreads)
clusterExport(clus,"sel_col")
samp_y <- parLapply(clus,input_samp,function(x) {
  return (ifelse(sel_col(x,1) == "审核未通过",1L,0L))
})
#stopCluster(clus)
pfield.end.time <- Sys.time()
pfield.time.taken <- pfield.end.time - pfield.start.time
pfield.time.taken

pfield.start.time <- Sys.time()
library(snow)
#clus <- makeCluster(nThreads)
clusterExport(clus,"sel_col")
samp_content <- parLapply(clus,input_samp,function(x) {
  return (sel_col(x,6))
})
#stopCluster(clus)
pfield.end.time <- Sys.time()
pfield.time.taken <- pfield.end.time - pfield.start.time
pfield.time.taken

if (samp_size > mul_thrd_seg_words_line_limit) {
  segword.start.time <- Sys.time()
  library(snow)
  #clus <- makeCluster(nThreads)
  #clusterExport(clus,"seg_words")
  #detach("package:Rwordseg", unload = TRUE) #jvm can't be forked!!  check out:http://stackoverflow.com/questions/24337383/unloading-rjava-and-or-restarting-jvm
  snow.time(in_samp_words <-
              parLapply(clus, samp_content, function(x) {
                library(Rwordseg)
                #.jinit()
                #insertWords(seg_words) # hang the worker!!!!!!!!
                segment.options(isNameRecognition = TRUE) # 设置人名识别
                segmentCN(x, returnType = 'tm', nosymbol = TRUE)
              }))
  #stopCluster(clus)
  segword.end.time <- Sys.time()
  segword.time.taken <- segword.end.time - segword.start.time
  cat("分词计算耗时:")
  print(segword.time.taken)
  cat("\n")
  system.time (write.table(as.matrix(in_samp_words),"sample_segmentCN.txt"))
  nTmpSum <- 0
  # for (nSamLine in 1:length(sample.words)) {
  #   ss_res <- strsplit(sample.words[[nSamLine]][1],"\\s+")
  #   #cat( length(ss_res[[1]]) )
  #   nTmpSum <- nTmpSum + length(ss_res[[1]])
  # }
  #clus <- makeCluster(nThreads)
  snow.time(lines_len <-
              parLapply(clus,in_samp_words,function(x) {
                ss_res <- strsplit(x[1],"\\s+")
                #cat( length(ss_res[[1]]) )
                length(ss_res[[1]])
              }))
  #stopCluster(clus)
  nTmpSum <- sum(unlist(lines_len))
  print("分词后单词数:")
  print(nTmpSum)
  #dim(input_file)
} else {
  segword.start.time <- Sys.time()
  library(Rwordseg)
  segment.options(isNameRecognition = TRUE) # 设置人名识别
  in_samp_words <-
    lapply(samp_content, segmentCN, returnType = 'tm', nosymbol = TRUE)
  segword.end.time <- Sys.time()
  segword.time.taken <- segword.end.time - segword.start.time
  cat("分词计算耗时:")
  print(segword.time.taken)
  cat("\n")
}

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

mystopwords <-
  unlist (read.table(
    StopWordsFile, encoding = "UTF-8",stringsAsFactors = F
  ))

swords.start.time <- Sys.time()
#sample.clean <- lapply( sample.words, removeStopWords, mystopwords )
#clus <- makeCluster(nThreads)
exp_fun <- c("removeStopWords","mystopwords")
clusterExport(clus,exp_fun)
snow.time(sample.clean <-
            parLapply(clus, in_samp_words, removeStopWords, mystopwords))
## remove empty entries and empty rows
sample.clean <-
  parLapply(clus, sample.clean, function(x)
    x <- unlist(x))
#stopCluster(clus)
st_non_emp_idx <- lapply(sample.clean,length) > 0
sample.clean <- sample.clean[st_non_emp_idx]
swords.end.time <- Sys.time()
swords.time.taken <- swords.end.time - swords.start.time
cat("删除停用词耗时:")
print(swords.time.taken)
cat("\n")

samp_y <- samp_y[st_non_emp_idx]

# StemSynonym = function(line,stemword_list) {
#   #browser()
#   #cat(stopword)
#   nStem <- length(stemword_list)
#   #line <- paste(word_list,collapse = " ")
#   grep_line <- line
#   if (grep_line != "")
#   {
#     for (i in 1:nStem) {
#       if (length(grep_line))
#         grep_line <- gsub(synonyms_reg[[i]],names(stemword_list)[i],grep_line)
#     }
#     if (grep_line != line)
#     {
#       cat(grep_line)
#       cat("\n")
#       cat(line)
#       cat("\n\n")
#     }
#   }
#   #cat(line)
#   #cat("\n")
#   #browser()
#   strsplit(grep_line, "\\s+")
# }
# stem.start.time <- Sys.time()
# clus <- makeCluster(nThreads,out_file = "stem_syn_content.txt")
# exp_var <- c("StemSynonym","synonyms","synonyms_reg")
# clusterExport(clus,exp_var)
# snow.time(sample.clean <-
#             parLapply(clus, sample.clean, StemSynonym, synonyms))
# stopCluster(clus)
# #     sample.clean <- lapply(sample.clean, StemSynonym, synonyms)
# stem.end.time <- Sys.time()
# stem.time.taken <- stem.end.time - stem.start.time
# cat("替换同义词耗时：")
# print(stem.time.taken)
# cat("\n")
# #system.time (write.table(as.matrix(sample.clean),"sample_clean_stem.txt"))
if (length(sample.clean) != length(samp_y))
  stop("sampling error! x and y length not matched!")

library(tm)
system.time (corpus_samp <- Corpus(VectorSource(sample.clean)))
system.time(corpus_samp <- tm_map(corpus_samp, removeWords, stopwords("english")))
system.time(dtm_samp <-
              DocumentTermMatrix(corpus_samp, control = list(wordLengths = c(2, Inf))))
system.time(dtm_samp_norm <-
              DocumentTermMatrix(corpus_samp, control = list(wordLengths = c(2, Inf))))
# system.time(dtm_samp <-
#               DocumentTermMatrix(corpus_samp, control = list(
#                 wordLengths = c(2, Inf), weighting = weightTfIdf
#               ))) # Idf is NOT suitable for new text content
library(slam)
term.statistics <- function(dtm) {
  #dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  data.frame(
    term = vocabulary,
    characters = nchar(vocabulary),
    number = grepl("[0-9]", vocabulary),
    nonalpha = grepl("\\W", vocabulary),
    termfreq = col_sums(dtm),
    docfreq = col_sums(dtm > 0),
    reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
    tfidf = tapply(dtm$v / row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm) /
                                                                       col_sums(dtm > 0))
  )
}
voc_stat <- term.statistics(dtm_samp)
samp_p_count <- sum(unlist(samp_y) == TRUE)
samp_n_count <- length(samp_y) - samp_p_count
samp_len <- length(sample.clean)
samp_p_ratio <- samp_n_count / samp_len
cat(
  "样本个数", samp_size, "实际正样本个数", samp_p_count, "(", samp_p_ratio * 100, "%)",
  "负样本个数", samp_n_count, "(", (1 - samp_p_ratio) * 100, "%)", "\n"
)

N <- length(sample.clean)
voc_len <- length(voc_stat$term)
calc_x_squared <- function(cur_word, dtm_samp) {
  cur_word <- as.character(cur_word)
  #browser()
  has_word_idx <- which(tm::inspect(dtm_samp[,cur_word]) != 0)
  if (sum(has_word_idx)==0) {browser()}
  tab_w_y <- table(unlist(samp_y[has_word_idx]))
  A <-
    w_p_count <-
    ifelse(length(tab_w_y[names(tab_w_y) == 1]),tab_w_y[names(tab_w_y) == 1],0)
  B <-
    w_n_count <-
    ifelse(length(tab_w_y[names(tab_w_y) == 0]),tab_w_y[names(tab_w_y) == 0],0)
  #wo_word_idx <- !has_word_idx
  tab_wo_y <- table(unlist(samp_y[-has_word_idx]))
  C <-
    wo_p_count <-
    ifelse(length(tab_wo_y[names(tab_wo_y) == 1]),tab_wo_y[names(tab_wo_y) == 1],0)
  D <-
    wo_n_count <-
    ifelse(length(tab_wo_y[names(tab_wo_y) == 0]),tab_wo_y[names(tab_wo_y) == 0],0)
  names(A) <- names(B) <- names(C) <- names(D) <- NULL
  if (A==0 && B==0 && C==0 && D==0) {browser()}
  #   library(MASS)
  #   cont_tab <- matrix(c(A,B,C,D),byrow = T,2,2)
  #   chi_res <- chisq.test(cont_tab)
  #   (x_sqr <- 
  #     N * '^'(A * D - B * C, 2) / ((A + B) * (C + D) * (A + C) * (B +
  
  #                                                                   D)))
  x_sqr_reduced <- '^'(A * D - B * C, 2) / ((A + B) * (C + D))
  if ( is.na(x_sqr_reduced)|| x_sqr_reduced==0) {browser()}
  x_sqr_reduced
}
xsqr.start.time <- Sys.time()
#system.time( x_sqr_l <- lapply(voc_stat$term, calc_x_squared, dtm_samp) )
#clus <- makeCluster(nThreads)
exp_fun <- c("calc_x_squared","samp_y")
clusterExport(clus,exp_fun)
snow.time( x_sqr_l <- parLapply(clus, voc_stat$term, calc_x_squared, dtm_samp) )
stopCluster(clus)
x_sqr_l <- unlist(x_sqr_l)
x_sqr_l_ord <- order(x_sqr_l,decreasing = T)
xsqr.end.time <- Sys.time()
xsqr.time.taken <- xsqr.end.time - xsqr.start.time
cat("计算x_squared耗时:")
print(xsqr.time.taken)
cat("\n")

if (sel_n > voc_len) {
  sel_n <- voc_len
}
sel_words <- as.character(voc_stat$term[x_sqr_l_ord[1:sel_n]])
write.table(sel_words,"sel_words.txt")
dtm_sel <- dtm_samp_norm[,sel_words]

sel_term_tfidf  <-
  tapply(dtm_sel$v / row_sums(dtm_sel)[dtm_sel$i],   dtm_sel$j,  mean) *
  log2(nDocs(dtm_sel) / col_sums(dtm_sel  >  0))
summary(sel_term_tfidf)

library(Matrix)
feat_mat_s <-
  sparseMatrix(
    i = dtm_sel$i, j = dtm_sel$j, x = dtm_sel$v ,dims = c(dtm_sel$nrow, dtm_sel$ncol), dimnames = dtm_sel$dimnames
  )
dim(feat_mat_s)
summ <- summary(feat_mat_s)
feat_mat <- as.matrix(feat_mat_s) # not working when matrix is too large
#dim(feat_mat)
# system.time (feat_df <-
#                data.frame(
#                  Origin      = rownames(feat_mat_s)[summ$i],
#                  Destination = colnames(feat_mat_s)[summ$j],
#                  Weight      = summ$x
#                ))

n_row <- nrow(feat_mat)
tr_row_n <- floor(n_row * t_portion)
te_row_n <- n_row - tr_row_n # te=test, tr=train
tr_row_idx <- sort(sample(seq_along(1:n_row),tr_row_n))
train_mat <- feat_mat[tr_row_idx,]
train_df <- as.data.frame(train_mat)
rm(train_mat)
train_y <- samp_y[tr_row_idx]
train_y_all_len <- length(train_y)
test_mat <- feat_mat[-tr_row_idx,]
rm(feat_mat)
test_df <- as.data.frame(test_mat)
rm(test_mat)
test_y <- unlist(samp_y[-tr_row_idx])
gc()
train_df_all <- train_df
train_all_len <- nrow(train_df_all)
tr_0_idx <- which(row_sums(train_df_all) == 0)
tr_0_len <- length(tr_0_idx)
tr_df_0_ratio <- tr_0_len / train_all_len

train_df <- train_df[-tr_0_idx,] # remove zero feature vectors of train data frame
train_y_all <- unlist(train_y)
train_y <- train_y[-tr_0_idx]
train_y_len <- length(train_y)
tr_y_tab <- table(unlist(train_y))
if (length(tr_y_tab)==1) stop("训练数据只包含一类样本！")

te_0_idx <- which(row_sums(test_df) == 0)
te_0_len <- length(te_0_idx)
test_len <- nrow(test_df)
te_df_0_ratio <- te_0_len / test_len
test_df <- test_df[-te_0_idx,] # remove zero feature vectors of test data frame
test_y <- test_y[-te_0_idx]
test_y_len <- length(test_y)
if (nrow(test_df)!=test_y_len) stop("测试样本x和y长度不匹配！")
# cat(
#   paste0(
#     "测试集全零输入数", te_0_len, "(", te_df_0_ratio * 100, "%)", ",总输入数", test_len,"\n"
#   )
# )

#col_sel <- paste("V",seq(1:sel_n),sep="")
# ret_fm <- NULL
# col_len <- length(sel_n)
# for (i in 1:sel_n)
# {
#   if (i!=sel_n){ret_fm <-paste0(ret_fm,paste0(sel_words[i],"+"))}
#   else {
#     ret_fm <- ret_fm <-paste0(ret_fm,paste(sel_words[i]))
#   }
# }
# vnam <- paste0("V", 1:sel_n)
pred_vars_str <- paste(sprintf("`%s`", sel_words), collapse="+") # incase of R keywords,wrap your words in backticks "`" so that R will treat them as non-syntactic variable names
fm_str <- paste("y ~ ", pred_vars_str) 
(fmla <- as.formula(fm_str))
y <- as.factor(unlist(train_y))
train_dat <- cbind(y,train_df)
if (enable_smote){
  train_dat_sm <- cbind(train_df,y)
  library(DMwR) # Synthetic Minority Over-sampling TEchnique
  # Please note that it is compulsory that the dependent variable that one needs to predict (column “target” in my case) needs to be at the end of the data frame.
  system.time(train_dat_sm <- SMOTE(y ~ . , train_dat_sm)) # time consuming !!  $O(n)=n^2*m$, n is sample size, m is dimension
  system.time(lr_fit_sm <-
                glm(fmla, family = binomial(link = logit),data = train_dat_sm))
  save(
    lr_fit_sm,
    file = paste0(
      "smote_lr_", "samp=", train_y_len, "_p_port=", p_samp_portion, "_feat=", sel_n, ".data"
    )
  )
}

system.time(lr_fit <-
              glm(fmla, family = binomial(link = logit),data = train_dat))
save(
  lr_fit,
  file = paste0(
    "lr_", "samp=", train_y_len, "_p_port=", p_samp_portion, "_feat=", sel_n, ".data"
  )
)
if (enable_smote) {
  sink(
    paste0(
      "smote_lr_", "samp=", train_y_len,"_p_port=", p_samp_portion, "_feat=", sel_n, "_model_output.txt"
    )
  )
  print(summary(lr_fit_sm))
  sink()
} else {
  sink(
    paste0(
      "lr_", "samp=", train_y_len,"_p_port=", p_samp_portion, "_feat=", sel_n, "_model_output.txt"
    )
  )
  print(summary(lr_fit))
  sink()
}
ifelse(enable_smote, p <-
         predict(lr_fit_sm,test_df), p <- predict(lr_fit,test_df))
pred_val_raw = exp(p) / (1 + exp(p))
pred_val <- unlist(lapply(pred_val_raw,function(x) {
  ifelse(x > 0.5,1,0)
}))

if (enable_smote) {
  sink(
    paste0(
      "smote_lr_", "samp=", train_y_len, "_p_port=", p_samp_portion, "_feat=", sel_n, "_output.txt"
    )
  )
} else {
  sink(
    paste0(
      "lr_", "samp=", train_y_len, "_p_port=", p_samp_portion, "_feat=", sel_n, "_output.txt"
    )
  )
}

te_equ_res <- test_y == pred_val
tab_pred_comp <- table(te_equ_res)
true_count <- tab_pred_comp[names(tab_pred_comp) == TRUE]
false_count <- tab_pred_comp[names(tab_pred_comp) == FALSE]
accu_rate <- true_count / (true_count + false_count)

te_p_samp_idx <- which(test_y == TRUE)
te_p_samp_count <- length(te_p_samp_idx)
te_p_0_count <-   sum(rowSums(test_df[te_p_samp_idx,])==0)
te_p_0_ratio <- te_p_0_count / te_p_samp_count
te_p_samp_ratio <- te_p_samp_count / test_y_len
p_pred_accu <-
  sum(pred_val[te_p_samp_idx] == test_y[te_p_samp_idx]) / te_p_samp_count
te_n_samp_idx <- which(test_y == FALSE)
te_n_samp_count <- length(te_n_samp_idx)
te_n_0_count <-   sum(rowSums(test_df[te_n_samp_idx,])==0)
te_n_0_ratio <- te_n_0_count / te_n_samp_count
te_n_samp_ratio <- 1 - te_p_samp_ratio
te_0_count <- te_p_0_count + te_n_0_count
te_0_ratio <- te_0_count / test_y_len
te_0_idx <- which(rowSums(test_df) == 0)
te_0_pred_accu <-
  sum(pred_val[te_0_idx] == test_y[te_0_idx]) / test_y_len
te_n_0_idx <- which(rowSums(test_df[te_n_samp_idx,]) == 0)
te_n_0_pred_accu <-
  sum(pred_val[te_n_0_idx] == test_y[te_n_0_idx]) / te_n_samp_count
te_p_0_idx <- which(rowSums(test_df[te_p_samp_idx,]) == 0)
te_p_0_pred_accu <-
  sum(pred_val[te_p_0_idx] == test_y[te_p_0_idx]) / te_p_samp_count
n_pred_accu <-
  sum(pred_val[te_n_samp_idx] == test_y[te_n_samp_idx]) / te_n_samp_count

FP_idx <-
  unlist(lapply(seq_along(1:test_y_len),function (i) {
    return (pred_val[i] == 1 && test_y[i] == 0)
  }))
FN_idx <-
  unlist(lapply(seq_along(1:test_y_len),function (i) {
    return (pred_val[i] == 0 && test_y[i] == 1)
  }))
FP_len <- length(which(FP_idx))
FP_ratio <- FP_len / test_y_len
FN_len <- length(which(FN_idx))
FN_ratio <- FN_len / test_y_len

FP_0_idx <- which(rowSums(test_df[FP_idx,]) == 0)
if (length(FP_0_idx)) {
  FP_0_count <- sum(rowSums(test_df[FP_idx,]) == 0)
} else {
  FP_0_count <- 0
}

if (FP_0_count) {
  FP_0_pred_accu <-
    sum(pred_val[FP_0_idx] == test_y[FP_0_idx]) / FP_0_count
} else {
  FP_0_pred_accu <- NA
}
FP_0_ratio <- FP_0_count / FP_len

FN_0_idx <- which(rowSums(test_df[FN_idx,]) == 0)
FN_0_count <- sum(rowSums(test_df[FN_idx,])==0)
if (FN_0_count) {
  FN_0_pred_accu <-
    sum(pred_val[FN_0_idx] == test_y[FN_0_idx]) / FN_0_count
} else {
  FN_0_pred_accu <- NA
}
FN_0_ratio <- FN_0_count / FN_len


tr_y_tab <- table(unlist(train_y))
tr_p_samp_count <- tr_y_tab[2]
tr_n_samp_count <- tr_y_tab[1]
tr_p_samp_ratio <- tr_p_samp_count/(length(train_y))
tr_p_samp_idx <- which(train_y_all == TRUE)
tr_p_samp_count <- length(tr_p_samp_idx)
tr_p_0_count <-   sum(rowSums(train_df_all[tr_p_samp_idx,]) == 0)
tr_p_0_ratio <- tr_p_0_count / tr_p_samp_count
tr_p_samp_ratio <- tr_p_samp_count / train_y_all_len

tr_n_samp_idx <- which(train_y_all == FALSE)
tr_n_samp_count <- length(tr_n_samp_idx)
tr_n_0_count <-   sum(rowSums(train_df_all[tr_n_samp_idx,])==0)
tr_n_0_ratio <- tr_n_0_count / tr_n_samp_count
tr_n_samp_ratio <- tr_n_samp_count / train_y_all_len

# cat(
#   paste0(
#     "训练集全零输入数", tr_0_len, "(", tr_df_0_ratio * 100, "%)", ",总输入数", train_all_len,"\n"
#   )
# )
real_train_len <- nrow(train_df)
real_samp_ratio <- real_train_len / samp_len
feature_len <- ncol(dtm_samp)
cat(
  paste0(
    "预处理后训练集总样本数", train_y_all_len, ",正样本数", tr_p_samp_count, "(", tr_p_samp_ratio * 100, "%)",
    ",", "正样本中全零输入个数", tr_p_0_count, "(", tr_p_0_ratio * 100, "%)", ",", "\n",
    "负样本数", tr_n_samp_count, "(", (1 - tr_p_samp_ratio) * 100, "%)",
    ",", "负样本中全零输入个数", tr_n_0_count, "(", tr_n_0_ratio * 100, "%)", "\n",
    "选择特征数", sel_n, ",", "总特征数", feature_len, "(", sel_n / feature_len * 100, ")", "训练集全零输入数", tr_0_len, "(", tr_df_0_ratio * 100, "%)", "\n",
    "输入样本数", samp_len, " ", "实际训练样本数", real_train_len, "(", real_samp_ratio * 100, "%)", "\n\n"
  )
)

cat(
  paste0(
    "测试集总样本数", test_y_len, ",","全零输入个数", te_0_count, "(", te_0_ratio * 100,"%)",
    ",", "全零输入预测准确率", "(", te_0_pred_accu*100, "%)","\n",
    "正样本数", te_p_samp_count, "(", te_p_samp_ratio * 100, "%)", ",",
    "正样本中全零输入个数", te_p_0_count, "(", te_p_0_ratio * 100, "%)", ",", "预测准确率", te_p_0_pred_accu* 100, "%","\n",
    "负样本数", te_n_samp_count, "(", te_n_samp_ratio * 100, "%)", ",",
    "负样本中全零输入个数", te_n_0_count, "(", te_n_0_ratio * 100, "%)", ",", "预测准确率", te_n_0_pred_accu* 100, "%","\n\n"
  )
)
fp_0_pred_accru_str <-
  ifelse(
    !exists("FP_0_pred_accu") ||
      is.na(FP_0_pred_accu) ||
      is.infinite(FP_0_pred_accu), " ", paste0(",", "全零输入预测正确率", FP_0_pred_accu * 100,  "%")
  )
fn_0_pred_accru_str <-
  ifelse(
    !exists("FN_0_pred_accu") ||
      is.na(FN_0_pred_accu) ||
      is.infinite(FN_0_pred_accu), " ", paste0(",", "全零输入预测正确率", FN_0_pred_accu * 100, "%")
  )
cat(
  paste0(
    "预测正确率", accu_rate * 100,"%", "\t", "正样本预测正确率", p_pred_accu * 100, "%", ",负样本预测正确率", n_pred_accu * 100, "%", "\n",
    "FP:", FP_len, "(", FP_ratio * 100, "%)", ",", "全零输入个数", FP_0_count, "(", FP_0_ratio * 100,"%)", fp_0_pred_accru_str, "\n",
    "FN:", FN_len, "(", FN_ratio * 100, "%)", ",", "全零输入个数", FN_0_count, "(", FN_0_ratio * 100,"%)", fn_0_pred_accru_str, "\n\n"
  )
)

pred_conf <- unlist(lapply(pred_val_raw,function (x) {abs(x-0.5)}))
hist(pred_conf, labels = T)

high_conf_limit <- 0.4
high_conf_p_idx <- which(pred_conf > high_conf_limit)
high_conf_true_n <- sum(pred_val[high_conf_p_idx]==test_y[high_conf_p_idx])
high_conf_pred_accu <- high_conf_true_n/length(high_conf_p_idx)
mean_pred_conf <- mean(pred_conf)

low_conf_limit <- 0.1
low_conf_p_idx <- which(pred_conf < low_conf_limit)
low_conf_true_n <- sum(pred_val[low_conf_p_idx]==test_y[low_conf_p_idx])
low_conf_p_len <- length(low_conf_p_idx)
low_conf_p_ratio <- low_conf_p_len/ test_y_len
low_conf_pred_accu <- low_conf_true_n/ low_conf_p_len

cat(
  "高置信度区间", "(>", high_conf_limit, ")", "预测正确率", high_conf_pred_accu * 100, "%", "\t",
  "低置信度区间", "(<", low_conf_limit, ")", "预测正确率", low_conf_pred_accu * 100, "%", "\n", sep = ""
)
cat(paste0(
  "低置信度预测个数", low_conf_p_len, "(", low_conf_p_ratio * 100, "%)", "\n\n"
))

true_val <- unlist(test_y)
retrieved = sum(pred_val)
TP <- sum(pred_val & true_val)
FP <- retrieved - TP
TN <- sum(!(pred_val | true_val))
FN <- sum(!pred_val) - TN
precision = TP / retrieved
recall = TP / sum(true_val)
F_measure = 2 * precision * recall / (precision + recall) #计算Recall，Precision和F-measure
type_I_err <- FN / (TP + FN)
type_II_err <- FP / (FP + TN)
mean_err <- (type_I_err + type_II_err)/2
cat(
  paste0(
    "retrieved=",retrieved,"\t","precision=",precision * 100,"%","\t","recall=",recall *
      100,"%","\t","F_measure=",F_measure * 100,"%", "\n",
    "type_I_err=", type_I_err * 100, "%", ",", "type_II_err=", type_II_err * 100, "%", "\n",
    "mean_err=", mean_err * 100, "%", "\n"
  )
)
sink()

library(pROC)
roc_obj <- roc(test_y,pred_val)
plot.roc(roc_obj,test_y,pred_val,print.auc = T)

library(ROCR)
pred_obj <- prediction(pred_val, test_y)
perf <- performance(pred_obj, measure = "f")
plot(perf)

# library(randomForest)
# system.time( fit_rf <- randomForest::randomForest(train_dat) ) # HANG the system !!

library(bigrf)
train_y_fac <- as.factor(unlist(train_y))
system.time( fit_bigrfc <- bigrfc(train_df, train_y_fac, ntrees = 50, trace = 1) )
save(
  fit_bigrfc,
  file = paste0(
    "rf_", "samp=", train_y_len,"_p_port=", p_samp_portion, "_feat=", sel_n, ".data"
  )
)
print(fit_bigrfc)
predictions <- predict(fit_bigrfc, test_df)
rf_pred <-
  unlist(lapply(predictions@.Data, function(x) {
    if (x == 1) {
      return (0)
    } else {
      return (1)
    }
  }))
rf_retrieved = sum(rf_pred)
rf_TP <- sum(rf_pred & true_val)
rf_FP <- rf_retrieved - rf_TP
rf_TN <- sum(!(rf_pred | true_val))
rf_FN <- sum(!rf_pred) - rf_TN
rf_precision = rf_TP / rf_retrieved
rf_recall = rf_TP / sum(true_val)
rf_F_measure = 2 * rf_precision * rf_recall / (rf_precision + rf_recall) #计算Recall，Precision和F-measure
rf_type_I_err <- rf_FN / (rf_TP + rf_FN)
rf_type_II_err <- rf_FP / (rf_FP + rf_TN)
rf_mean_err <- (rf_type_I_err + rf_type_II_err)/2
sink(
  paste0(
    "rf_", "samp=", train_y_len,"_p_port=", p_samp_portion, "_feat=", sel_n, "_output.txt"
  )
)
cat(
  paste0(
    "Random Forest: retrieved=",rf_retrieved,"\t","precision=",rf_precision * 100,"%","\t","recall=",rf_recall *
      100,"%","\t","F_measure=",rf_F_measure * 100,"%", "\n",
    "type_I_err=", rf_type_I_err * 100, "%", ",", "type_II_err=", rf_type_II_err * 100, "%", "\n",
    "mean_err=", rf_mean_err * 100, "%", "\n"
  )
)

rf_te_p_samp_idx <- which(test_y == TRUE)
rf_te_p_samp_count <- length(rf_te_p_samp_idx)
rf_te_p_0_count <-   sum(rowSums(test_df[rf_te_p_samp_idx,])==0)
rf_te_p_0_ratio <- rf_te_p_0_count / rf_te_p_samp_count
rf_te_p_samp_ratio <- rf_te_p_samp_count / test_y_len
rf_p_pred_accu <-
  sum(rf_pred[rf_te_p_samp_idx] == test_y[rf_te_p_samp_idx]) / rf_te_p_samp_count
rf_te_n_samp_idx <- which(test_y == FALSE)
rf_te_n_samp_count <- length(rf_te_n_samp_idx)
rf_n_pred_accu <-
  sum(rf_pred[rf_te_n_samp_idx] == test_y[rf_te_n_samp_idx]) / rf_te_n_samp_count
rf_te_equ_res <- rf_pred == true_val
rf_equ_prop_tab <- prop.table(table(rf_te_equ_res))
rf_accu_rate <- rf_equ_prop_tab[names(rf_equ_prop_tab)=="TRUE"]
cat("Random Forest预测准确率:", rf_accu_rate * 100, "%",
    "正样本预测正确率", rf_p_pred_accu * 100, "%", ",负样本预测正确率", rf_n_pred_accu * 100, "%", "\n")
sink()
sink(
  paste0(
    "rf_", "samp=", train_y_len,"_p_port=", p_samp_portion, "_feat=", sel_n, "model_output.txt"
  )
)
print(fit_bigrfc)
sink()

library(pROC)
rf_roc_obj <- roc(test_y,rf_pred)
plot.new()
plot.roc(rf_roc_obj,test_y,rf_pred,print.auc = T)

library(ROCR)
rf_pred_obj <- prediction(rf_pred, test_y)
rf_perf <- performance(rf_pred_obj, measure = "f")
plot(rf_perf)

write.table(as.matrix(sample.clean[which(!te_equ_res)]),"tr_cla_err_samp.txt")
write.table(as.matrix(sample.clean[which(FP_idx)]),"tr_cla_fp_samp.txt")
write.table(as.matrix(sample.clean[which(FN_idx)]),"tr_cla_fn_samp.txt")

prog.end.time <- Sys.time()
prog.time.taken <- prog.end.time - prog.start.time
cat("程序耗时:")
print(prog.time.taken)

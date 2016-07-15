source("classifier_config.R")
setwd('~/barrage/')
prog.start.time <- Sys.time()

if (!exists(barrage_rdata_file) ||
    file.info(barrage_rdata_file)$size == 0) {
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
system.time(corpus_samp <-
              tm_map(corpus_samp, removeWords, stopwords("english")))
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
  if (sum(has_word_idx) == 0) {
    browser()
  }
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
  if (A == 0 && B == 0 && C == 0 && D == 0) {
    browser()
  }
  #   library(MASS)
  #   cont_tab <- matrix(c(A,B,C,D),byrow = T,2,2)
  #   chi_res <- chisq.test(cont_tab)
  #   (x_sqr <-
  #     N * '^'(A * D - B * C, 2) / ((A + B) * (C + D) * (A + C) * (B +
  
  #                                                                   D)))
  x_sqr_reduced <- '^'(A * D - B * C, 2) / ((A + B) * (C + D))
  if (is.na(x_sqr_reduced) || x_sqr_reduced == 0) {
    browser()
  }
  x_sqr_reduced
}
xsqr.start.time <- Sys.time()
#system.time( x_sqr_l <- lapply(voc_stat$term, calc_x_squared, dtm_samp) )
#clus <- makeCluster(nThreads)
exp_fun <- c("calc_x_squared","samp_y")
clusterExport(clus,exp_fun)
snow.time(x_sqr_l <-
            parLapply(clus, voc_stat$term, calc_x_squared, dtm_samp))
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
feat_mat <-
  as.matrix(feat_mat_s) # not working when matrix is too large
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

train_df_all <- train_df
train_all_len <- nrow(train_df_all)
tr_0_idx <- which(row_sums(train_df_all) == 0)
tr_0_len <- length(tr_0_idx)
tr_df_0_ratio <- tr_0_len / train_all_len

train_df <-
  train_df[-tr_0_idx,] # remove zero feature vectors of train data frame
train_y_all <- unlist(train_y)
train_y <- train_y[-tr_0_idx]
train_y_len <- length(train_y)
tr_y_tab <- table(unlist(train_y))
if (length(tr_y_tab) == 1)
  stop("训练数据只包含一类样本！")

te_0_idx <- which(row_sums(test_df) == 0)
te_0_len <- length(te_0_idx)
test_len <- nrow(test_df)
te_df_0_ratio <- te_0_len / test_len
test_df <-
  test_df[-te_0_idx,] # remove zero feature vectors of test data frame
test_y <- test_y[-te_0_idx]
test_y_len <- length(test_y)
if (nrow(test_df) != test_y_len)
  stop("测试样本x和y长度不匹配！")

train_y <- unlist(train_y)
tr_df_save <- cbind(train_df,train_y)
system.time(write.csv(tr_df_save, "train_df.csv"))
system.time(write.csv(test_df, "test_df.csv", col.names = F))
test_y <- unlist(test_y)
system.time(write.csv(test_y, "trues.csv", col.names = F))

prog.end.time <- Sys.time()
prog.time.taken <- prog.end.time - prog.start.time
cat("程序耗时:")
print(prog.time.taken)

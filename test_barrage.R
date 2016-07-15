library(openxlsx)

source("~/barrage/bar_config.R") # string/path variables definitions
setwd(work_dir)

read.start.time <- Sys.time()
con <- file("~/barrage/第三周综合全utf8.txt")
input_file <- readLines(con,encoding = "UTF-8")
#input_file <- read.table("~/barrage/第二周综合全.txt",quote = "\"",fileEncoding = "UTF-8")
read.end.time <- Sys.time()
read.time.taken <- read.end.time - read.start.time
read.time.taken
close(con)
nInLen <- length(input_file)
length(input_file)
head(input_file)

# pfield.start.time <- Sys.time() 
# library(doParallel)
# clus <- makeCluster(nThreads)
# registerDoParallel(clus)
# nRec <- length(input_file)
# foreach (i = 2:nRec)  %dopar% {
#   m <- gregexpr(pattern = '".*?"', input_file[i])
#   input_file[i] <- gsub('"', '', regmatches(input_file[i], m)[[1]])
# }
# stopCluster(clus)
# pfield.end.time <- Sys.time()
# pfield.time.taken <- pfield.end.time - pfield.start.time
# pfield.time.taken
# colnames(input_file) <- input_file[1]
#content <- vector("list",nInLen)

# pfield.start.time <- Sys.time()
# library(snow)
# clus <- makeCluster(nThreads)
# input_content <- parLapply(clus,input_file,function(x) {
#   m <- gregexpr(pattern = '".*?"', x)
#   x <- gsub('"', '', regmatches(x, m)[[1]])
#   #content[i] <- x[6]
#   x[6]
# })
# stopCluster(clus)
# pfield.end.time <- Sys.time()
# pfield.time.taken <- pfield.end.time - pfield.start.time
# pfield.time.taken

input_col <- parLapply(clus,input_file, function(x,col_num) {
  m <- gregexpr(pattern = '".*?"', x)
  x <- gsub('"', '', regmatches(x, m)[[1]])
  #content[i] <- x[6]
  x[col_num]
}, 6)

pfield.start.time <- Sys.time()
library(snow)
clus <- makeCluster(nThreads)
input_all <- parLapply(clus,input_file,function(x) {
  m <- gregexpr(pattern = '".*?"', x)
  x <- gsub('"', '', regmatches(x, m)[[1]])
  #content[i] <- x[6]
  ret <- NULL
  for (i in 1:length(x) ) {
    ret <- c(ret,x[i])
  }
  ret
})
stopCluster(clus)
colnames(input_all) <- input_all[1]
input_all[1] <- NULL
pfield.end.time <- Sys.time()
pfield.time.taken <- pfield.end.time - pfield.start.time
pfield.time.taken

makeCluster(nThreads)
snow.time(len_res <- parLapply(clus,input_content,nchar))
stopCluster(clus)
input_content <- input_content[len_res>0]
input_content <- as.character(input_content)

# WARNING: THIS LINE BELOW WILL FREEZE THE SYSTEM!!!! Func will return out of memory cause file's too big
# installDict(paste(dict_dir,"/standard.txt",sep = ""), dictname = "standard.txt") #搜狗标准词库 
installDict(
  paste(dict_dir,"/meme_words.txt",sep = ""), dictname = "meme_words.txt"
) #搜狗拼音网络流行新词词库
installDict(
  paste(dict_dir,"/celebrities.scel",sep = ""), dictname = "celebrities.scel", dicttype = "scel"
) #搜狗拼音名人词库
installDict(
  paste(dict_dir,"/singer.scel",sep = ""), dictname = "singer.scel", dicttype = "scel"
) #搜狗拼音歌手词库
installDict(
  paste(dict_dir,"/korean fan.scel",sep = ""), dictname = "korean fan.scel", dicttype = "scel"
) #搜狗韩粉必备词库
installDict(
  paste(dict_dir,"/korean meme.scel",sep = ""), dictname = "korean meme.scel", dicttype = "scel"
) #搜狗韩国流行语词库
installDict(paste(dict_dir,"/popular.txt",sep = ""), dictname = "popular.txt") #搜狗拼音热词词典
installDict(
  paste(dict_dir,"/hua_qian_gu.scel",sep = ""), dictname = "花千骨词库大全.scel",dicttype =
    "scel"
) #搜狗拼音花千骨词典
installDict(paste(dict_dir,"/xuan_feng_shao_nv.scel",sep = ""), dictname = "旋风少女.scel",dicttype =
              "scel") #搜狗拼音旋风少女词典
installDict(
  paste(dict_dir,"/baba_qu_na_er_di_san_ji.scel",sep = ""), dictname = "《爸爸去哪儿第三季》词库.scel",dicttype =
    "scel"
) #搜狗拼音"爸3"词典
installDict(
  paste(dict_dir,"/re_men_zong_yi.scel",sep = ""), dictname = "re_men_zong_yi.scel",dicttype =
    "scel"
) #搜狗拼音"热门综艺"词典
installDict(
  paste(dict_dir,"/gang_tai_ou_xiang_ju.scel",sep = ""), dictname = "gang_tai_ou_xiang_ju.scel",dicttype =
    "scel"
) #搜狗拼音"港台偶像剧"词典

library(Rwordseg)
con <- file(SegWordsFile, encoding = "UTF-8")
seg_words <- readLines(con)
insertWords(seg_words,save=TRUE)
close(con)

segword.start.time <- Sys.time()
library(snow)
clus <- makeCluster(nThreads)
clusterExport(clus,"seg_words")
detach("package:Rwordseg", unload = TRUE) #jvm can't be forked!!  check out:http://stackoverflow.com/questions/24337383/unloading-rjava-and-or-restarting-jvm
snow.time(sample.words <- parLapply(clus, input_content, function(x) {
  library(Rwordseg)
  #.jinit()
  #insertWords(seg_words) # hang the worker!!!!!!!!
  segment.options(isNameRecognition = TRUE) # 设置人名识别
  segmentCN(x, returnType = 'tm', nosymbol = TRUE) 
}))
stopCluster(clus)
# system.time (sample.words <-
#                segmentCN(
#                  input_content, returnType = 'tm', nosymbol = TRUE
#                ))
segword.end.time <- Sys.time()
segword.time.taken <- segword.end.time - segword.start.time
cat("分词计算耗时:")
print(segword.time.taken)
cat("\n")
system.time (write.table(as.matrix(sample.words),"sample_segmentCN.txt"))
nTmpSum <- 0
# for (nSamLine in 1:length(sample.words)) {
#   ss_res <- strsplit(sample.words[[nSamLine]][1],"\\s+")
#   #cat( length(ss_res[[1]]) )
#   nTmpSum <- nTmpSum + length(ss_res[[1]])
# }
clus <- makeCluster(nThreads)
snow.time(lines_len <- parLapply(clus,sample.words,function(x) {
  ss_res <- strsplit(x[1],"\\s+")
  #cat( length(ss_res[[1]]) )
  length(ss_res[[1]])
}))
stopCluster(clus)
nTmpSum <- sum(unlist(lines_len))
print("分词后单词数:")
print(nTmpSum)
#dim(input_file)

mystopwords <-
  unlist (read.table(
    StopWordsFile, encoding = "UTF-8",stringsAsFactors = F
  ))

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
#sample.clean <- lapply( sample.words, removeStopWords, mystopwords )
clus <- makeCluster(nThreads)
exp_fun <- c("removeStopWords")
clusterExport(clus,exp_fun)
snow.time( sample.clean <- parLapply(clus, sample.words, removeStopWords, mystopwords) )
## remove empty entries and empty rows
sample.clean <-
  parLapply(clus, sample.clean, function(x)
    x <- unlist(x))
stopCluster(clus)
sample.clean <- sample.clean[lapply(sample.clean,length)>0]
swords.end.time <- Sys.time()
swords.time.taken <- swords.end.time - swords.start.time
cat("删除停用词耗时:")
print(swords.time.taken)
cat("\n")
system.time ( write.table(as.matrix(sample.clean),"sample_clean_rsw.txt") )


con <- file(SynWordsFile, encoding = "UTF-8")
synonyms <- readLines(con)
synonyms <- synonyms[!sapply(synonyms, is.null)]
synonyms_ss <- strsplit(synonyms,"\\s+")
# read the synonyms dict from file
readFirstField <- function(x) {
  ret <- NULL
  #cat(paste(x[1],"\n"))
  grep_res <- grep("^\\s*#", x[1])
  # skip comment
  if (length(grep_res)) {
    #cat(grep_res)
    return(ret)
  }
  x[[1]][1] # return first column
}
removeFirstField <- function(x) {
  ret <- NULL
  grep_res <- grep("^\\s*#", x[1])
  # skip comment
  if (length(grep_res)) {
    #cat(grep_res)
    return(ret)
  }
  nLen <- length(x)
  index <- 1
  while (index <= nLen) {
    if (index != 1)
      ret <- c(ret,x[index])
    index <- index + 1
  }
  ret
}
syn_names <-
  lapply(synonyms_ss, readFirstField) # first column as subsitution word
syn_names <- syn_names[!sapply(syn_names, is.null)]
synonyms <- lapply(synonyms_ss, removeFirstField)
synonyms <- synonyms[!sapply(synonyms, is.null)]
names(synonyms) <- syn_names
close(con)

StemSynonym = function(line,stemword_list) {
  index <- 1
  it_max <-
    length(line) # this fucking index scheme is killing me!!!!!!
  #cat(it_max)
  #cat("\n")
  nStemLen <- length(stemword_list)
  # cat(nStemLen)
  # cat("\n")
  sub_res <- ret <- character(0)
  sub_names <- names(stemword_list)
  while (index <= it_max) {
    #cat(line[index])
    #cat("\n")
    #cat(paste(index,":",line[index]," ",sep=""))
    break_flag <- FALSE
    sub_res <- line[index]
    for (i in 1:nStemLen) {
      nWord <- length(stemword_list[[i]])
      for (j in 1:nWord) {
        #cat( paste(":",stemword_list[[i]][j],"\n") )
        # cat(line[index])
        # cat(" vs ")
        # cat(stemword_list[[i]][j])
        # cat("\n")
        if (line[index] == stemword_list[[i]][j]) {
          sub_res <- sub_names[[i]]
          cat(paste(
            stemword_list[[i]][j],"->",sub_names[[i]],":",
            paste(line,collapse = " "),"\n"
          ))
          break_flag = TRUE
          break
        }
      }
      if (break_flag)
        break
    }
    ret <- c(ret, sub_res)
    index <- index + 1
  }
  #cat(paste("index:",index,"it_max",it_max,"\n"))
  ret
}

stem.start.time <- Sys.time()
clus <- makeCluster(nThreads)
exp_var <- c("StemSynonym","synonyms")
clusterExport(clus,exp_var)
snow.time(sample.clean <-
            parLapply(clus, sample.clean, StemSynonym, synonyms))
stopCluster(clus)
stem.end.time <- Sys.time()
stem.time.taken <- stem.end.time - stem.start.time
cat("替换同义词耗时：")
print(stem.time.taken)
cat("\n")
system.time (write.table(as.matrix(sample.clean),"sample_clean_stem.txt"))

print_wordcut_line <-
  function(word_list, file_name, append = TRUE, sep = " ") {
    cat(unlist(word_list), file = "", sep = " ", fill = TRUE)
  }

sink(wordcut_res_file)
for (i in 1:length(sample.clean))
  print_wordcut_line(sample.clean[[i]], wordcut_res_file)
sink()

library(hash)
library(snow)
clus <- makeCluster(nThreads)
snow.time(word_freq_list <-
            parLapply(clus, sample.clean, function(x) {
              line <- unlist(strsplit(x,"\\s+"))
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
              list(word_voc)
            }))
stopCluster(clus)
# lapply(sample.clean[1:100], function(x){
#   line <- unlist(strsplit(x,"\\s+"))
#   word_voc <- NULL
#   w_len <- length(line)
#   for (i in 1:w_len){
#     if (line[i] %in% names(word_voc)) {
#       word_voc[line[i]] <- word_voc[line[i]]+1
#       browser()
#     }
#     else {
#       #browser()
#       word_voc <- c(word_voc,1)
#       names(word_voc)[length(word_voc)] <- line[i]
#     }
#   }
#   word_voc
# })
clus <- makeCluster(nThreads)
snow.time( w_names <-
   parLapply(clus,word_freq_list,function(x) {
    names(x[[1]])}) )
stopCluster(clus)
word_voc <- sort(unique(unlist(w_names)))
voc_size <- length(word_voc)
idx_list <- list(seq(1:voc_size))
names(idx_list[[1]]) <- word_voc
idx_list <- unlist(idx_list)
i_list <- NULL
j_list <- NULL
v_list <- NULL
nDoc <- length(word_freq_list)
# library(Matrix)
# slist.start.time <- Sys.time()
# clus <- makeCluster(nThreads)
# exp_var <- c("word_freq_list","idx_list")
# system.time(clusterExport(clus,exp_var))
# snow.time(sparse_aggr <-
#             parLapply(clus,seq_along(word_freq_list)[1:280],function(i) {
#               i_list <- NULL
#               j_list <- NULL
#               v_list <- NULL
#               line <-
#                 strsplit(names(unlist(word_freq_list[[i]])),"\\s") # R语言的嵌套list，谁用谁晕死
#               freq <- unlist(word_freq_list[[i]])
#               word_num <- length(line)
#               i_list <- c(i_list,rep(i,word_num))
#               for (j in 1:word_num) {
#                 word <- line[[j]]
#                 idx <- idx_list[word]
#                 names(idx) <- NULL
#                 j_list <- c(j_list,idx)
#                 #browser()
#                 cat(names(freq[j]))
#                 names(freq[j]) <- NULL
#                 cat(names(freq[j]))
#                 v_list <- c(v_list,freq[j])
#                 c(list(i_list),list(j_list),list(v_list))
#               }
#             }))
# stopCluster(clus)
# slist.end.time <- Sys.time()
# slist.time.taken <- slist.end.time - slist.start.time
# cat("生成sparse matrix列表耗时：")
# print(slist.time.taken)
# cat("\n")

library(synchronicity)
slist.start.time <- Sys.time()
i_m <- boost.mutex()
j_m <- boost.mutex()
v_m <- boost.mutex()
# library(doParallel)
# clus <- makeCluster(nThreads)
# registerDoParallel(clus)
# for (i in 2:nDoc) { # stuck with 3m rows!!!!
# exp_var <- c("lock","unlock")
foreach (i = 2:nThreads * 100000,.verbose = T,.export = exp_var)  %dopar% {
  line <-
    strsplit(names(unlist(word_freq_list[[i]])),"\\s") # R语言的嵌套list，谁用谁晕死
  freq <- unlist(word_freq_list[[i]])
  names(freq) <- NULL
  word_num <- length(line)
  lock(i_m)
  i_list <- c(i_list,rep(i,word_num))
  unlock(i_m)
  for (j in 1:word_num) {
    word <- line[[j]]
    idx <- idx_list[word]
    names(idx) <- NULL
    lock(j_m)
    j_list <- c(j_list,idx)
    unlock(j_m)
    lock(v_m)
    v_list <- c(v_list,freq[j])
    unlock(v_m)
  }
}
# stopCluster(clus)
slist.end.time <- Sys.time()
slist.time.taken <- slist.end.time - slist.start.time
cat("生成sparse matrix列表耗时：")
print(slist.time.taken)
cat("\n")


tm.start.time <- Sys.time()
library(tm)
#vec_src <- lapply(sample.clean, function(x) x <- paste(x,collapse=" "))
#corpus <- Corpus(VectorSource(vec_src))
corpus <- Corpus(VectorSource(sample.clean))
library(SnowballC)
corpus <- tm_map(corpus, stripWhitespace)   # *Stripping whitespace
# corpus <-
#   tm_map(corpus, stemDocument)   # *Removing common word endings* (e.g., "ing", "es") # extremly slow if data is big

#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, content_transformer(tolower))
#system.time(corpus <- tm_map(corpus, removeWords, stopwords("english")))
#library(tmcn)
#system.time(corpus <- tm_map(corpus, removeWords, stopwordsCN()))
#corpus <- tm_map(corpus, PlainTextDocument)

system.time(dtm <-
              DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf))))
#   system.time(dtm <-
#                 DocumentTermMatrix(corpus, control = list(
#                   wordLengths = c(2, Inf), stopwords = mystopwords
#                 )))

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
all_words <- term.statistics(dtm)
tm.end.time <- Sys.time()
tm.time.taken <- tm.end.time-tm.start.time
cat("tm操作耗时：")
print(tm.time.taken)
cat("\n")



# read.start.time <- Sys.time()
# input_table <- read.table("~/barrage/第三周综合全utf8.txt",quote = "\"",fileEncoding = "UTF-8")
# read.end.time <- Sys.time()
# read.time.taken <- read.end.time - read.start.time
# read.time.taken

# input_file_list <-
#   t( read.table(data_list_file, stringsAsFactors = F) )
# nInLen <- length(input_file_list)
# nRealLen <- nInLen*3
# real_file_list <- vector("list",nRealLen)
# for (i in 1:nInLen) {
#   for (j in 0:2){
#     file_name <- paste(input_file_list[i],"_",data_file_suffix[j+1],".csv",sep = "")
#     real_file_list[i*3-j] <- file_name
#   }
# }
# real_file_list <- unlist(real_file_list)
# 
# test_file <- read.csv(real_file_list[1],fileEncoding = "UTF-8",header = F, stringsAsFactors = F,quote = "\"")
# length(test_file$V1)
# 
# if (nRealLen < nThreads)
#   nThreads <- nRealLen
# library(snow)
# clus <- makeCluster(nThreads)
# parLapply(clus,real_file_list,function(x) {
#   read.csv(x,fileEncoding = "UTF-8",header = F, stringsAsFactors = F,quote = "\"")
# })
# stopCluster(clus)
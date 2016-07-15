r_file <- "classifier_test.R"
# r_file <- "lr_combo_test.R"
t1 <-
  try(system(paste("Rscript", r_file, "--max-ppsize 500000"), intern = T)
  )
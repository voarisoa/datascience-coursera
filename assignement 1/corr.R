
corr <- function(directory, threshold = 0) {
  
  allfile <- list.files(path = directory, full.names = TRUE)
  
  data <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(allfile)) {
    moni_i <- read.csv(allfile[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      data <- c(data, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  data
}

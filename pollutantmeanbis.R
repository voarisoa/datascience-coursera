pollutantmean <- function(directory, pollutant, id=1:332){
   directory <- paste(getwd(), "/", directory, "/", sep = " ")
   file_list <- list.files(directory)
   data <- NA
   
   for (i in id) {
     file_dir <- paste(directory, file_list[i], sep =" ")
     file_data <- read.csv(file_dir)
     data <- rbind(data, file_data)
     
   }
  
  mean(data[[pollutant]], na.rm = TRUE)
  
  
}


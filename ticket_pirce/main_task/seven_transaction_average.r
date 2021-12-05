pacman::p_load(tidyverse, #data cleaning
               janitor, # for clean_names
               reshape2, # for melt 
               msm,# for delta method
               readxl,# read_excel
               stargazer, # regression table making 
               scales, # percent 
               kableExtra, # table making 
               data.table,# sometimes useful
               zoo) # rollapply
setwd(dirname(rstudioapi::getSourceEditorContext()$path))



data_seven_transaction_smoothing <- function(data){
    data_length <- length(data)
    data_seven_transaction_average <- rep(NA, data_length)
    data_seven_transaction_average[(1+3):(data_length-3)] <- rollapply(data, width = 7, by = 1, FUN = mean, align = "right")
    data_seven_transaction_average[1+2] <- mean(data[1:6], na.rm=T)
    data_seven_transaction_average[data_length-2] <- mean(data[(data_length-5):data_length], na.rm=T)
    data_seven_transaction_average[1+1] <- mean(data[1:5], na.rm=T)
    data_seven_transaction_average[data_length-1] <- mean(data[(data_length-4):data_length], na.rm=T)
    data_seven_transaction_average[1] <- mean(data[1:4], na.rm=T)
    data_seven_transaction_average[data_length] <- mean(data[(data_length-3):data_length], na.rm=T)
  return(data_seven_transaction_average)
}

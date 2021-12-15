pacman::p_load(tidyverse, #data cleaning
               janitor, # for clean_names
               reshape2, # for melt 
               msm,# for delta method
               readxl,# read_excel
               stargazer, # regression table making 
               scales, # percent 
               kableExtra, # table making 
               data.table) # sometimes useful
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# this function is similar to table_region_move. it takes same inputs and gives same outputs.
# The key difference is that I manually define that 
# is_urban when urban == 1, rural when urban == 0, and I treat 2 as NA because
# urban == 2 only has 0.71% and I don't know what does 2 represent.

table_urban_move <- function(data, column){
  years <- c(1979:1993, seq(1994, 2012, by = 2))
  table <- tibble(id = rep(NA,12686), move_count = rep(NA,12686))
  table <- table %>% add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years))
  for(i in 1:12686){
    temp <- data %>% filter(id == i)
    action_year <- c()
    count <- 0
    # right here we let urban == 2 be the NA
    # temp <- bfi_data %>% filter(id == 1)
    for(k in 1:25){
      if(isTRUE(as.numeric(temp[k, column]) == 2)){
        temp[k, column] <- NA
      }
    }
    origin <- as.numeric(temp[1, column])
    m <- 1
    while(is.na(origin)){
      origin <- as.numeric(temp[m, column])
      m = m + 1
      if(m == 26){
        break
      }
    }
    for(j in 2:25){
      if(!is.na(as.numeric(temp[j,column]))){
        if(isTRUE(as.numeric(temp[j,column]) != origin)){
          origin <- as.numeric(temp[j,column])
          count <- count + 1
          action_year <- c(action_year, as.numeric(temp[j,8]))
        }
      }
    }
    table$id[i] <- i
    table$move_count[i] <- count
    for(k in 1:length(action_year)){
      for(j in 3:27){ 
        year_col <- as.numeric(colnames(table)[j])
        if(isTRUE(year_col == action_year[k])){
          table[i,j] <- 1
        }
      }
    }
  }
  return(table)
}











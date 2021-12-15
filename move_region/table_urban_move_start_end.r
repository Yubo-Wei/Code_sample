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

# this function consider the move direction from start to end


table_urban_move_start_end <- function(data, column, start = 0, end = 1){
  years <- c(1979:1993, seq(1994, 2012, by = 2))
  # create empty table
  table <- tibble(id = rep(NA,12686), move_count = rep(NA,12686))
  table <- table %>% add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years))
  for(i in 1:12686){
    temp <- data %>% filter(id == i)
    action_year <- c()
    origin <- as.numeric(temp[1, column])
    m <- 1
    for(k in 1:25){
      if(isTRUE(as.numeric(temp[k, column]) == 2)){
        temp[k, column] <- NA
      }
    }
    
    while(is.na(origin)){
      origin <- as.numeric(temp[m, column]) 
      m = m + 1
      if(m == 26){
        break
      }
    }
    
    # we only care about origin == start
    m <- 1
    n <- 2
    if(!is.na(origin)){ # make sure origin is not NA
      while(isTRUE(origin != start)){ # we only care about origin ==start
        origin <- as.numeric(temp[m, column])
        n = m # this the index that start from 
        m = m + 1
        if(m == 26){
          break
        }
      }
    }
    count <- 0
    for(j in n:25){
      if(!is.na(as.numeric(temp[j,column]))){
        if(isTRUE(as.numeric(temp[j,column]) == end)){ # only matches with  == end
          if(isTRUE(origin == start)){ # only matches with origin == start
            count <- count + 1
            action_year <- c(action_year, as.numeric(temp[j,8]))
          }
        }
        origin <- as.numeric(temp[j,column])
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

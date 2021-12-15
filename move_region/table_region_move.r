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

# this function is made for bfi data test question 1
# it takes data and column_order as input, and it gives a detailed table



table_region_move <- function(data, column){
  years <- c(1979:1993, seq(1994, 2012, by = 2))
  # create empty table
  table <- tibble(id = rep(NA,12686), move_count = rep(NA,12686))
  table <- table %>% add_column(!!!set_names(as.list(rep(NA, length(years))),nm=years))
  for(i in 1:12686){
    # first to subset the data to current id
    temp <- data %>% filter(id == i)
    action_year <- c()
    origin <- as.numeric(temp[1, column])
    # need to consider the case that the first term is NA
    # so we add a while loop to solve this
    m <- 1
    while(is.na(origin)){
      origin <- as.numeric(temp[m, column]) # if origin = NA, go to the next
      m = m + 1
      # some may all be NAs, need to break at some point.
      if(m == 26){
        break
      }
    }
    count <- 0
    # then compare j-1 th element of column with jth element 
    # get count, and action_year
    for(j in 2:25){
      if(!is.na(as.numeric(temp[j,column]))){
        if(isTRUE(as.numeric(temp[j,column]) != origin)){
          origin <- as.numeric(temp[j,column])
          count <- count + 1
          action_year <- c(action_year, as.numeric(temp[j,8]))
        }
      }
    }
    # start to assign the data to the table.
    table$id[i] <- i
    table$move_count[i] <- count
    # the following loop write the `move year` to the table.
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

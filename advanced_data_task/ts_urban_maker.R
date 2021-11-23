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


ts_urban_maker <- function(data){
  years <- c(1979:1993, seq(1994, 2012, by = 2))
  compare_with_move_year <- tibble(year = NA, before = NA, after = NA)
  for(i in 2:25){
    null_table <- tibble(id = NA, birth = NA, gender = NA, race = NA, region = NA, urban = NA, wage = NA, year= NA, educ = NA, employment = NA)
    null_table1 <- null_table
    null_table2 <- null_table
    temp <- data %>% filter(year == years[i] & indicator == 1)
    for(j in 1:nrow(temp)){
      temp2 <- bfi_data %>% 
        filter(id == as.numeric(temp[j,1])) %>%
        filter(year %in% c(years[i-2], years[i-1], years[i], years[i+1]))
      before <- temp2[1:(which(temp2[,8] ==  years[i])-1),] 
      after <- temp2[(which(temp2[,8] ==  years[i])):nrow(temp2),]
      null_table1 <- rbind(null_table1, before)
      null_table2 <- rbind(null_table2, after)
    }
    # finially we got mean_wage_before and after for year[i]
    mean_wage_before <- null_table1 %>% summarise(mean(wage, na.rm = T)) %>% as.numeric() 
    mean_wage_after <- null_table2 %>% summarise(mean(wage, na.rm = T)) %>% as.numeric()
    table <- tibble(year = years[i], before = mean_wage_before, after = mean_wage_after)
    compare_with_move_year <- rbind(compare_with_move_year, table)
  }
  a <- compare_with_move_year %>% slice(-1)
  compare_with_move_year_ts = select(melt(a , id = 1:2, measure = c("before", "after")), -before)
  return(compare_with_move_year_ts)
}
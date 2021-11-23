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

# this function is for Question 1, part 4
# it takes the data, and outputs a time series table for urban

mean_wages_emp_edu_ts_for_urban <- function(data){
  a <- tibble(year = NA, mean_wages = NA, mean_employment = NA, mean_education = NA, urban = NA)
  for(i in 0:1){
    b <- bfi_data %>% 
      group_by(year) %>%
      filter(urban == i) %>%
      summarise(mean_wages = mean(wage, na.rm = T),
                mean_employment = mean(employment, na.rm = T),
                mean_education = mean(educ, na.rm = T),
                urban = i)
    a <- rbind(a,b)
  }
  
  mean_wages_emp_edu_ts <- a %>% 
    slice(-1) %>% 
    arrange(year) %>% 
    mutate(urban = as.character(urban)) # this is for group plot
  return(mean_wages_emp_edu_ts)
}
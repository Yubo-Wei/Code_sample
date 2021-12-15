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

# this function is for Question 1, part 3
# it takes the data, and outputs a time series table for region

mean_wages_emp_edu_ts_for_region <- function(data){
  a <- tibble(year = NA, mean_wages = NA, mean_employment = NA, mean_education = NA, region = NA)
  for(i in 1:4){
    b <- bfi_data %>% 
      group_by(year) %>%
      filter(region == i) %>%
      summarise(mean_wages = mean(wage, na.rm = T),
                mean_employment = mean(employment, na.rm = T),
                mean_education = mean(educ, na.rm = T),
                region = i)
    a <- rbind(a,b)
  }
  
  mean_wages_emp_edu_ts <- a %>% 
    slice(-1) %>% 
    arrange(year) %>% 
    mutate(region = as.character(region)) # this is for group plot
  return(mean_wages_emp_edu_ts)
}

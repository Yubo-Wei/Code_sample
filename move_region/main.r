rm(list = ls())
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
bfi_data <- read_csv("nlsy79-prepared.csv")
# change the name of the first column to "id" to avoid confusion in loops.
colnames(bfi_data)[1] <- "id"  
bfi_data <- bfi_data %>% mutate(employment = ifelse(wage >0, 1, 0)) # create employment column
 
bfi_data %>% # check to see the data
  group_by(urban) %>%
  summarize(n = n())

#### Question 1  ####

# Report the count of moves across US regions in the sample 
# for each possible transitions

# For ideal results, we need label `times of move` and `year of move`,
# "move direction" and binding with other variables are not easy to present.

source("table_region_move.r") 
# we use this function to make a table contains info about move_count and move_year
# if we want region column, we set column = 5
region_table <- table_region_move(data = bfi_data, column = 5) # it takes 1 min to run.

region_table %>% summarise(sum_region = sum(move_count))

# then we want to see a time series data.
region_ts <- region_table%>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator")

moves_ts <- region_ts %>%
  group_by(year) %>%
  summarize(count_move = sum(indicator, na.rm = T)) %>%
  slice(-1) # remove the first row since 1979 is the first year.

ggplot(data = moves_ts, aes(as.numeric(year),count_move))+
  geom_line()+
  ggtitle("Trends In the Number of Region Move Over the Last 30 Years") +
  ylab("Move Count")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(moves_ts$year)), max(as.numeric(moves_ts$year)), by = 4),1))+
  theme_light()

# I manually define that 
# is_urban when urban == 1, rural when urban == 0, and I treat 2 as NA because
# urban == 2 only has 0.71% and I don't know what does 2 represent.
# similar function to table_region_move.r, see detailed explanation in function R fle.
source("table_urban_move.r") 
urban_table <- table_urban_move(data = bfi_data, column = 6) # it takes 1 min

urban_table %>% summarise(sum_urban = sum(move_count))
urban_ts <- urban_table%>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator")

moves_ts <- urban_ts %>%
  group_by(year) %>%
  summarize(count_move = sum(indicator, na.rm = T)) %>%
  slice(-1) # remove the first row since 1979 is the first year.

ggplot(data = moves_ts, aes(as.numeric(year),count_move))+
  geom_line()+
  ggtitle("Trends In the Number of Urban Move Over the Last 30 Years") +
  ylab("Move Count")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(moves_ts$year)), max(as.numeric(moves_ts$year)), by = 4),1))+
  theme_light()



bfi_data$wage[bfi_data$wage == 4115087] <- NA # this number is clearly inappropriate and it appears mutiple times in 1992
# part 3. region
# create time series table
source("mean_wages_emp_edu_ts_for_region.r")
region_mean_wages_emp_edu_ts <- mean_wages_emp_edu_ts_for_region(bfi_data)

ggplot(data = region_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_wages, color = region))+
  geom_line()+
  ggtitle("Trends In Mean Wages of 4 Regions Over the Last 30 Years") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(region_mean_wages_emp_edu_ts$year)), max(as.numeric(region_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()
ggplot(data = region_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_employment, color = region))+
  geom_line()+
  ggtitle("Trends In Mean Employment of 4 Regions Over the Last 30 Years") +
  ylab("Mean Employment")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(region_mean_wages_emp_edu_ts$year)), max(as.numeric(region_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()
ggplot(data = region_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_education, color = region))+
  geom_line()+
  ggtitle("Trends In Mean Education of 4 Regions Over the Last 30 Years") +
  ylab("Mean Education")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(region_mean_wages_emp_edu_ts$year)), max(as.numeric(region_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()

# part 4. urban/nonurban
source("mean_wages_emp_edu_ts_for_urban.r")
urban_mean_wages_emp_edu_ts <- mean_wages_emp_edu_ts_for_urban(bfi_data)

ggplot(data = urban_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_wages, color = urban))+
  geom_line()+
  ggtitle("Trends In Mean Wages of In Urban/Rural Over the Last 30 Years") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(urban_mean_wages_emp_edu_ts$year)), max(as.numeric(urban_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()
ggplot(data = urban_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_employment, color = urban))+
  geom_line()+
  ggtitle("Trends In Mean Employment In Urban/Rural Over the Last 30 Years") +
  ylab("Mean Employment")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(urban_mean_wages_emp_edu_ts$year)), max(as.numeric(urban_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()
ggplot(data = urban_mean_wages_emp_edu_ts, aes(as.numeric(year),mean_education, color = urban))+
  geom_line()+
  ggtitle("Trends In Mean Education In Urban/Rural Over the Last 30 Years") +
  ylab("Mean Education")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(urban_mean_wages_emp_edu_ts$year)), max(as.numeric(urban_mean_wages_emp_edu_ts$year)), by = 4),1))+
  theme_light()


#### Question 2 #####
# to get a clear view of the result, now we only focus on the movers from region 3 to region 1 
# first we need to find (3 to 1) movers and the years, create a table similar to question 1
# next is (2 to 4)

# Part 1
# 3 to 1
source("table_region_move_start_end.r") # focus on region 3 to 1
region_start_end <- table_region_move_start_end(data = bfi_data, column = 5, start = 3, end = 1 )

region__start_end_ts <- region_start_end%>%
  filter(move_count > 0) %>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator") %>%
  arrange(year)

source("ts_region_maker.R")
compare_with_move_year_ts <- ts_region_maker(data = region__start_end_ts)

ggplot(data = compare_with_move_year_ts , aes(as.numeric(year),value, color = variable))+
  geom_line()+
  ggtitle("Trends In Before and After Mean Wages of Region Movers (3 to 1)") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(compare_with_move_year_ts$year)), max(as.numeric(compare_with_move_year_ts$year)), by = 4),1))+
  theme_light()

# 2 to 4
region_start_end2 <- table_region_move_start_end(data = bfi_data, column = 5, start = 2, end = 4)
region__start_end_ts2 <- region_start_end2 %>%
  filter(move_count > 0) %>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator") %>%
  arrange(year)
compare_with_move_year_ts2 <- ts_region_maker(data = region__start_end_ts2)

ggplot(data = compare_with_move_year_ts2 , aes(as.numeric(year),value, color = variable))+
  geom_line()+
  ggtitle("Trends In Before and After Mean Wages of Region Movers (2 to 4)") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(compare_with_move_year_ts2$year)), max(as.numeric(compare_with_move_year_ts2$year)), by = 4),1))+
  theme_light()

# Part 2
source("table_urban_move_start_end.r")
rural_to_urban <- table_urban_move_start_end(data = bfi_data, column = 6, start = 0, end = 1 )
urban_to_rural <- table_urban_move_start_end(data = bfi_data, column = 6, start = 1, end = 0 )

rural_to_urban_ts <- rural_to_urban%>%
  filter(move_count > 0) %>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator") %>%
  arrange(year)
urban_to_rural_ts <- urban_to_rural%>%
  filter(move_count > 0) %>%
  pivot_longer(cols = `1979`: `2012`, names_to = "year", values_to = "indicator") %>%
  arrange(year)
source("ts_urban_maker.R")
compare_rural_to_urban <- ts_urban_maker(data = rural_to_urban_ts)
compare_urban_to_rural <- ts_urban_maker(data = urban_to_rural_ts)

ggplot(data = compare_rural_to_urban , aes(as.numeric(year),value, color = variable))+
  geom_line()+
  ggtitle("Trends In Before and After Mean Wages of Urban Movers (Rural to Urban)") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(compare_rural_to_urban$year)), max(as.numeric(compare_rural_to_urban$year)), by = 4),1))+
  theme_light()

ggplot(data = compare_urban_to_rural , aes(as.numeric(year),value, color = variable))+
  geom_line()+
  ggtitle("Trends In Before and After Mean Wages of Urban Movers (Urban to Rural)") +
  ylab("Mean Wages")+
  xlab("Year") +
  scale_x_continuous(breaks = round(seq(min(as.numeric(compare_urban_to_rural$year)), max(as.numeric(compare_urban_to_rural$year)), by = 4),1))+
  theme_light()



#### Question 3  ####
# we are gonna still use the Q2 info, mover from 3 to 1
i = 3 # let's take a look at 1981
years <- c(1979:1993, seq(1994, 2012, by = 2))
null_table <- tibble(id = NA, birth = NA, gender = NA, race = NA, region = NA, urban = NA, wage = NA, year= NA, educ = NA, employment = NA)
null_table1 <- null_table
null_table2 <- null_table

temp <- region__start_end_ts %>% filter(year == years[i])
for(j in 1:nrow(temp)){
  temp2 <- bfi_data %>% 
    filter(id == as.numeric(temp[j,1])) %>%
    filter(year %in% c(years[i-2], years[i-1], years[i], years[i+1]))
  before <- temp2[1:(which(temp2[,8] ==  years[i])-1),] 
  after <- temp2[(which(temp2[,8] ==  years[i])):nrow(temp2),]
  null_table1 <- rbind(null_table1, before)
  null_table2 <- rbind(null_table2, after)
}
list_of_movers <- region__start_end_ts %>% filter(year == years[i] & indicator == 1) %>% select(id) %>% unlist() %>% as.numeric()

mean_wage_before <- null_table1 %>% group_by(id) %>% filter(year == years[i-1] & region == 3) %>% summarise(birth=birth,gender=gender, educ = educ, average_wage_before = mean(wage, na.rm = T))
mean_wage_before_and_indicator <-mean_wage_before  %>% mutate(mover_status = ifelse(id %in% list_of_movers ,1,0)) %>% ungroup()
linear_1 <- lm(data = mean_wage_before_and_indicator, 
   average_wage_before ~ mover_status + birth + gender + educ)


mean_wage_after <- null_table2  %>% group_by(id) %>% filter(year == years[i] & region == 1) %>% summarise(birth=birth,gender=gender, educ = educ, average_wage_after = mean(wage, na.rm = T))
mean_wage_after_and_indicator <-mean_wage_after  %>% mutate(mover_status = ifelse(id %in% list_of_movers ,1,0)) %>% ungroup()
linear_2 <- lm(data = mean_wage_after_and_indicator, 
   average_wage_after ~ mover_status + birth + gender + educ)

# now let's plot
source("stargazer.r")
resizebox.stargazer(linear_1, linear_2, 
                              title="Wage of Movers to the Wage of Stayers in Both Region of Origin and Region of Arrival.", 
                              type = 'latex',
                              column.labels = c("Before Moving", "After Moving"),
                              dep.var.labels.include = FALSE,
                              dep.var.caption = "",
                              column.sep.width = "1pt",
                              tab.width = "1.1\\textwidth",
                              tab.height = "1.1\\textheight")










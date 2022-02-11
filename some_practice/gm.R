rm(list = ls())
pacman::p_load(tidyverse, #data cleaning
               janitor, # for clean_names
               reshape2, # for melt 
               msm,# for delta method
               readxl,# read_excel
               stargazer, # regression table making 
               scales, # percent 
               kableExtra, # table making 
               data.table,# sometimes useful
               lubridate, # ymd
               Hmisc, # graphing
               randomForest, # randomforest
               zoo) # rollapply
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ht <- read_csv("test_data.csv") %>% mutate(survivors = ifelse(victims == 0, 0, 1))

q2 <- ht%>%group_by(survivors) %>% summarise(avg_income = mean(avg_income, na.rm = T))
q2$survivors <- c("without survivors", "with survivors")
ggplot(data = q2, aes(as.character(survivors), avg_income))+
  geom_bar(stat = "identity") + 
  theme_light() +
  labs(title = ("The relationship between average monthly income and trafficking status")) +
  xlab("trafficking status") +
  ylab("average monthly income")

q3 <- ht %>%
  mutate(the_sum = select(., starts_with("PBF")) %>% rowSums(na.rm = T)) %>%
  mutate(ever_received = ifelse(abs(the_sum) > 1, 1, 0)) %>%
  mutate(count_each_month = apply(select(., starts_with("PBF")),1,function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(receive_each_month = ifelse(count_each_month == 96, 1, 0)) %>%
  mutate(count_2012 = apply(select(., starts_with("PBF_2012")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2013 = apply(select(., starts_with("PBF_2013")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2014 = apply(select(., starts_with("PBF_2014")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2015 = apply(select(., starts_with("PBF_2015")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2016 = apply(select(., starts_with("PBF_2016")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2017 = apply(select(., starts_with("PBF_2017")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2018 = apply(select(., starts_with("PBF_2018")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_2019 = apply(select(., starts_with("PBF_2019")),1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(count_each_year = apply(select(., starts_with("count_201")), 1, function(x) sum(abs(x) > 1, na.rm = T))) %>%
  mutate(receive_each_year = ifelse(count_each_year == 8, 1, 0))
mean(q3$ever_received)
mean(q3$count_each_month)
mean(q3$receive_each_year)


a = q3[866,]

 # 4 
fit1 <- glm(data = q3, factor(survivors) ~ avg_income, family = "binomial")
fit2 <- glm(data = q3, factor(survivors) ~ avg_income + hhead_literate + hhead_age + 
              hhead_edu + hhead_migrant + hhead_foreign, family = "binomial")
fit3 <- glm(data = q3, factor(survivors) ~ avg_income + hhead_literate + hhead_age + 
              hhead_edu + hhead_migrant + hhead_foreign + kids + kids_under13 + 
              nadults + nmales + homeless_hhold + count_each_month + count_each_year, family = "binomial")

fit4 <- randomForest::randomForest(data = q3, survivors ~ count_each_month+count_each_year, importance = T)
summary(fit4)


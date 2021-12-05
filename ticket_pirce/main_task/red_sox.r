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
               zoo) # rollapply
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# deal with date and then combine.
red_sox_09 <- read_csv("red_sox_2009.csv") %>% 
  mutate(year = 9) %>%
  mutate(gamedate = as.Date(paste(gamedate, 9), "%b %d %y")) %>%
  mutate(transaction_date = as.Date(paste(str_replace(transaction_date, "_", " "), 9), "%m %d %y"))
red_sox_10 <- read_csv("red_sox_2010.csv") %>% 
  mutate(year = 10) %>%
  mutate(gamedate = as.Date(paste(gamedate, 10), "%b %d %y")) %>%
  mutate(transaction_date = as.Date(paste(str_replace(transaction_date, "_", " "), 10), "%m %d %y"))
red_sox_11 <- read_csv("red_sox_2011.csv") %>% 
  mutate(year = 11) %>%
  mutate(gamedate = as.Date(paste(gamedate, 11), "%b %d %y")) %>%
  mutate(transaction_date = as.Date(paste(str_replace(transaction_date, "_", " "), 11), "%m %d %y"))
red_sox_12 <- read_csv("red_sox_2012.csv") %>% 
  mutate(year = 12) %>%
  mutate(gamedate = as.Date(paste(gamedate, 12), "%b %d %y")) %>%
  mutate(transaction_date = as.Date(paste(str_replace(transaction_date, "_", " "), 12), "%m %d %y"))
# now combine them and calculate the date difference, notice that is different from the column days_from_transaction_until_game.
red_sox <- rbind(red_sox_09, red_sox_10, red_sox_11, red_sox_12) %>%
  mutate(diff = as.numeric(str_replace(difftime(gamedate, transaction_date, unit = "days"), " days", "")))

a = red_sox %>% 
  filter(gamedate == "2009-06-19" | gamedate == "2009-06-20" | gamedate ==  "2009-06-21")  %>%
  group_by(sectiontype) %>%
  summarise(n(), mean_price = mean(logprice, na.rm= T))

### examples 
# here we test it with the first game in the data and LowerBleachers seats.
first_test_data <- red_sox %>% 
  filter(gamedate == "2009-06-19" | gamedate == "2009-06-20" | gamedate ==  "2009-06-21") %>%
  filter(sectiontype == "LowerBleachers")
ggplot(data = first_test_data, aes(transaction_date, logprice))+
  geom_line()+
  ggtitle("Log Price of LowerBleachers Seats of the Game With ATL (June 2009)") +
  ylab("Log Price")+
  xlab("Date") +
  theme_light()
source("seven_transaction_average.r")
ggplot(data = first_test_data, aes(transaction_date, data_seven_transaction_smoothing(logprice)))+
  geom_line()+
  ggtitle("Log Price of LowerBleachers Seats of the Game With ATL (June 2009)") +
  ylab("Log Price (7 transaction smoothed)")+
  xlab("Date") +
  theme_light()
# another seat
first_test_data_another_seats <- red_sox %>% 
  filter(gamedate == "2009-06-19" | gamedate == "2009-06-20" | gamedate ==  "2009-06-21") %>%
  filter(sectiontype == "LogeBox")
ggplot(data = first_test_data_another_seats, aes(transaction_date, data_seven_transaction_smoothing(logprice)))+
  geom_line()+
  ggtitle("Log Price of LogeBox Seats of the Game With ATL (June 2009)") +
  ylab("Log Price (7 transaction smoothed)")+
  xlab("Date") +
  theme_light()

# other seats 
seats <- c("FieldBox", "RFGS")
first_test_data_4_seats <- red_sox %>% 
  filter(gamedate == "2009-06-19" | gamedate == "2009-06-20" | gamedate ==  "2009-06-21") %>%
  filter(sectiontype %in% seats)

ggplot(data = first_test_data_4_seats, aes(transaction_date, data_seven_transaction_smoothing(logprice), color = sectiontype ))+
  geom_line()+
  ggtitle("Log Price of FieldBox Seats and RFGS Seats of the Game With ATL (June 2009)") +
  ylab("Log Price (7 transaction smoothed)")+
  xlab("Date") +
  theme_light()





# Trend break analysis

first_test_data_trend_break <- first_test_data%>%
  mutate(start_with_Feb = abs((diff - max(diff)))) 
 # create this column to make trend break analysis.
# the coefficent doesn't make sense now but we only care about the R^2.
r <- c()
for(i in 0:max(first_test_data_trend_break$diff)){
  mydata <- first_test_data_trend_break %>% mutate(t_loop = ifelse(start_with_Feb - i >0, start_with_Feb - i ,0))
  linear <- lm(logprice ~ start_with_Feb + t_loop, data = mydata)
  r <- c(r, summary(linear)$r.squared)
  if (i == max(first_test_data_trend_break$diff)){
    answer <- which.max(r) 
  }
}

ggplot(data = first_test_data_trend_break, aes(transaction_date, data_seven_transaction_smoothing(logprice)))+
  geom_line()+
  ggtitle("Log Price of LowerBleachers Seats of the Game With ATL (June 2009)") +
  ylab("Log Price 7 transaction smooted")+
  xlab("Date") +
  geom_smooth(method='lm') +
  theme_light() +
  geom_vline(xintercept= as.Date(answer - 1+ as.Date("2009-02-01")), color = 'red') +
  geom_text(aes(x=as.Date(answer - 1+ as.Date("2009-02-01")), label= as.Date(answer - 1+ as.Date("2009-02-01")), y=3), colour="red",size = 4)

figure_tibble <- tibble(Date = as.Date(as.Date("2009-02-01", "%Y-%m-%d"):as.Date("2009-06-21", "%Y-%m-%d")), R_squared = r)
ggplot(data = figure_tibble)+
  geom_line(aes(Date, R_squared))+ 
  theme_light() %>%
  labs(title = ("The Trend Break Date that Maximizes the R2 of the Regression Model"))+
  ylab("R Squared") +
  xlab("Date") +
  geom_vline(xintercept= as.Date(answer - 1+ as.Date("2009-02-01")), color = 'red') +
  geom_text(aes(x=as.Date(answer - 1+ as.Date("2009-02-01")), label= as.Date(answer - 1+ as.Date("2009-02-01")), y= 0.18), colour="red",size = 4)

source("turning_point_maker.R")
turningPoint(data = red_sox, seats = c("LowerBleachers"),  game_startdate =  "2011-09-15", game_enddate = "2011-09-18")



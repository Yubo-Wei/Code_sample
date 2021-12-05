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


turningPoint <- function(data, seats, game_startdate, game_enddate){
  temp <- red_sox %>%
    filter(gamedate %in% c(as.Date(game_startdate) : as.Date(game_enddate))) %>%
    filter(sectiontype %in% seats) %>%
    mutate(start_with_Feb = abs((diff - max(diff))))
  r <- c()
  
  for(i in 0:max(temp$diff)){
    mydata <- temp %>% mutate(t_loop = ifelse(start_with_Feb - i >0, start_with_Feb - i ,0))
    linear <- lm(logprice ~ start_with_Feb + t_loop, data = mydata)
    r <- c(r, summary(linear)$r.squared)
    if (i == max(temp$diff)){
      answer <- which.max(r) 
    }
  }
  transaction_startdate <- sort(temp$transaction_date)[1]
  title <- paste("Log Price of Seats",paste0("(",seats,")"), "of the Game against", temp$team[1], ",", game_startdate, "to", game_enddate)
  figure_tibble <- tibble(Date = as.Date(as.Date(transaction_startdate) : as.Date(game_enddate)), R_squared = r)
  
  ggplot(data = temp, aes(transaction_date, data_seven_transaction_smoothing(logprice)))+
    geom_line()+
    ggtitle(title) +
    ylab("Log Price 7 transaction smooted")+
    xlab("Date") +
    geom_smooth(method='lm') +
    theme_light() +
    geom_vline(xintercept= as.Date(answer - 1+ as.Date(transaction_startdate)), color = 'red') +
    geom_text(aes(x=as.Date(answer - 1+ as.Date(transaction_startdate)), label= as.Date(answer - 1+ as.Date(transaction_startdate)), y= mean(logprice, na.rm = T)), colour="red",size = 2)
}






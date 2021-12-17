# R language.
 
rm(list = ls())
pacman::p_load(tidyverse, #data cleaning
               janitor, # for clean_names
               reshape2, # for melt 
               msm,# for delta method
               MASS, # rlm
               readxl,# read_excel
               stargazer, # regression table making 
               scales, # percent 
               kableExtra, # table making 
               data.table,# sometimes useful
               lubridate, # ymd
               Hmisc, # graphing
               usmap, # US map
               chron, # for time
               zoo) # rollapply
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


shift <- read_csv("test_data.csv") %>% 
  mutate(shift_date = as.Date(shift_date, "%d%b%Y")) %>% # take care of the date. 
  separate(arrive, c("arrive_date", "arrive_time"), sep = " ")  %>%
  mutate(arrive_date = as.Date(arrive_date, "%d%b%Y")) %>%
  separate(leave, c("leave_date", "leave_time"), sep = " ")  %>%
  mutate(leave_date = as.Date(leave_date, "%d%b%Y")) %>% 
  filter(pred_lnlos > 0) %>% # remove negative values.
  mutate(arrive_time = times(arrive_time),
         leave_time = times(leave_time)) %>% # format the time
  mutate(shift_start = gsub("\\.", "", shift_start),
         shift_end = gsub("\\.", "", shift_end)) %>% # remove period
  mutate(shift_start = ifelse(shift_start == "noon", "12 pm", shift_start),
         shift_end = ifelse(shift_end == "noon", "12 pm", shift_end))  %>% #remove noon
  mutate(shift_start = times(format(strptime(shift_start, "%I %p"), "%H:%M:%S")),
         shift_end = times(format(strptime(shift_end, "%I %p"), "%H:%M:%S")))  %>% #  convert to standard time.
  mutate(shift_nextday = ifelse(shift_end < shift_start, 1, 0)) %>%  # an indicator shows if the shift cross the date
  filter(!(arrive_date == leave_date & leave_time <= arrive_time)) # filter out those weird entries.

# 1 
shift_1 <- shift %>% 
  mutate(arrive_early = ifelse(arrive_date == shift_date & arrive_time < shift_start, 1, 0)) %>%# same day arrive
  mutate(leave_late = ifelse((shift_nextday == 0 & leave_date == shift_date & leave_time > shift_end)| # same day leave
                               (shift_nextday == 1 & leave_date > shift_date & leave_time > shift_end), 1, 0)) # next day leave
mean(shift_1$arrive_early)
mean(shift_1$leave_late)

# 2
shift_2 <- shift_1 %>%
  mutate(arrive_hours = hours(arrive_time)) # add hours of arrival
ggplot(data = shift_2, aes(x = arrive_hours)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color="black", fill="white") + 
  geom_density(alpha=0.2, fill="red") +
  scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) + # set x breaks
  theme_light() +
  labs(title = ("Hourly patterns of patient arrivals")) +
  xlab("Hours") +
  ylab("Density")


ggplot(data = shift_2 %>% 
  group_by(arrive_hours) %>% # get the average by hours
  summarise(average_severity = mean(pred_lnlos, na.rm = T)),
  aes(x = arrive_hours, y = average_severity)) +
  geom_line(color = "red") + 
  scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) + # set x breaks
  theme_light() +
  geom_smooth(method='lm') +
  labs(title = ("Hourly patterns of average severity")) +
  xlab("Hours") +
  ylab("Average Severity")

fit <- lm(data = shift_2 %>% 
            group_by(arrive_hours) %>% # get the average by hours
            summarise(average_severity = mean(pred_lnlos, na.rm = T)), average_severity ~ arrive_hours )


# part 3
census <- tibble(phys_id = NA, shift_date = as.Date(NA), shift_start = times(NA), shift_end = times(NA),
                 hour = NA, census = as.numeric(NA), census_lowerbound = as.numeric(NA), 
                 census_upperbound = as.numeric(NA)) # empty census
all_shifts <- shift_2 %>% 
  group_by(phys_id, shift_date, shift_start, shift_end, shift_nextday) %>%
  summarise(n()) # all distinct id and shifts 
for(k in 1:nrow(all_shifts)){
  temp <- shift_2 %>% # temp, filtered data for each id and shift
    filter(phys_id == all_shifts$phys_id[k] & shift_date == all_shifts$shift_date[k]) %>% 
    mutate(patient_nextday = ifelse(leave_time < arrive_time, 1, 0)) %>%
    arrange(arrive_time) # sort the arrive time.
  if(temp$shift_nextday[1] != 1){ # same day shift
    for(i in 1:(hours(temp$shift_end)[1] - hours(temp$shift_start)[1] + 4)){
      count <- 0 
      count_lower <- 0 
      count_upper <- 0
      for(j in 1:nrow(temp)){
        if(hours(temp$arrive_time[j]) <  hours(temp$shift_start[1]) + i){ # arrived 
          if(hours(temp$leave_time[j]) >=  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count <- count +1
          } else if(temp$patient_nextday[j] == 1 & hours(temp$leave_time[j]) + 24 > hours(temp$shift_start[1]) + i - 1){  # consider patient arrived but spent another day
            count <- count + 1
          }
        }
      }
      for(j in 1:nrow(temp)){
        if(hours(temp$arrive_time[j]) <  hours(temp$shift_start[1]) + i){ # arrived 
          if(hours(temp$leave_time[j]) >  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count_lower <- count_lower +1
          } else if(temp$patient_nextday[j] == 1 & hours(temp$leave_time[j]) + 24 > hours(temp$shift_start[1]) + i - 1){  # consider patient arrived but spent another day
            count_lower <- count_lower + 1
          }
        }
      }
      for(j in 1:nrow(temp)){
        if(hours(temp$arrive_time[j]) <=  hours(temp$shift_start[1]) + i){ # arrived 
          if(hours(temp$leave_time[j]) >=  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count_upper <- count_upper +1
          } else if(temp$patient_nextday[j] == 1 & hours(temp$leave_time[j]) + 24 > hours(temp$shift_start[1]) + i - 1){  # consider patient arrived but spent another day
            count_upper <- count_upper + 1
          }
        }
      }
      tibble <- tibble(phys_id = temp$phys_id[1], shift_date = temp$shift_date[1],
                       shift_start = temp$shift_start[1], shift_end = temp$shift_end[1],
                       hour = i, census = count, census_lowerbound = count_lower, 
                       census_upperbound = count_upper)
      census <- rbind(census, tibble)
    }
  } else { # the case two day shift 
  # temp for this case
    temp <- shift_2 %>% # temp, filtered data for each id and shift
      filter(phys_id == all_shifts$phys_id[k] & shift_date == all_shifts$shift_date[k]) %>% 
      mutate(patient_nextday = ifelse(leave_date !=  arrive_date, 1, 0)) %>%
      mutate(hours_arrive = hours(arrive_time)) %>%
      mutate(hours_arrive = ifelse(hours_arrive < 12, hours_arrive + 24, hours_arrive)) %>% # take care of the two day shift.
      mutate(hours_leave = hours(leave_time)) %>%
      mutate(hours_leave = ifelse(hours_leave < 12 | patient_nextday == 1, hours_leave + 24, hours_leave)) %>%
      mutate(hours_shift_end = hours(shift_end)) %>%
      mutate(hours_shift_end = ifelse(hours_shift_end < 12, hours_shift_end + 24, hours_shift_end)) %>%
      arrange(arrive_time) # sort the arrive time.
    for(i in 1:(hours(temp$shift_end)[1]+24 - hours(temp$shift_start)[1] + 4)){
      count <- 0 
      count_lower <- 0 
      count_upper <- 0
      for(j in 1:nrow(temp)){
        if(temp$hours_arrive[j] <  hours(temp$shift_start[1]) + i){ # arrived 
          if(temp$hours_leave[j] >=  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count <- count +1
          }
        }
      }
      for(j in 1:nrow(temp)){
        if(temp$hours_arrive[j] <  hours(temp$shift_start[1]) + i){ # arrived 
          if(temp$hours_leave[j] >  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count_lower <- count_lower +1
          }
        }
      }
      for(j in 1:nrow(temp)){
        if(temp$hours_arrive[j] <=  hours(temp$shift_start[1]) + i){ # arrived 
          if(temp$hours_leave[j] >=  hours(temp$shift_start[1]) + i - 1){ # not leaved yet
            count_upper <- count_upper +1
          }
        }
      }
      tibble <- tibble(phys_id = temp$phys_id[1], shift_date = temp$shift_date[1],
                       shift_start = temp$shift_start[1], shift_end = temp$shift_end[1],
                       hour = i, census = count, census_lowerbound = count_lower, 
                       census_upperbound = count_upper)
      census <- rbind(census, tibble)
    }
  }
}
census <- census[-1,]

ggplot(data = census %>% group_by(shift_end) %>% 
  summarise(n = mean(census)), aes(x = hours(shift_end), y = n))+
  geom_line(color = "red") + 
  scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) + # set x breaks
  theme_light() +
  geom_point()+
  labs(title = ("Hourly patterns of census with time relative to
end of shift")) +
  xlab("Hours") +
  ylab("Average Census")

# part 4
shift_4 <- shift_2 %>% 
  mutate(patient_nextday = ifelse(leave_date != arrive_date, 1, 0)) %>%
  mutate_at(c(7,9), as.numeric) %>% #convert to seconds. 
  mutate(leave_time = ifelse(patient_nextday == 1, leave_time + 1, leave_time)) %>% #if  patient tow day, add one more day to leave_time
  mutate(diff = (leave_time - arrive_time)*24*3600) # in seconds.

df <- shift_4 %>%
  group_by(phys_id) %>%
  summarise(average_stay = mean(diff, na.rm = T))
ggplot(data = df, aes(x = phys_id, y = average_stay))+
  geom_line(color = "red") + 
  scale_x_continuous(breaks = round(seq(1, 43, by = 1),1)) + # set x breaks
  theme_light() +
  geom_point()+
  labs(title = ("Average seconds that physicians spend on discharging patients.")) +
  xlab("Physicians") +
  ylab("Seconds")

linear_1 <- lm(data = shift_4, log(diff) ~ phys_id)
df3 <- shift_2 %>% 
  group_by(shift_end) %>%
  summarise(observations = n()) %>%
  mutate(share = observations/sum(observations))

shift_2%>%group_by(shift_end) %>%
  summarise(n())

# Define shifts that end at 4 am as empty shifts.

shift_5 <- shift_4 %>%
  mutate(empty_shift = ifelse(shift_end == "04:00:00", 1 ,0)) %>% 
  mutate(weekend = ifelse(weekdays(shift_date) %in% c("Saturday", "Sunday"), 1, 0))

linear_2 <- rlm(data = shift_5, log(diff) ~ phys_id + pred_lnlos + empty_shift + weekend)


write.csv(census, file = "census.csv")
  
  
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
scp <- read_csv("scp-1205.csv")


# the time open then instruction.pdf: Sat Dec 4 18:11


# start typing: 18:28
first_row <- colnames(scp)
colnames(scp) <- c("countyname", "state", "contract",  "healthplanname", 
                   "typeofplan", "countyssa", "eligibles", "enrollees",
                   "penetration", "ABrate")
scp <- rbind(first_row, scp)
# dealt with the first row


scp$eligibles[is.na(scp$eligibles)] <- 0
scp$enrollees[is.na(scp$enrollees)] <- 0
scp$penetration[is.na(scp$penetration)] <- 0
# dealt with NA

scp %>%
  group_by(state, countyname) %>%
  summarise(n()) # something weird 

# which(scp$state == 99)
scp_cleaned <- scp %>%
  filter(state != 99)
# data cleaned


# columns
numberofplans1 <- scp_cleaned %>% 
  group_by(state, countyname) %>%
  filter(enrollees > 10) %>%
  summarise(numberofplans1 = n()) 

numberofplans2 <- scp_cleaned %>% 
  group_by(state, countyname) %>%
  filter(penetration > 0.5) %>%
  summarise(numberofplans2 = n())

other_columns <- scp_cleaned %>% 
  group_by(state, countyname) %>%
  summarise(countyssa = mean(as.numeric(countyssa),na.rm= T),
            eligibles = mean(as.numeric(eligibles), na.rm = T), 
            totalenrollees = sum(as.numeric(enrollees), na.rm = T)) %>%
  mutate(totalpenetration = round(totalenrollees*100/eligibles, 2))

# combine them
 
temp <- right_join(numberofplans2, other_columns, by = c("state", "countyname"))
requested_data <- right_join(numberofplans1, temp, by = c("state", "countyname"))
requested_data$numberofplans1[is.na(requested_data$numberofplans1)] <- 0
requested_data$numberofplans2[is.na(requested_data$numberofplans2)] <- 0
requested_data <- requested_data%>% filter(state != "PR" & state != "GU")

# done at 7:01 requested_data is the result

# some issues: na



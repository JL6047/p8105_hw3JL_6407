library(tidyverse)
accel = read_csv(file = "./accel_data.csv")%>%
  janitor::clean_names()
accel = mutate( accel,weekday_vs_weekend = ifelse(day=="Monday","Weekday",
                                          ifelse(day=="Tuesday","Weekday",
                                                 ifelse(day=="Wednesday","Weekday",
                                                        ifelse(day=="Thursday","Weekday",
                                                               ifelse(day=="Friday","Weekday","Weekend"
                                                                      ))))))
accel_day = accel %>%
  mutate(day_beat=rowSums(accel_day[,c(4:1443)]))%>%
  select(week,day_id,day,weekday_vs_weekend,day_beat)

  
  ggplot(accel_day, aes(x = day, y = day_beat))+
    geom_point(aes(color = day), alpha = .5) 

         
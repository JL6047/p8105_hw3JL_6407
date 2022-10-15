library(p8105.datasets)
data("ny_noaa")

ny_noaa = ny_noaa %>% 
  janitor::clean_names()%>% 
  separate(date, c("year","month","day"))%>%
  mutate(tmax = as.numeric(tmax)/10) %>% 
  mutate(prcp = as.numeric(prcp)/10 ) %>% 
  mutate(tmin = as.numeric(tmin)/10 )

ny_noaa%>%
  filter(month=='01'|month=='07')%>%
  filter(!is.na(tmax)) %>% 
  group_by(month,year,id)%>%
  summarise(mean_month = mean(tmax))%>%
  ggplot(aes(
    x=factor(year),
    y=mean_month,
    color=year))+
  geom_point() +
  facet_grid(.~month)+
  ggtitle('Mean max temperature in January and July in each station across years')+
  theme(axis.text.x = element_text(angle = 90, hjust = 5))

ny_noaa%>%
  filter(!is.na(tmax),!is.na(tmin)) %>% 
  ggplot(aes(x = tmax, y = tmin)) + 
  geom_hex() +
  ggtitle('tmax vs tmin')

ny_noaa%>%
  filter(!is.na(snow)) %>%
  filter(snow>0&snow<100)%>%
  group_by(year)%>%
  ggplot(aes(x=snow,
             color=year))+
  geom_density()+
  ggtitle("Distribution of Snow Fall by year")


ny_noaa %>% 
  filter(!is.na(snow)) %>%
  filter(snow>0,snow<100) %>%
  ggplot(aes(x=snow, color=year))+
  geom_density()

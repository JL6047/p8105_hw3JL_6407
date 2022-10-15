library(p8105.datasets)
data("instacart")
skimr::skim(instacart)
head(instacart)

library(tidyverse)
library(ggridges)
ggplot(instacart, aes(x = user_id)) + geom_histogram()

insta = count(instacart,aisle)%>%
arrange(desc(n))
insta        


insta%>%
  filter(n > 10000) %>% 
  mutate(aisle = fct_reorder(aisle, n)) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  labs(title = "Number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

instacart%>%
  select(aisle,product_name)%>%
  filter(aisle=="baking ingredients")%>%
  count(product_name)%>%
  arrange(desc(n))%>%

instacart%>%
  select(aisle,product_name)%>%
  filter(aisle=="dog food care")%>%
  count(product_name)%>%
  arrange(desc(n))

instacart%>%
  select(aisle,product_name)%>%
  filter(aisle=="packaged vegetables fruits")%>%
  count(product_name)%>%
  arrange(desc(n))

instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow, value = mean_hour) %>%
  knitr::kable(digits = 2)

           
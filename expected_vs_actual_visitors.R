library(tidyverse)
library(lubridate)
library(ggplot2)

reservations <- read_csv("~/projects/learnathon_2018/recruit-restaurant-visitor-forecasting/air_reserve.csv")
visits <- read_csv("~/projects/learnathon_2018/recruit-restaurant-visitor-forecasting/air_visit_data.csv")

reservations %>% 
mutate(planned_date = date(visit_datetime)) %>%
select(air_store_id, planned_date, reserve_visitors) %>% 
group_by(air_store_id, planned_date) %>% 
summarize(expected_number_visitors = sum(reserve_visitors)) %>% 
left_join(visits, by = c("air_store_id" = "air_store_id", "planned_date" = "visit_date")) %>%
ggplot() + geom_point(mapping = aes(x = expected_number_visitors, y = visitors))
ggsave("visitors.png")

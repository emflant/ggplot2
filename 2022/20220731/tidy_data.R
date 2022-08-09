library(tidyverse)
library(readxl)
tb_car_accident = read_excel("~/github/ggplot2/2022/20220731/car_accident_2021.xlsx")
# tb_car_accident
tb_car_accident1 = tb_car_accident %>% 
  select(age_type = 사상자연령층,
         gender = 사상자성별,
         death = 사망자수)


# tb_car_accident1 %>%  distinct(gender)
tb_car_accident2 = tb_car_accident1 %>% 
  group_by(gender) %>% 
  summarise(death = sum(death)) %>% 
  mutate(rate = death / sum(death) * 100) %>% 
  filter(death > 0)

tb_car_accident3 = tb_car_accident1 %>% 
  filter(death > 0) %>% 
  group_by(age_type) %>% 
  mutate(rate = round(death / sum(death) * 100, 2)) %>% 
  ungroup() %>% 
  mutate(label = paste0(round(rate, 2), "%"))

tb_car_accident4 = tb_car_accident1 %>% 
  filter(death > 0) %>% 
  add_column(age_type2 = rep(c("20세이하", "21-40세", "41-60세", "61세이상"), each = 4))

tb_car_accident5 = tb_car_accident4 %>% 
  group_by(age_type2, gender) %>% 
  summarise(death = sum(death), .groups = "drop") %>% 
  group_by(age_type2) %>% 
  mutate(rate = round(death / sum(death) * 100, 2)) %>% 
  ungroup() %>% 
  mutate(label = paste0(round(rate, 2), "%"))

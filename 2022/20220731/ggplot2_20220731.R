library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
# https://www.data.go.kr/data/15070293/fileData.do
tb_car_accident = read_excel("~/github/ggplot2/2022/20220731/car_accident_2021.xlsx")


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
  
tb_car_accident2

ggplot(tb_car_accident2, aes(x = 1, y = death, fill = gender)) +
  geom_col()
ggplot(tb_car_accident2, aes(x = 1, y = death, fill = gender)) +
  geom_col() + 
  geom_label(aes(x = 4, label = paste0(gender," ", death, "명")), 
             family = "AppleSDGothicNeo-Bold",
             position = position_stack(vjust = 0.5)) +
  scale_x_continuous(limits = c(-5,4)) +
  scale_fill_brewer() +
  coord_polar(theta = "y") +
  theme_void(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none")

tb_car_accident2 %>% 
  mutate(rate = death / sum(death) * 100)

tb_car_accident2 %>% filter(gender == "여") %>% pull(rate) %>% round(2)
ggplot(tb_car_accident2, aes(x = 1, y = death, fill = gender)) +
  geom_col(position = "fill")
ggplot(tb_car_accident2, aes(x = 1, y = death, fill = gender)) +
  geom_col() + 
  # geom_label(aes(x = 4, label = paste0(gender," ", death, "명")), 
  #            family = "AppleSDGothicNeo-Bold",
  #            position = position_stack(vjust = 0.5)) +
  annotate("label", x = -5, y = 0, 
           label = paste0(tb_car_accident2 %>% filter(gender == "여") %>% pull(rate) %>% round(2), "%"),
           label.size = 0, size = 10,
           family = "AppleSDGothicNeo-Bold") +
  scale_x_continuous(limits = c(-5,4)) +
  scale_fill_brewer() +
  coord_polar(theta = "y") +
  theme_void(base_family = "AppleSDGothicNeo-Bold") +
  theme(legend.position = "none")


##########################################################################

tb_car_accident3 = tb_car_accident1 %>% 
  filter(death > 0) %>% 
  group_by(age_type) %>% 
  mutate(rate = round(death / sum(death) * 100, 2)) %>% 
  ungroup() %>% 
  mutate(label = paste0(round(rate, 2), "%"))
  # mutate(label = ifelse(gender == "여", paste0(rate, "%"), NA))
tb_car_accident3
ggplot(tb_car_accident3, aes(x = 1, y = death, fill = gender)) +
  geom_col(position = "fill") +
  facet_wrap(~ age_type)
tb_car_accident3


v_background_color = "gray100"  #F1F0EA  #363847


ggplot(tb_car_accident3, aes(x = 1, y = death, fill = gender)) +
  geom_col(position = "fill") +
  geom_label(aes(x = -5, y = 0, label = label),
             label.size = 0, size = 5, fill = v_background_color,
             family = "AppleSDGothicNeo-Bold") +
  facet_wrap(~ age_type, ncol = 4) +
  scale_x_continuous(limits = c(-5,1.5)) +
  scale_fill_brewer(palette = "Purples") +
  coord_polar(theta = "y") +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        strip.text = element_text(margin = margin(0.2,0,0.05,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220731/save_ggplot_4x3_01.png", 
       width = 8, height = 6, dpi = 320, units = "in")



################################################################################
tb_car_accident4
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
  
tb_car_accident5

ggplot(tb_car_accident5, aes(x = 1, y = death, fill = gender)) +
  geom_col(position = "fill") +
  geom_label(aes(x = -5, y = 0, label = label),
             label.size = 0, size = 5, fill = v_background_color,
             family = "AppleSDGothicNeo-Bold") +
  facet_wrap(~ age_type2, ncol = 4) +
  scale_x_continuous(limits = c(-5,1.5)) +
  scale_fill_brewer(palette = "Purples") +
  coord_polar(theta = "y") +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        strip.text = element_text(margin = margin(0.2,0,0.05,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220731/save_ggplot_4x3_02.png", 
       width = 8, height = 6, dpi = 320, units = "in")


####################################3





ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Purples") +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        # axis.text.x = element_text(),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220731/save_ggplot_4x3_03.png", 
       width = 8, height = 6, dpi = 320, units = "in")


#######################################################

tb_car_accident3
ggplot(tb_car_accident3, aes(x = age_type, y = death, group = gender, fill = gender)) +
  geom_area() +
  scale_fill_brewer(palette = "Purples") +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        # axis.text.x = element_text(),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20220731/save_ggplot_4x3_04.png", 
       width = 8, height = 6, dpi = 320, units = "in")

library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)
library(ggbump)
library(janitor)

v_dark_bgcolor = "#334960" 
v_light_bgcolor = "#F4F6FB"
v_font_bm = "BMJUAOTF"
v_font_base = "AppleSDGothicNeo-Bold"

# 예시
# tb4 = add_line(in_start = c(1,4), in_end = c(-3, 7), 1)
# tb5 = add_line(in_start = c(3,7.7), in_end = c(7, 10), -1)
add_line = function(in_start, in_end, in_hjust = 0){
  tibble(pos = c("x", "y"), start = in_start, end = in_end) %>% 
    mutate(middle = ifelse(pos == "x", (start + end)/2 + in_hjust, end)) %>% 
    pivot_longer(cols = 2:4) %>% 
    pivot_wider(names_from = pos, values_from = value)
}
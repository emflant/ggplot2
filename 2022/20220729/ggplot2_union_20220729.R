library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)


tb_baserate = read_excel("~/github/ggplot2/2022/20220729/The Bank of Korea Base Rate.xlsx")
x_breaks = seq.Date(from = ymd("2011-01-01"), to = ymd("2023-01-01"), by = "6 months")
x_breaks
x_label
seq(1, 10, 2)


x_label = as.character(year(x_breaks))
x_label[seq(1, length(x_breaks), 2)] = ""
x_label




tb_baserate1 = tb_baserate %>% 
  unite("년도", "변경일자", col = "date", sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  rename(base_rate = 기준금리) %>% 
  arrange(date) %>% 
  # filter(date >= ymd("2018-01-01")) %>% 
  print()

geom_text

tb_baserate1 %>% filter(date >= "2011-01-01") %>% 
  summarise(max_base_rate = max(base_rate)) %>% 
  mutate(max_base_rate = ceiling(max_base_rate)) %>% 
  pull(max_base_rate)

g_baserate9 = function(in_colour, in_start_date, in_add_y_pos = 0.3){
  v_background_color = "#363847"
  v_font_color = "gray80"
  v_x = tb_baserate1 %>% tail(1) %>% pull(date)
  v_y = tb_baserate1 %>% tail(1) %>% pull(base_rate) 
  
  v_max_y = tb_baserate1 %>% filter(date >= in_start_date) %>% 
    summarise(max_base_rate = max(base_rate)) %>% 
    mutate(max_base_rate = ceiling(max_base_rate)) %>% 
    pull(max_base_rate)
  
  ggplot(tb_baserate1, aes(date, base_rate)) +
    geom_step(size = 1, colour = in_colour) +
    scale_x_date(limits = c(in_start_date, ymd("2022-12-31")),
                 breaks = x_breaks, labels = x_label) +
    scale_y_continuous(limits = c(0, v_max_y),
                       breaks = seq(0,5,1),
                       expand = expansion(c(0,0.05))) +
    # annotate("rect", xmin = ymd("2022-03-13"), xmax = ymd("2022-11-13"),
    #          ymin = 2.4, ymax = 2.8, fill = v_background_color) +
    annotate("label", x = v_x, y = v_y + in_add_y_pos, 
             label.size = 1, label.padding = unit(2, "mm"),
             fill = v_background_color,
             label = paste0(v_y, "%"),
             family = "AppleSDGothicNeo-Bold", size = 7, colour = v_font_color) +
    annotate("point", size = 3.5, colour = in_colour,
             x = v_x, y = v_y) +
    annotate("point", size = 1.5, colour = v_background_color,
             x = v_x, y = v_y) +
    theme_minimal(base_family = "AppleSDGothicNeo-Bold") +
    theme(legend.position = "none",
          axis.text = element_text(colour = v_font_color, size = 14),
          axis.line.x = element_line(colour = v_font_color, size = 1),
          axis.text.x = element_text(margin = margin(0.1,0,0,0,"cm")),
          axis.ticks.x = element_line(color = rep(c(v_font_color, NA), length.out = length(x_breaks)),
                                      size = 1),
          axis.ticks.length.x = unit(0.2,"cm"),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = .3, colour = v_font_color, 
                                            linetype = "dotted"),
          plot.margin = margin(1,1,1,1, "cm"),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
}
#363847  background
#DBD765  yellow
#CD7378  red
#7AADC8  blue

layout <- "
AAAABB
AAAABB
CCCCCC
"

g_baserate9("#DBD765", ymd("2018-11-30")) +
  g_baserate9("#CD7378", ymd("2022-01-01")) +
  g_baserate9("#7AADC8", ymd("2011-01-01"), 0.8) +
  plot_layout(design = layout) +
  plot_annotation(caption = "twitter @sourcebox7",
                  theme = theme(plot.caption = element_text(color = "gray80", 
                                                            family = "Menlo", 
                                                            hjust = 1, 
                                                            size = 12,
                                                            margin = margin(0,0,0,0,"cm")),
                                plot.margin = margin(1,1,1,1,"cm"),
                                plot.background = element_rect(fill = "#363847", color = NA)))



ggsave("~/github/ggplot2/2022/20220729/save_ggplot_91.png", 
       width = 12, height = 8, dpi = 320, units = "in")


library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)


tb_baserate = read_excel("~/github/ggplot2/2022/20220729/The Bank of Korea Base Rate.xlsx")



tb_baserate1 = tb_baserate %>% 
  unite("년도", "변경일자", col = "date", sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  rename(base_rate = 기준금리) %>% 
  arrange(date) %>% 
  filter(date >= ymd("2018-01-01")) %>% 
  print()

tb_baserate1

ggplot(tb_baserate1, aes(date, base_rate)) +
  geom_line(size = 1) 

ggsave("~/github/ggplot2/2022/20220729/save_ggplot_test_0.png", 
       width = 6, height = 4, dpi = 320, units = "in")

ggplot(tb_baserate1, aes(date, base_rate)) +
  geom_step(size = 1) 

ggsave("~/github/ggplot2/2022/20220729/save_ggplot_test_1.png", 
       width = 6, height = 4, dpi = 320, units = "in")


ggplot(tb_baserate1, aes(date, base_rate)) +
  geom_step(size = 1) +
  geom_point(size = 4) +
  geom_point(size = 2, colour = "gray100")

ggsave("~/github/ggplot2/2022/20220729/save_ggplot_test_2.png", 
       width = 6, height = 4, dpi = 320, units = "in")

#######################################3

g_baserate = function(in_colour){
  ggplot(tb_baserate1, aes(date, base_rate)) +
    geom_step(size = 2, colour = in_colour) +
    # geom_point(size = 3.5, colour = "#F1F0EA", shape = 15) +
    # geom_point(size = 2, colour = "#245875") +
    scale_x_date(limits = c(ymd("2018-11-30"), ymd("2022-12-31")),
                 breaks = x_breaks, labels = x_label) +
    scale_y_continuous(limits = c(0.25,2.5),
                       breaks = seq(0,5,0.5)) +
    annotate("text", x = tb_baserate1 %>% tail(1) %>% pull(date),
             y = tb_baserate1 %>% tail(1) %>% pull(base_rate) + 0.15,
             label = paste0(tb_baserate1 %>% tail(1) %>% pull(base_rate), "%"),
             family = "AppleSDGothicNeo-Bold", size = 5, colour = "gray30") +
    theme_minimal(base_family = "AppleSDGothicNeo-Bold") +
    labs(y = "기준금리(%)", caption = "twitter @sourcebox7") +
    theme(legend.position = "none",
          axis.text.x = element_text(margin = margin(0.1,0,0,0,"cm")),
          axis.ticks.x = element_line(color = rep(c("black", NA), length.out = 9)),
          axis.ticks.length.x = unit(0.2,"cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0, vjust = 1.07, size = 10,
                                      margin = margin(0,-1.2,0,0,"cm")),
          axis.line.x = element_line(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = .3, colour = "gray30", linetype = "dotted"),
          plot.margin = margin(1.3,1,1,.8, "cm"),
          plot.caption = element_text(color = "gray30", 
                                      family = "Menlo", 
                                      hjust = 1, 
                                      size = 8,
                                      margin = margin(0.5,0,0,0,"cm")),
          plot.background = element_rect(fill = "#F1F0EA", color = "#F1F0EA"))
}

g_baserate("#398AB9")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_1.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_baserate("#D1512D")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_2.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_baserate("#74959A")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_3.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_baserate("#6EBEE6")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_4.png", 
       width = 6, height = 4, dpi = 320, units = "in")



###########################################################

tb_baserate1


g_baserate1 = function(in_colour){
  
  v_background_color = "#F1F0EA"
  v_font_color = "gray100"
  v_x = tb_baserate1 %>% tail(1) %>% pull(date)
  v_y = tb_baserate1 %>% tail(1) %>% pull(base_rate) 
  
  ggplot(tb_baserate1, aes(date, base_rate)) +
    geom_step(size = 1, colour = in_colour) +
    scale_x_date(limits = c(ymd("2018-11-30"), ymd("2022-12-31")),
                 breaks = x_breaks, labels = x_label) +
    scale_y_continuous(limits = c(0.25,3),
                       breaks = seq(0,5,0.5)) +
    annotate("rect", xmin = ymd("2022-03-13"), xmax = ymd("2022-11-13"), 
              ymin = 2.4, ymax = 2.8, fill = in_colour) +
    annotate("text", x = tb_baserate1 %>% tail(1) %>% pull(date),
             y = tb_baserate1 %>% tail(1) %>% pull(base_rate) + 0.35,
             label = paste0(tb_baserate1 %>% tail(1) %>% pull(base_rate), "%"),
             family = "AppleSDGothicNeo-Bold", size = 5, colour = "gray100") +
    annotate("point", size = 3.5, colour = in_colour,
             x = v_x, y = v_y) +
    annotate("point", size = 1.5, colour = v_background_color,
             x = v_x, y = v_y) +
    theme_minimal(base_family = "AppleSDGothicNeo-Bold") +
    labs(y = "기준금리(%)", caption = "twitter @sourcebox7") +
    theme(legend.position = "none",
          axis.text.x = element_text(margin = margin(0.1,0,0,0,"cm")),
          axis.ticks.x = element_line(color = rep(c("black", NA), length.out = 9)),
          axis.ticks.length.x = unit(0.2,"cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0, vjust = 1.07, size = 10,
                                      margin = margin(0,-1.2,0,0,"cm")),
          axis.line.x = element_line(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = .3, colour = "gray30", linetype = "dotted"),
          plot.margin = margin(1.3,1,1,.8, "cm"),
          plot.caption = element_text(color = "gray30", 
                                      family = "Menlo", 
                                      hjust = 1, 
                                      size = 8,
                                      margin = margin(0.5,0,0,0,"cm")),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
}

g_baserate1("#398AB9")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_11.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_baserate1("#D1512D")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_12.png", 
       width = 6, height = 4, dpi = 320, units = "in")

###########################################################
 
tb_baserate1


g_baserate2 = function(in_colour){
  v_background_color = "#363847"
  v_font_color = "gray80"
  v_x = tb_baserate1 %>% tail(1) %>% pull(date)
  v_y = tb_baserate1 %>% tail(1) %>% pull(base_rate) 
  
  ggplot(tb_baserate1, aes(date, base_rate)) +
    geom_step(size = 1, colour = in_colour) +
    scale_x_date(limits = c(ymd("2018-11-30"), ymd("2022-12-31")),
                 breaks = x_breaks, labels = x_label) +
    scale_y_continuous(limits = c(0.25,3),
                       breaks = seq(0,5,0.5)) +
    annotate("rect", xmin = ymd("2022-03-13"), xmax = ymd("2022-11-13"),
             ymin = 2.4, ymax = 2.8, fill = v_background_color) +
    annotate("text", x = v_x, y = v_y + 0.25,
             label = paste0(v_y, "%"),
             family = "AppleSDGothicNeo-Bold", size = 5, colour = v_font_color) +
    annotate("point", size = 3.5, colour = in_colour,
             x = v_x, y = v_y) +
    annotate("point", size = 1.5, colour = v_background_color,
             x = v_x, y = v_y) +
    theme_minimal(base_family = "AppleSDGothicNeo-Bold") +
    theme(legend.position = "none",
          axis.text = element_text(colour = v_font_color),
          axis.line.x = element_line(colour = v_font_color, size = 1),
          axis.text.x = element_text(margin = margin(0.1,0,0,0,"cm")),
          axis.ticks.x = element_line(color = rep(c(v_font_color, NA), length.out = 9),
                                      size = 1),
          axis.ticks.length.x = unit(0.2,"cm"),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = .3, colour = v_font_color, 
                                            linetype = "dotted"),
          plot.margin = margin(1,1,1,.8, "cm"),
          plot.caption = element_text(color = v_font_color, family = "Menlo", 
                                      hjust = 1, size = 8,
                                      margin = margin(0.5,0,0,0,"cm")),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
}
#363847  background
#DBD765  yellow
#CD7378  red
#7AADC8  blue

g_baserate2("#DBD765")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_21.png", 
       width = 6, height = 4, dpi = 320, units = "in")


g_baserate2("#CD7378")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_22.png", 
       width = 6, height = 4, dpi = 320, units = "in")


g_baserate2("#7AADC8")
ggsave("~/github/ggplot2/2022/20220729/save_ggplot_23.png", 
       width = 6, height = 4, dpi = 320, units = "in")
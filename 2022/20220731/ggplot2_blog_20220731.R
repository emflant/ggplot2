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
  


##########################################################################

tb_car_accident3 = tb_car_accident1 %>% 
  filter(death > 0) %>% 
  group_by(age_type) %>% 
  mutate(rate = round(death / sum(death) * 100, 2)) %>% 
  ungroup() %>% 
  mutate(label = paste0(round(rate, 2), "%"))
  # mutate(label = ifelse(gender == "여", paste0(rate, "%"), NA))
tb_car_accident3

v_background_color = "gray100"  #F1F0EA  #363847


################################################################################
# tb_car_accident4
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


# v_palette = "RdPu"
v_palette = "OrRd"

tb_car_accident6 = tb_car_accident5 %>% filter(age_type2 != "41-60세")

####################################
in_palette = "RdPu"
v_background_color = "gray100"
ggplot(tb_car_accident6, aes(x = 1, y = death, fill = gender)) +
  geom_col(position = "fill") +
  facet_wrap(~ age_type2, ncol = 3) +
  scale_x_continuous(limits = c(-4,1.5)) +
  scale_fill_brewer(palette = in_palette) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

flex_dashboard()
ggsave("~/github/ggplot2/2022/20220731/save_ggplot_16x9_blog_01.png", 
       width = 8, height = 3, dpi = 320, units = "in")

ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = in_palette) +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220731/save_ggplot_16x9_blog_02.png", 
       width = 8, height = 4, dpi = 320, units = "in")


ggplot(tb_car_accident3, aes(x = age_type, y = death, group = gender, fill = gender)) +
  geom_area() +
  scale_fill_brewer(palette = in_palette) +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220731/save_ggplot_16x9_blog_03.png", 
       width = 8, height = 4, dpi = 320, units = "in")

plot_union = function(in_palette){
  
  g1 = ggplot(tb_car_accident5, aes(x = 1, y = death, fill = gender)) +
    geom_col(position = "fill") +
    facet_wrap(~ age_type2, ncol = 4) +
    scale_x_continuous(limits = c(-4,1.5)) +
    scale_fill_brewer(palette = in_palette) +
    coord_polar(theta = "y") +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # strip.text = element_text(margin = margin(0.2,0,0.05,0,"in")),
          strip.text = element_blank(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  
  ####################################3
  
  g2 = ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
    geom_col(position = "fill") +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  #######################################################
  
  
  g3 = ggplot(tb_car_accident3, aes(x = age_type, y = death, group = gender, fill = gender)) +
    geom_area() +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  layout <- "
AAAAAA
AAAAAA
AAAAAA
BBBCCC
BBBCCC
"
  
  
  # v_palette = "RdPu"
  g1 + g2 + g3 +
    plot_layout(design = layout) +
    plot_annotation(caption = "twitter @sourcebox7",
                    theme = theme(plot.caption = element_text(color = "gray30", 
                                                              family = "Menlo", 
                                                              hjust = .95, 
                                                              size = 8,
                                                              margin = margin(0.1,0,0,0,"in")),
                                  plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
                                  plot.background = element_rect(fill = v_background_color, color = NA)))
  
}




plot_union("Blues")
ggsave("~/github/ggplot2/2022/20220731/save_ggplot_2x1_Blues.png", 
       width = 8, height = 4, dpi = 320, units = "in")

plot_union("RdPu")
ggsave("~/github/ggplot2/2022/20220731/save_ggplot_2x1_RdPu.png", 
       width = 8, height = 4, dpi = 320, units = "in")



tb_car_accident5


plot_union2 = function(in_palette){
  
  tb_car_accident6 = tb_car_accident5 %>% head(4)
  tb_car_accident6
  g1 = ggplot(tb_car_accident6, aes(x = 1, y = death, fill = gender)) +
    geom_col(position = "fill") +
    facet_wrap(~ age_type2, ncol = 4) +
    scale_x_continuous(limits = c(-4,1.5)) +
    scale_fill_brewer(palette = in_palette) +
    coord_polar(theta = "y") +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # strip.text = element_text(margin = margin(0.2,0,0.05,0,"in")),
          strip.text = element_blank(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  
  ####################################3
  
  g2 = ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
    geom_col(position = "fill") +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0.3,0,"in"),
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  #######################################################
  
  
  g3 = ggplot(tb_car_accident3, aes(x = age_type, y = death, group = gender, fill = gender)) +
    geom_area() +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  layout <- "
BBBBBB
BBBBBB
AAACCC
AAACCC
"
  
  
  
  # v_palette = "RdPu"
  g1 + g2 + g3 +
    plot_layout(design = layout) +
    plot_annotation(caption = "twitter @sourcebox7",
                    theme = theme(plot.caption = element_text(color = "gray30", 
                                                              family = "Menlo", 
                                                              hjust = .95, 
                                                              size = 8,
                                                              margin = margin(0.1,0,0,0,"in")),
                                  plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
                                  plot.background = element_rect(fill = v_background_color, color = NA)))
  
}


plot_union2("Blues")
ggsave("~/github/ggplot2/2022/20220731/save_ggplot_16x9_Blues_02.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")





plot_union3 = function(in_palette){
  
  tb_car_accident6 = tb_car_accident5 %>% head(4)
  tb_car_accident6
  g1 = ggplot(tb_car_accident6, aes(x = 1, y = death, fill = gender)) +
    geom_col(position = "fill") +
    facet_wrap(~ age_type2, ncol = 4) +
    scale_x_continuous(limits = c(-4,1.5)) +
    scale_fill_brewer(palette = in_palette) +
    coord_polar(theta = "y") +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          # strip.text = element_text(margin = margin(0.2,0,0.05,0,"in")),
          strip.text = element_blank(),
          panel.spacing.x = unit(0.1, "in"),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  
  ####################################3
  
  g2 = ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
    geom_col(position = "fill") +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0,"in"),
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  #######################################################
  
  
  g3 = ggplot(tb_car_accident3, aes(x = age_type, y = death, group = gender, fill = gender)) +
    geom_area() +
    scale_x_discrete(expand = expansion(c(0.01,0.01))) +
    scale_y_continuous(expand = expansion(c(0.02,0.05))) +
    scale_fill_brewer(palette = in_palette) +
    theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0.1,"in"),
          # axis.text.x = element_text(),
          plot.background = element_rect(fill = v_background_color, color = v_background_color))
  
  
  layout <- "
AAACCC
AAACCC
AAACCC
BBBCCC
BBBCCC
"
  
  
  
  # v_palette = "RdPu"
  g1 + g2 + g3 +
    plot_layout(design = layout) +
    plot_annotation(caption = "twitter @sourcebox7",
                    theme = theme(plot.caption = element_text(color = "gray30", 
                                                              family = "Menlo", 
                                                              hjust = .99, 
                                                              size = 8,
                                                              margin = margin(0.1,0,0,0,"in")),
                                  plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
                                  plot.background = element_rect(fill = v_background_color, color = NA)))
  
}



plot_union3("Purples")
ggsave("~/github/ggplot2/2022/20220731/save_ggplot_16x9_Purples_03.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")
# install.packages("plotly")
library(plotly)
g1 = ggplot(tb_car_accident3, aes(x = age_type, y = death, fill = gender)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Purples", direction = -1) +
  theme_void(base_family = "AppleSDGothicNeo-Bold", base_size = 15) +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0,"in"),
        # axis.text.x = element_text(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggplotly(g1)

library(tidyverse)
library(patchwork)
# rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
rent %>% distinct(city)

rent_marin = rent %>% 
  filter(city == 'marin') %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  arrange(year)

rent_marin %>% head()

################################################################################


g_col_1 = function(in_low, in_high){
  ggplot(rent_marin, aes(year, count, fill = count)) +
    geom_col(width = .5) +
    scale_fill_gradient(low = in_low, high = in_high) +
    scale_x_continuous(breaks = c(2000, 2012, 2018)) +
    scale_y_continuous(expand = expansion(c(0, 0.15))) +
    theme_void(base_family = "AppleSDGothicNeo-ExtraBold", base_size = 13) +
    labs(caption = "twitter @sourcebox7") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(margin = margin(0.2,0,0,0,"cm"), size = 10),
      plot.caption = element_text(color = "gray30", 
                                  family = "Menlo", 
                                  hjust = .95, 
                                  size = 8,
                                  margin = margin(0.5,0,0,0,"cm")),
      plot.margin = margin(0.5,1,1,1,"cm"),
      plot.background = element_rect(fill = "#F1F0EA", color = "#F1F0EA")
    )
}

g_col_1(in_low = "#C8C6C6", in_high = "#4B6587")
ggsave("~/github/ggplot2/2022/20220719/save_ggplot_col_1.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_col_1(in_low = "#B6D2D9", in_high = "#0AA1DD")
ggsave("~/github/ggplot2/2022/20220719/save_ggplot_col_2.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_col_1(in_low = "#BBE1F2", in_high = "#42C2FF")
ggsave("~/github/ggplot2/2022/20220719/save_ggplot_col_3.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_col_1(in_low = "#FFC4C4", in_high = "#A10035")
ggsave("~/github/ggplot2/2022/20220719/save_ggplot_col_4.png", 
       width = 6, height = 4, dpi = 320, units = "in")

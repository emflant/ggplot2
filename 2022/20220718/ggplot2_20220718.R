library(tidyverse)
library(patchwork)
# rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

rent_sanfrancisco1 = rent %>% filter(city == 'san francisco') %>% select(year) 
rent_sanfrancisco1 %>% head()

################################################################################


g_histogram_1 = function(in_low, in_high){
  ggplot(rent_sanfrancisco1) +
    geom_histogram(aes(year, fill = ..count..), 
                   binwidth = 1, show.legend = F) +
    scale_fill_gradient(low = in_low, high = in_high) +
    scale_x_continuous(breaks = c(2000, 2004, 2018)) +
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

g_histogram_1(in_low = "#9FE8FA", in_high = "#26BAEE")
ggsave("~/github/ggplot2/2022/20220718/save_ggplot_histogram_1.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_histogram_1(in_low = "#C4DFAA", in_high = "#73A9AD")
ggsave("~/github/ggplot2/2022/20220718/save_ggplot_histogram_2.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_histogram_1(in_low = "#CEEDF6", in_high = "#4592A3")
ggsave("~/github/ggplot2/2022/20220718/save_ggplot_histogram_3.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_histogram_1(in_low = "#FAD9A1", in_high = "#F37878")
ggsave("~/github/ggplot2/2022/20220718/save_ggplot_histogram_4.png", 
       width = 6, height = 4, dpi = 320, units = "in")



################################################################################

g_histogram = function(in_binwidth) {
  ggplot(rent_sanfrancisco1) +
    geom_histogram(aes(year, fill = ..count..), 
                   binwidth = in_binwidth,
                   show.legend = F) +
    stat_bin(geom = "text", aes(year, label = ifelse(..count.. == max(..count..), 
                                                     formatC(..count.., format="f", big.mark=",", digits = 0), "")),
             binwidth = in_binwidth, colour = "gray30", vjust = -0.5,
             family = "AppleSDGothicNeo-ExtraBold", size = 6) +
    scale_fill_gradient(low = "#C4DFAA", high = "#73A9AD" ) +
    scale_y_continuous(expand = expansion(c(0, 0.25))) +
    labs(title = paste0("* binwidth = ",in_binwidth)) +
    theme_void(base_family = "AppleSDGothicNeo-ExtraBold", base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(margin = margin(0.2,0,0,0,"cm"), size = 12, colour = "gray30"),
      plot.margin = margin(1,1,1,1,"cm"),
      plot.background = element_rect(fill = "#F1F0EA", color = "#F1F0EA")
    )
}

g_histogram(.5) + 
  g_histogram(1) + 
  g_histogram(2) +
  g_histogram(5) +
  g_histogram(10) +
  g_histogram(20) +
  plot_layout(ncol = 3) +
  plot_annotation(caption = "twitter @sourcebox7",
                  theme = theme(plot.caption = element_text(color = "gray30", 
                                                            family = "Menlo", 
                                                            hjust = 1, 
                                                            size = 12,
                                                            margin = margin(0,0,0.5,0,"cm")),
                                plot.background = element_rect(fill = "#F1F0EA", color = NA)))


ggsave("~/github/ggplot2/2022/20220718/save_ggplot_histogram_union.png", 
       width = 12, height = 8, dpi = 320, units = "in")

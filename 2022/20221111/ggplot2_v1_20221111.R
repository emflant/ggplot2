source("./core.R")

tb1 = tibble(x = c(1, 1, 2),
             y = c(2, 5.7, 2),
             l = c(NA, "10월 물가상승률", "목표치"),
             y2 = c(NA, "7.7%", "2%"))


reorder(tb1$y, -tb1$y)

tibble(x = c(1, 1, 2),
       y = c(2, 5.7, 2),
       # l = c(NA, "10월 물가상승률", "목표치"),
       l = c(NA, "A", "B"),
       y2 = c(NA, "7.7%", "2%")) %>% 
  ggplot(aes(x,y, fill = reorder(y, -y))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = y2), size = 8,
            position = position_stack(),
            vjust = -1, colour = "gray100",
            na.rm = T,
            family = "BMJUAOTF") +
  scale_y_continuous(expand = expansion(c(0.05,0.5))) +
  scale_x_continuous(expand = expansion(c(0.15,0.15))) +
  annotate("segment", x = 0, xend = 3, y = 2, yend = 2,
           linetype = "dotted", colour = "gray100", size = 0.7) +
  annotate("rect", xmin = 0, xmax = 3, ymin = -2, ymax = 0,
           fill = v_dark_bgcolor, colour = "gray100", size = 0.3) +
  geom_text(aes(y = -1, label = l),
            na.rm = T, size = 7, colour = "gray100",
            family = "BMJUAOTF") + 
  scale_fill_brewer(palette = "RdGy" ) + #"RdGy"
  labs(title = "(ggplot2) geom_col + geom_segment",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(plot.margin = margin(0.3,0.3,1,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.3, 0, -0.3, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.93, 
                                    size = 12,
                                    margin = margin(0.3,0,-0.5,0,"in")),
        legend.position = "none")

ggsave("./2022/20221111/save_01.png", 
       width = 8, height = 6, dpi = 120, units = "in")


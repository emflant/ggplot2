source("./core.R")

tb1 = tibble(x = 1:6,
       y_text = c("635.8878", "648.7020", "660.5558",
             "673.7552", "687.4270", "703.7512"),
       x_label = c("'21년\n 12월", "'22년\n 2월", "4월", "6월", "8월", "10월")) %>% 
  mutate(y = as.numeric(y_text)) %>% 
  mutate(y1 = str_replace(y_text, "\\.", "조\n")) %>% 
  mutate(y2 = str_c(y1, "억")) %>% 
  mutate(p = ifelse(x == n(), NA, y)) %>% 
  select(x, y, y_label = y2, x_label, p)


tb1 
# tibble(x = 1:6,
#        y = c(635.8878, 648.7020, 660.5558,
#              673.7552, 687.4270, 703.7512),
#        l = c("635조\n8878억","648조\n8878억","635조\n8878억",
#              "635조\n8878억","635조\n8878억","635조\n8878억"),
#        x_label = c("21년\n12월", "22년\n2월", "4월", "6월", "8월", "10월")) %>% 
ggplot(tb1, aes(x,y)) +
  geom_line(size = 2, colour = "gray100",
            arrow = arrow(angle = 15, ends = "last", type = "closed",
                          length = unit(0.3, "in"))) +
  geom_segment(aes(x = x, y = y, xend = x, yend = 570),
               linetype = "dotted", size = 0.8) +
  geom_point(aes(x,p), size = 4, na.rm = T) +
  geom_point(aes(x,p), size = 2, colour = "gray100", na.rm = T) +
  geom_text(aes(label = y_label), vjust = -0.6,
            family = "BMJUAOTF", size = 5, colour = "gray100") +
  scale_x_continuous(breaks = tb1$x, labels = tb1$x_label,
                     limits = c(0.5, 6.5)) +
  # xlim(0.5, 6.5) +
  ylim(570, 800) +
  labs(title = "ggplot2 - geom_line",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.3,1,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.15, 
                                  size = 20,
                                  margin = margin(0.5, 0, -0.3, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.88, 
                                    size = 12,
                                    margin = margin(0.3,0,-0.5,0,"in")),
        axis.text.x = element_text(vjust = 1, size = 15))
  

ggsave("./2022/20221109/save_01.png", 
       width = 8, height = 6, dpi = 180, units = "in")

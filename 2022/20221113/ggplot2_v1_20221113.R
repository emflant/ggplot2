source('./core.R')


tb1 = tibble(x = c(1,3), y = c(4, 7.7))

tb_line1 = add_line(in_start = c(1,4.1), in_end = c(-2, 7), 1)
tb_line2 = add_line(in_start = c(3,7.8), in_end = c(6, 10), -1)

ggplot(tb1, aes(x,y)) +
  geom_col() +
  xlim(-5, 9) +
  ylim(0, 13) +
  geom_line(aes(x,y), data = tb_line1) +
  geom_line(aes(x,y), data = tb_line2) +
  annotate("text", x = -2.7, y = 7, label = 4) +
  annotate("text", x = 7, y = 10, label = 7.7) +
  # theme_void() +
  theme(axis.title = element_blank())
  



ggplot(tb1, aes(x,y)) +
  geom_col(fill = "#F8E09E") +
  scale_x_continuous(breaks = c(1,3), 
                     labels = c("A", "B"),
                     limits = c(-5,9)) +
  ylim(0, 13) +
  geom_line(aes(x,y), data = tb_line1, colour = "gray80") +
  geom_line(aes(x,y), data = tb_line2, colour = "gray80") +
  annotate("text", x = -2.7, y = 7, label = 4, size = 7,
           family = "BMJUAOTF", colour = "gray100") +
  annotate("text", x = 7, y = 10, label = 7.7, size = 7,
           family = "BMJUAOTF", colour = "gray100") +
  labs(title = "(ggplot2) geom_col + geom_line",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.3,0.3,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.3, 0, 0, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.93, 
                                    size = 12,
                                    margin = margin(0.3,0,0,0,"in")),
        axis.title = element_blank(),
        axis.text.x = element_text(colour = "gray100", size = 17))


ggsave("./2022/20221113/save_01.png", 
       width = 8, height = 6, dpi = 180, units = "in")

source("core.R")


tibble(x = 1:3, y = c(1,2,3)) %>% 
  ggplot(aes(x,y)) +
  geom_col(fill = "gray100") +
  scale_x_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  scale_y_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  coord_fixed() +
  labs(title = "Hugo 파비콘(favicon) 설정하기") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.2,0.3,0.5,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  hjust = 0.5, 
                                  size = 25,
                                  margin = margin(0.5, 0, -0.2, 0,"in")),
        axis.line.x = element_line(size = 1, colour = "gray100"))


ggsave("./2022/20221116/save_01.png", 
       width = 8, height = 6, dpi = 120, units = "in")




tibble(x = 1:3, y = c(1,2,3)) %>% 
  ggplot(aes(x,y)) +
  geom_col(fill = "gray100") +
  scale_x_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  scale_y_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.2,0.3,0.3,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.line.x = element_line(size = 1, colour = "gray100"))


ggsave("./2022/20221116/favicon.png", 
       width = 192, height = 192, dpi = 120, units = "px")




ggplot2::ggsave()

tibble(x = 1:3, y = c(1,2,3)) %>% 
  ggplot(aes(x,y)) +
  geom_col(fill = "gray100") +
  scale_x_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  scale_y_continuous(expand = expansion(c(0,0)),
                     limits = c(0,4)) +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.2,0.3,0.4,0.3,"in"), 
        plot.background = element_rect(fill = "#404040", color = "#404040"),
        axis.line.x = element_line(size = 1, colour = "gray100"))


ggsave("./2022/20221116/logo.png", 
       width = 192, height = 192, dpi = 120, units = "px")




5.416 - 3.808
5.175 - 4.081

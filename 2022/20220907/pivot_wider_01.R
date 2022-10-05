
tibble(month = c(1,1,2,4,4,4),
       day = c(1,1,3,3,4,4)) 

tibble(month = c(1,1,2,4,4,4),
       day = c(1,1,3,3,4,4)) %>% 
  group_by(month, day) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = month,
              values_from = count)


tibble(month = paste0(c(1,1,2,4,4,4), "월"),
       day = paste0(c(1,1,3,3,4,4), "일")) %>% 
  group_by(month, day) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = month,
              values_from = count)


tb1 = tibble(month = c(1,1,2,4,4,4),
       day = c(1,1,3,3,4,4)) %>% 
  group_by(month, day) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  mutate(row = 1:4)
tb1


ggplot(tb1, aes(month, day, fill = count)) +
  geom_tile(colour = "gray100", size = 1) + 
  geom_text(aes(label = count), colour = "gray100",
            family = "BMJUAOTF", size = 10) +
  coord_fixed() +
  theme_bw() 


v_background_color = "#3A536E" #334960
v_font_color = v_background_color
ggplot(tb1, aes(month, day, fill = factor(count))) +
  geom_tile(colour = v_background_color, size = 3) + 
  geom_text(aes(label = count), colour = v_font_color,
            family = "BMJUAOTF", size = 10) +
  scale_fill_brewer(palette = "RdBu") +
  labs(title = "ggplot2 - geom_tile",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = -0.5, 
                                  size = 20,
                                  margin = margin(0,0,0,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1.3, 
                                    size = 10,
                                    margin = margin(0,0,0,0,"in")),
        plot.margin = margin(0.5,0,0.5,0,"in"),
        aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = NA))
  

ggsave("~/github/ggplot2/2022/20220907/save_ggplot_01.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 


tb1
ggplot(tb1, aes(month, day)) +
  geom_hex() +
  coord_fixed()

v_background_color = "#3A536E" #334960
v_font_color = v_background_color
ggplot(tb1, aes(month, day, fill = factor(count))) +
  geom_tile(colour = v_background_color, size = 3) + 
  geom_text(aes(label = count), colour = v_font_color,
            family = "BMJUAOTF", size = 10) +
  scale_fill_brewer(palette = "RdBu") +
  labs(title = "ggplot2 - geom_tile",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = -0.5, 
                                  size = 20,
                                  margin = margin(0,0,0,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1.3, 
                                    size = 10,
                                    margin = margin(0,0,0,0,"in")),
        plot.margin = margin(0.5,0,0.5,0,"in"),
        # aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = NA))


ggsave("~/github/ggplot2/2022/20220907/save_ggplot_02.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 


d <- ggplot(diamonds, aes(carat, price))
diamonds
d + geom_hex()


tibble(x = c(1,2,3,4),
       y = c(3,3,3,3)) %>% 
  ggplot(aes(x,y)) +
  geom_hex()

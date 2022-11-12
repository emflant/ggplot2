source('./core.R')




v_width = 2
tb1 = expand.grid(x = 1:6 * v_width, 
                  y = 1:5) %>% as_tibble() %>% 
  arrange(x, y) %>% 
  mutate(l = c(2017:2021, seq(100, 140, 10), seq(200, 240, 10), seq(300, 340, 10), 
               seq(400, 440, 10), seq(500, 540, 10)))
tb2 = expand.grid(x = 1:6 * v_width, y = 0) %>% 
  as_tibble() %>% 
  mutate(l = c("년도", "서울", "대전", "대구", "광주", "부산"))
tb2



v_background_color = "#F8F6F2"
v_background_color = "#FDFCFB"
v_background_color = "gray20"
ggplot() +
  geom_tile(data = tb1, aes(x,y), 
            colour = "gray30", fill = "gray100") +
  geom_text(data = tb1, aes(x,y, label = l),
            family = "BMJUAOTF", colour = "gray0") +
  geom_tile(data = tb2, aes(x,y),
            colour = "gray100", fill = "#3182bd") +
  geom_text(data = tb2, aes(x,y, label = l),
            family = "BMJUAOTF", colour = "gray100") +
  # xlim(c(0,30)) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("./2022/20221103/save_01.png", 
       width = 8, height = 6, dpi = 120, units = "in")




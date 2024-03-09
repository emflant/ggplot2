source('./geo_core.R')

map_seoul = read_sf('./2024/20240306/sf/map_seongeogu_0_01x.shp')
map_seoul
map_songpa = map_seoul |> filter(str_detect(sig, "송파구"))
map_songpa

g2_bg = "gray97"
ggplot() +
  geom_sf(data = map_songpa, aes(fill = snggmyn)) +
  geom_text(data = get_center(map_songpa), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())
  
ggsave(filename = "./2024/20240306/v5-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

ggplot(map_songpa) +
  geom_sf(aes(fill = snggmyn)) +
  geom_text(data = get_center(map_songpa), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())

ggsave(filename = "./2024/20240306/v5-02.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

ggplot() +
  geom_sf(data = map_gangnam, aes(fill = snggmyn), colour = g2_bg, linewidth = 0.4) +
  geom_text(data = get_center(map_gangnam), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.15,
            family = v_font_heavy) +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
  labs(title = str_c("강남구", " 선거구")) +
  # facet_wrap(vars(sig)) +
  theme_void(base_family = v_font_gmarket_bold) +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = v_font_gmarket_medium),
        legend.position = "bottom",
        plot.background = element_rect(fill = g2_bg, colour = g2_bg),
        plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))

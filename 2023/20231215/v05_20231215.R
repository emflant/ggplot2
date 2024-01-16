source('./geo_core.R')

# spData::world 를 이용한 한국 주변 지도 그리기
#ggplot #RStats #infographic #dataviz

spData::world
ggplot(spData::world) +
  geom_sf()

ggsave(filename = "./2023/20231215/v05_20231215-01.png", 
       device = grDevices::png,
       width = 8, height = 3.8, dpi = 120, units = "in")

spData::world |> head(3)

asia = spData::world |> 
  filter(continent == 'Asia')
asia
ggplot(asia) +
  geom_sf()

ggsave(filename = "./2023/20231215/v05_20231215-02.png", 
       device = grDevices::png,
       width = 8, height = 4.7, dpi = 120, units = "in")


#
# spData::world |> filter(str_detect(name_long, '.*sia.*'))

korea = spData::world |> 
  filter(iso_a2 %in% c('MN', 'CN', 'KP', 'KR', 'JP', 'RU'))

ggplot(spData::world) +
  geom_sf() +
  coord_sf(xlim = c(100, 150), # 경도
           ylim = c(25, 50)) + # 위도
  annotate("text", x = 125, y = 36, alpha = 0.3,
           label = v_water_mark, colour = "gray0",size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  theme(axis.title = element_blank())

ggsave(filename = "./2023/20231215/v05_20231215-03.png", 
       device = grDevices::png,
       width = 8, height = 4.8, dpi = 120, units = "in")


korea
ggplot(korea) +
  geom_sf(aes(fill = iso_a2)) +
  coord_sf(xlim = c(100, 150), # 경도
           ylim = c(25, 50)) + # 위도
  scale_fill_brewer(palette = "YlGnBu") +
  annotate("text", x = 125, y = 36, alpha = 0.3,
           label = v_water_mark, colour = "gray0",size = 7,
           family = "AppleSDGothicNeo-Heavy") +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        text = element_text(family = v_font_bm))

ggsave(filename = "./2023/20231215/v05_20231215-04.png", 
       device = grDevices::png,
       width = 8, height = 4.8, dpi = 120, units = "in")


ggsave(filename = "./2023/20231215/v05_20231215-05.png", 
       device = grDevices::png,
       width = 8, height = 4.8, dpi = 240, units = "in")



korea
ggplot(korea) +
  geom_sf(aes(fill = name_long)) +
  coord_sf(xlim = c(100, 150), # 경도
           ylim = c(25, 50)) + # 위도
  scale_fill_brewer(palette = "YlGnBu") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        text = element_text(family = v_font_bm))

ggsave(filename = "./2023/20231215/v05_20231215-06.png", 
       device = grDevices::png,
       width = 8, height = 4, dpi = 240, units = "in")

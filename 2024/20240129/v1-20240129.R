source('./geo_core.R')

dining_expenses = read_excel('./2024/20240129/data.xls') |> 
  select(지역, 삼겹살 = `삼겹살(환산후)`) |> 
  mutate(삼겹살 = as.numeric(삼겹살))

area = dining_expenses |> 
  select(지역)

area

sido3 = get_map_sido(0.1)

ggplot(sido3) +
  geom_sf()

sido2 = sido_with_dokdo()
sido = sido_with_dokdo()
sido
ggplot(sido) +
  geom_sf()

## 44.충청남도
sido44 = sido |> filter(CTPRVN_CD == '44')
sido44
## 36.세종특별자치시.
sido36 = sido |> filter(CTPRVN_CD == '36')
sido36
geom_sido44_2 = st_union(st_geometry(sido44), st_geometry(sido36))

geom_sido44_2
sido44_2 = st_set_geometry(sido44, geom_sido44_2)
sido44_2

sido2 = sido |> 
  filter(CTPRVN_CD != '36', CTPRVN_CD != '44') |> 
  union_all(sido44_2) |> 
  arrange(CTPRVN_CD) |> 
  mutate(yn = CTPRVN_CD == 44, .before = geometry)

sido2

sido1 = sido |> 
  mutate(yn = CTPRVN_CD %in% c('36', '44'), .before = geometry)


ggplot() +
  geom_sf(data = sido1, aes(fill = yn)) + 
  scale_fill_brewer(palette = "BrBG", direction = -1)

g1 = ggplot() +
  geom_sf(data = sido1, aes(fill = yn)) + 
  geom_text(data = get_center(sido3), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BrBG", direction = -1) +
  theme_bw() +
  theme(legend.position = "none", 
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))


g2 = ggplot() +
  geom_sf(data = sido2, aes(fill = yn)) + 
  geom_text(data = get_center(sido3), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BrBG", direction = -1) +
  theme_bw() +
  theme(legend.position = "none",
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))


g1
g1 + g2 + plot_layout(ncol = 2)

ggsave(filename = "./2024/20240129/v01-01.png", 
       device = grDevices::png,
       width = 10, height = 4.4, dpi = 180, units = "in")


drop_sido = sido |> st_drop_geometry()



sido3 = sido |> mutate(CTPRVN_CD = ifelse(CTPRVN_CD == 36, 44, CTPRVN_CD)) |> 
  group_by(CTPRVN_CD) |> 
  summarise() |> 
  left_join(st_drop_geometry(sido), by = join_by(CTPRVN_CD)) |> 
  select(1, 3:4, 2) |> 
  mutate(yn = CTPRVN_CD == 44, .before = geometry)

sido3

get_center(sido3)

ggplot() +
  geom_sf(data = sido3, aes(fill = yn)) + 
  geom_text(data = get_center(sido3), aes(x,y), label = v_water_mark,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  scale_fill_brewer(palette = "BrBG", direction = -1) +
  theme_bw() +
  theme(legend.position = "none",
        title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))

ggsave(filename = "./2024/20240129/v01-02.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

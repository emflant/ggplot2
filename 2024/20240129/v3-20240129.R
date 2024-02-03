source('./geo_core.R')

sido = sido_with_dokdo()
sido3 = sido |> mutate(CTPRVN_CD = ifelse(CTPRVN_CD == 36, 44, CTPRVN_CD)) |> 
  group_by(CTPRVN_CD) |> 
  summarise() |> 
  left_join(st_drop_geometry(sido), by = join_by(CTPRVN_CD)) |> 
  select(1, 3:4, 2) 

# excel_data = read_excel('./2024/20240129/data.xls') |> 
#   select(1:5, 삼겹살 = 7, 8:10) |> 
#   arrange(지역) |> 
#   print(n= 5)
# all_data = excel_data |>
#   bind_cols(sido3 |> st_drop_geometry() |> 
#               arrange(CTP_KOR_NM) |> 
#               select(1))

all_data |> 
  print(n = 5)
# sido3 |> st_drop_geometry() |> 
#   arrange(CTP_KOR_NM)

data = read_excel('./2024/20240129/data.xls') |> 
  select(지역, 삼겹살 = `삼겹살(환산후)`) |> 
  mutate(삼겹살 = as.numeric(삼겹살)) |> 
  arrange(지역) |> 
  bind_cols(sido3 |> st_drop_geometry() |> 
              arrange(CTP_KOR_NM) |> 
              select(1))

sido5 = sido3 |> 
  left_join(data, by = join_by(CTPRVN_CD)) |> 
  select(1:3, 5:6, 4)

sido5

ggplot() +
  geom_sf(data = sido5, aes(fill = 삼겹살)) +
  geom_text(data = get_center(sido5), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  scale_fill_viridis_c(option = "B", begin = 0.5, direction = -1,
                       labels = label_comma(suffix = "원")) +
  theme_bw(base_family = v_font_bold) +
  theme(title = element_blank(),
        plot.margin = margin(0,0.1,0.1,0.1, "in"))

ggsave(filename = "./2024/20240129/v03-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in")





# read_excel('./2024/20240129/data.xls')
all_data = read_excel('./2024/20240129/data.xls') |> 
  select(1:5, 삼겹살 = 7, 8:10) |> 
  arrange(지역) |> 
  bind_cols(sido3 |> st_drop_geometry() |> 
              arrange(CTP_KOR_NM) |> 
              select(1))

# excel_data = read_excel('./2024/20240129/data.xls') |> 
#   select(1:5, 삼겹살 = 7, 8:10) |> 
#   arrange(지역)
# 
# sido3 |> st_drop_geometry() |> 
#   arrange(CTP_KOR_NM) |> 
#   select(1)
  

all_data

sido66 = sido3 |> 
  left_join(all_data, by = join_by(CTPRVN_CD)) 
sido66
sido77 = sido66 |> 
  pivot_longer(냉면:삼계탕) |> 
  mutate(value = as.numeric(value))
# sido77_2
sido77_2 = sido77 |> 
  group_by(name) |> 
  mutate(get_center(sido77), n = row_number()) |> 
  mutate(x = ifelse(n == 1, x, NA),
         y = ifelse(n == 1, y, NA))


sido77_2$value |> min()
ggplot(data = sido77_2) +
  geom_sf(aes(fill = value)) +
  geom_text(aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  scale_fill_viridis_c(option = "B", begin = 0.4, direction = -1,
                       breaks = c(5923, 19429), labels = label_comma(suffix = "원")) +
  coord_sf(datum = st_crs(5179)) +
  facet_wrap(vars(name)) +
  theme_void(base_family = "GmarketSansTTFMedium") +
  theme(title = element_blank(), 
        legend.position = c(.91, -0.05),
        legend.direction = "horizontal",
        legend.key.width = unit(0.3, "in"),
        legend.key.height = unit(0.1, "in"),
        # legend.position = "bottom",
        plot.margin = margin(0,0.05,0.7,0.05, "in"), 
        plot.background = element_rect(fill = "gray100", colour = "gray100"),
        panel.background = element_rect(fill = "gray97"),
        panel.border = element_rect(colour = v_dark_bgcolor, fill = NA),
        strip.background = element_rect(fill = v_dark_bgcolor, colour = v_dark_bgcolor),
        strip.text = element_text(margin = margin(0.1,0.1,0.1,0.1,"in"),
                                  colour = "gray100", size = 12,
                                  family = "GmarketSansTTFBold"),
        # panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())

ggsave(filename = "./2024/20240129/v03-03.png", 
       device = grDevices::png,
       width = 10, height = 7.2, dpi = 180, units = "in")
 #######################################################

sido88 = sido77 |> 
  group_by(name) |> 
  mutate(min_value = min(value),
         max_value = max(value)) |> 
  mutate(norm = (value - min_value) / (max_value - min_value))
  

sido88 |> head()
# ggplot() +
#   geom_sf(data = sido88, aes(fill = norm)) +
#   scale_fill_viridis_c(option = "C", begin = 0.5, direction = -1,
#                        labels = label_comma(suffix = "원")) +
#   coord_sf(datum = st_crs(5179)) +
#   facet_wrap(vars(name)) +
#   theme_linedraw(base_family = v_font_bold) +
#   theme(title = element_blank(), legend.position = "none",
#         plot.margin = margin(0,0.1,0.1,0.1, "in"), 
#         panel.border = element_rect(colour = v_dark_bgcolor),
#         strip.background = element_rect(fill = v_dark_bgcolor),
#         # panel.grid = element_blank(),
#         axis.text = element_blank(), axis.ticks = element_blank())
# 
# 
# ggsave(filename = "./2024/20240129/v03-02.png", 
#        device = grDevices::png,
#        width = 10, height = 6.6, dpi = 240, units = "in")



cent = get_center(sido88)

sido99 = sido88 |> 
  mutate(get_center(sido88), n = row_number()) |> 
  mutate(x = ifelse(n == 1, x, NA),
         y = ifelse(n == 1, y, NA))

sido99
sido99 |> view()


sido99 |> ungroup() |> st_drop_geometry() |> distinct(CTPRVN_CD)
scale_fill_brewer()

ggplot(data = sido99) +
  geom_sf(aes(fill = norm)) +
  geom_text(aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  scale_fill_viridis_c(option = "G", begin = 0.5, direction = -1,
                       breaks = c(0,1), labels = c('저가', '고가')) +
  coord_sf(datum = st_crs(5179)) +
  facet_wrap(vars(name)) +
  theme_void(base_family = "GmarketSansTTFMedium") +
  theme(title = element_blank(), 
        # legend.position = "none",
        legend.position = c(.91, -0.05),
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.3, "in"),
        legend.key.height = unit(0.1, "in"),
        plot.margin = margin(0,0.05,0.7,0.05, "in"), 
        plot.background = element_rect(fill = "gray100", colour = "gray100"),
        panel.background = element_rect(fill = "gray97"),
        panel.border = element_rect(colour = v_dark_bgcolor, fill = NA),
        strip.background = element_rect(fill = v_dark_bgcolor, colour = v_dark_bgcolor),
        strip.text = element_text(margin = margin(0.1,0.1,0.1,0.1,"in"),
                                  colour = "gray100", size = 12,
                                  family = "GmarketSansTTFBold"),
        # panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())


ggsave(filename = "./2024/20240129/v03-02.png", 
       device = grDevices::png,
       width = 10, height = 7.4, dpi = 180, units = "in")

ggplot(data = sido99) +
  geom_sf(aes(fill = norm)) +
  geom_text(aes(x,y), label = v_water_mark, na.rm = T,
               colour = "gray0",size = 7, alpha = 0.2,
               family = v_font_heavy) +
  scale_fill_viridis_c(option = "B", begin = 0.4, direction = -1,
                       breaks = c(0,1), labels = c('저가', '고가')) +
  coord_sf(datum = st_crs(5179)) +
  facet_wrap(vars(name)) +
  theme_void(base_family = "GmarketSansTTFMedium") +
  theme(title = element_blank(), 
        # legend.position = "none",
        legend.position = c(.91, -0.05),
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.key.width = unit(0.3, "in"),
        legend.key.height = unit(0.1, "in"),
        plot.margin = margin(0,0.05,0.7,0.05, "in"), 
        plot.background = element_rect(fill = "gray100", colour = "gray100"),
        panel.background = element_rect(fill = "gray97"),
        panel.border = element_rect(colour = v_dark_bgcolor, fill = NA),
        strip.background = element_rect(fill = v_dark_bgcolor, colour = v_dark_bgcolor),
        strip.text = element_text(margin = margin(0.1,0.1,0.1,0.1,"in"),
                                  colour = "gray100", size = 12,
                                  family = "GmarketSansTTFBold"),
        # panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())


ggsave(filename = "./2024/20240129/v03-04.png", 
       device = grDevices::png,
       width = 10, height = 7.4, dpi = 180, units = "in")

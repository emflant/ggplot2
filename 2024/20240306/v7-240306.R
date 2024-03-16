source('./geo_core.R')

stringi::stri_trans_general('선거구-경기도', "Hangul-Latin")

map_dong1 = read_sf('~/data/map/census_2023/bnd_dong_00_2023_2023/bnd_dong_00_2023_2023_2Q.shp') |> 
  clean_names()

map_sido1 = read_sf('~/data/map/census_2023/bnd_sido_00_2023_2023/bnd_sido_00_2023_2023_2Q.shp') |> 
  clean_names() |> 
  st_drop_geometry()
map_sido1

map_sigungu1 = read_sf('~/data/map/census_2023/bnd_sigungu_00_2023_2023/bnd_sigungu_00_2023_2023_2Q.shp') |> 
  clean_names() |> 
  st_drop_geometry()
map_sigungu1

map_dong1
map_dong2 = map_dong1 |> mutate(sido_cd = str_sub(adm_cd, 1,2), .before = geometry) |> 
  mutate(sigungu_cd = str_sub(adm_cd,1,5), .before = geometry) |> 
  left_join(map_sigungu1, by = join_by(sigungu_cd)) |> 
  left_join(map_sido1, by = join_by(sido_cd)) |> 
  select(adm_nm, adm_cd, sido_cd, sido_nm, sigungu_cd, sigungu_nm)

map_dong2_gyeonggido = map_dong2 |> filter(str_detect(sido_cd, '31'))

map_dong2
# map_dong2 |> filter()

map_dong2_gyeonggido
######################################################################################

gyeonggido = read_excel('~/data/map/vote/seongeogu-gyeong-gido.xlsx', skip = 3) |> 
  janitor::clean_names() |> 
  fill(seongeogumyeong)

# gyeonggido |> filter(is.na(seongeogumyeong))
gyeonggido
gyeonggido |> 
  fill(seongeogumyeong) |> 
  filter(is.na(seongeogumyeong))
fill

gyeonggido2 = gyeonggido |> 
  mutate(dong1 = map_vec(eubmyeondongmyeong, str_split, ',')) |> 
  unnest_longer(dong1) |> 
  select(-4) |> 
  mutate(dong1 = map_chr(dong1, trimws))
gyeonggido2
# gyeonggido3 = gyeonggido2
# gyeonggido2 = gyeonggido2 |> 
#   mutate(gusigunmyeong = ifelse(str_detect(dong1, '(성동구)'), '성동구', gusigunmyeong)) |> 
#   mutate(dong1 = str_replace(dong1, '\\(성동구\\)', ''))

# gyeonggido3 = gyeonggido2 |> 
#   mutate(dong2 = str_extract(dong1, '제\\d+.*동$')) |> 
#   mutate(dong3 = str_sub(dong2, 2)) |> 
#   mutate(dong4 = str_replace(dong1, dong2, '')) |> 
#   mutate(dong5 = str_c(dong4, dong3)) |> 
#   mutate(dong = ifelse(is.na(dong5), dong1, dong5), .before = dong1)

gyeonggido2 |> 
  filter(str_detect(dong1, '\\(.+\\)')) |>
  mutate(dong2 = str_extract(dong1, '\\(.+\\)') |> str_sub(2,-2)) |> 
  mutate(dong3 = ifelse(str_detect(dong2, '^부천시'), '부천시', dong2)) |> 
  print(n = Inf)

gyeonggido3 = gyeonggido2 |> 
  mutate(gusigunmyeong2 = str_extract(dong1, '\\(.+\\)') |> str_sub(2,-2)) |> 
  mutate(gusigunmyeong3 = ifelse(is.na(gusigunmyeong2), gusigunmyeong, gusigunmyeong2)) |> 
  mutate(gusigunmyeong4 = ifelse(str_detect(gusigunmyeong3, '^부천시'), '부천시', gusigunmyeong3)) |> 
  mutate(gusigunmyeong5 = ifelse(str_detect(gusigunmyeong4, '^화성시'), '화성시', gusigunmyeong4)) |> 
  mutate(dong2 = str_replace(dong1, '\\(.+\\)', '')) |> 
  select(seongeogumyeong, gusigunmyeong = gusigunmyeong5, dong = dong2)

# 화성시갑 화성시을 정리필요.

gyeonggido3 |> filter(dong == '율천동')


map_sigungu1
map_sigungu2 = map_sigungu1 |> 
  filter(str_detect(sigungu_cd, '^31')) |> 
  mutate(sigungu_nm2 = str_replace_all(sigungu_nm, '\\s+', ''))
map_sigungu2

# map_sigungu2 |> 
#   filter(str_detect(sigungu_nm2, '부천시'))


# gyeonggido3 |> 
#   filter(str_detect(gusigunmyeong, '부천시')) |> 
#   print(n = Inf)

gyeonggido4 = gyeonggido3 |> 
  left_join(map_sigungu2, 
            by = join_by(gusigunmyeong == sigungu_nm2)) 


gyeonggido4 |> filter(is.na(sigungu_nm)) |> 
  print(n = Inf)

gyeonggido4

######################################################################################



map_dong2_gyeonggido |> filter(adm_nm == '행신3동')
gyeonggido4 |> filter(dong == '행신3동')
map_dong2_gyeonggido2 = map_dong2_gyeonggido |> 
  ms_simplify(keep = 0.01) |> 
  left_join(gyeonggido4, by = join_by(adm_nm == dong, sigungu_cd)) |> 
  relocate(geometry, .after = last_col())



map_dong2_gyeonggido2 |> 
  filter(is.na(seongeogumyeong)) |> 
  print(n = Inf)

map_dong2_gyeonggido |> 
  filter(str_detect(adm_nm, '심곡'))
gyeonggido4 |> 
  filter(str_detect(dong, '심곡'))


# http://bucheon.go.kr/site/program/board/photoboard/view?menuid=148002006001&pagesize=12&boardtypeid=28488&encid=PqLz5QiRIuza8RabtJPWGQ==
# 부천시 
# 심곡동 --> 심곡1동,심곡2동,심곡3동,원미2동,소사동
# 부천동 --> 원미1동,역곡1동,역곡2동,춘의동,도당동
# 중동 --> 중동,상동
# 신중동 --> 약대동,중1동,중2동,중3동,중4동
# 상동 --> 상1동,상2동,상3동
# 대산동 --> 심곡본1동,심곡본동,송내1동,송내2동
# 소사본동 --> 소사본동,소사본1동
# 범안동 --> 범박동,옥길동,괴안동,역곡3동
# 성곡동 --> 성곡동,고강본동,고강1동
# 오정동 --> 원종1동,원종2동,오정동,신흥동

# https://www.yna.co.kr/view/AKR20231206080500061
# 오산시
# 신장동 --> 신장1동,신장2동 
# 대원동 --> 대원1동,대원2동


# http://news.lghellovision.net/news/articleView.html?idxno=416303
# 양주시 행정동 설치 조례와 읍·면·동 행정복지센터 소재지 조례 개정안
# 양주시
# 회천4동 --> 옥정1동, 옥정2동

# 안양시
# 안양시 '석수3동·관양1동·관양2동→충훈동·관양동·인덕원동' 변경
# 석수3동 --> 충훈동
# 관양1동 --> 관양동
# 관양2동 --> 인덕원동

map_dong2_gyeonggido3 = map_dong2_gyeonggido2 |> 
  group_by(seongeogumyeong) |> 
  summarise() 
map_dong2_gyeonggido3

map_dong2_gyeonggido4 = map_dong2_gyeonggido3 |> 
  mutate(sig = str_replace(seongeogumyeong, '(갑|을|병|정|무)$', ''), .before = geometry) |> 
  mutate(knd1 = str_extract(seongeogumyeong, '(갑|을|병|정|무)$'), .before = geometry) |> 
  mutate(knd2 = factor(knd1, levels = c("갑", "을", "병", "정", "무")), .before = geometry) |>
  arrange(sig, knd2) |> 
  mutate(knd3 = row_number())

map_dong2_gyeonggido4
map_dong2_gyeonggido4 |> print(n = Inf)
# a = c("을", "갑", "정", "무", "병")
# 
# factor(a)
# factor(a, levels = c("갑", "을", "병", "정", "무"))
# 
# factor(a, levels = c("갑", "을", "병", "정", "무")) |> order()

map_dong2_seoul4

# map_dong2_seoul4 |>  print(n = Inf)
# map_dong2_seoul5 = map_dong2_seoul4 |> 
#   ms_simplify(keep = 0.01)
map_dong2_seoul3
map_dong2_seoul2

map_dong2_seoul4 |> filter(knd2 == "병")

# map_dong2_seoul4 |> filter(str_detect(seongeogumyeong, "강서구")) |> 
map_dong2_gyeonggido4 |> filter(str_detect(seongeogumyeong, "고양시"))
ggplot() + 
  geom_sf(data = map_dong2_gyeonggido4 |> filter(str_detect(seongeogumyeong, "고양시")), 
          aes(fill = seongeogumyeong ) )+
  scale_fill_viridis_d(option = "E", begin = 0.3, end = 0.8) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())

ggplot() + 
  geom_sf(data = map_dong2_seoul4 |> filter(str_detect(seongeogumyeong, "강남구")), 
          aes(fill = reorder(seongeogumyeong, knd3) ) ) +
  scale_fill_viridis_d(option = "E", begin = 0.3, end = 0.8) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())


map_seongeogu = function(vmap, vdongmap, vTitle = "고양시", voption = "D", vbegin = 0.3, vend = 0.8){
  design <- "
    1222
    1222
  "
  
  vmap_2 = vmap |> filter(str_detect(sig, vTitle))
  
  g1_bg = "gray95"
  
  # g1 = ggplot() +
  #   geom_sf(data = vmap, fill = 'gray70', colour = g1_bg) +
  #   geom_sf(data = vmap_2, fill = "gray40", colour = g1_bg) +
  #   labs(title = "서울특별시\n49개 선거구 지도") +
  #   # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  #   # facet_wrap(vars(sig)) +
  #   theme_void(base_family = v_font_gmarket_bold) +
  #   theme(legend.position = "none", 
  #         plot.background = element_rect(fill = g1_bg, colour = g1_bg),
  #         plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
  #         plot.margin = margin(0.1,0.15,0.1,0.1,"in"))
  
  g2_bg = "gray97"
  
  g2 = ggplot() +
    # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
    geom_sf(data = vmap_2, aes(fill = reorder(seongeogumyeong, knd3)), 
            colour = g2_bg, linewidth = 0.5) +
    geom_sf(data = vdongmap |> filter(str_detect(seongeogumyeong, vTitle)), fill = NA,
            colour = g2_bg, linewidth = 0.1) +
    geom_text(data = get_center(vmap_2), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 5, alpha = 0.5,
              family = v_font_heavy) +
    scale_fill_viridis_d(option = voption, begin = vbegin, end = vend) +
    labs(title = str_c(vTitle, " 선거구")) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium),
          legend.position = "bottom",
          plot.background = element_rect(fill = g2_bg, colour = g2_bg),
          plot.margin = margin(0.2,0.1,0.2,0.1,"in"),
          plot.title = element_text(hjust = 0.5, size = 17, colour = "gray30"))
  
  # g1 + g2 + plot_layout(design = design)
  g2
}

# map_dong2_gyeonggido4
map_dong2_gyeonggido4
map_seongeogu(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "고양시", voption = "E")

ggsave(filename = "./2024/20240306/v7-01.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "화성시", voption = "E")

ggsave(filename = "./2024/20240306/v7-02.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "성남시", voption = "E")

ggsave(filename = "./2024/20240306/v7-03.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "용인시", voption = "E")

ggsave(filename = "./2024/20240306/v7-04.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 




map_seongeogu2 = function(vmap, vdongmap, vTitle = "고양시", voption = "RdYlBu", vn = 4){
  design <- "
    1222
    1222
  "
  
  vmap_2 = vmap |> filter(str_detect(sig, vTitle))
  
  g1_bg = "gray90"
  
  # g1 = ggplot() +
  #   geom_sf(data = vmap, fill = 'gray70', colour = g1_bg) +
  #   geom_sf(data = vmap_2, fill = "gray40", colour = g1_bg) +
  #   labs(title = "서울특별시\n49개 선거구 지도") +
  #   # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  #   # facet_wrap(vars(sig)) +
  #   theme_void(base_family = v_font_gmarket_bold) +
  #   theme(legend.position = "none", 
  #         plot.background = element_rect(fill = g1_bg, colour = g1_bg),
  #         plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
  #         plot.margin = margin(0.1,0.15,0.1,0.1,"in"))
  
  g2_bg = "gray20"
  
  g2 = ggplot() +
    # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
    geom_sf(data = vmap_2, aes(fill = reorder(seongeogumyeong, knd3)), 
            colour = g2_bg, linewidth = 0.5) +
    geom_sf(data = vdongmap |> filter(str_detect(seongeogumyeong, vTitle)), fill = NA,
            colour = g2_bg, linewidth = 0.1) +
    geom_text(data = get_center(vmap_2), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray100",size = 7, alpha = 0.3,
              family = v_font_heavy) +
    # scale_fill_brewer(palette = voption, direction = vdirection, type = "div" ) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = vn, name = voption) |> rev()) +
    labs(title = str_c(vTitle, " 선거구")) +
    theme_void(base_family = v_font_gmarket_bold) +
    theme(legend.title = element_blank(),
          legend.text = element_text(family = v_font_gmarket_medium, colour = "gray80"),
          legend.position = "bottom",
          plot.background = element_rect(fill = g2_bg, colour = g2_bg),
          plot.margin = margin(0.3,0.1,0.3,0.1,"in"),
          # plot.margin = margin(0,0,0,0,"in"),
          plot.title = element_text(hjust = 0.5, size = 17, colour = "gray80"))
  
  # g1 + g2 + plot_layout(design = design)
  g2
}


RColorBrewer::brewer.pal(n = 7, name = "RdBu") |> rev()
RColorBrewer::display.brewer.all()
ggplot() + 
  geom_sf(data = map_dong2_gyeonggido4 |> filter(str_detect(seongeogumyeong, "고양시")), 
          aes(fill = seongeogumyeong ) )+
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "RdYlBu") |> rev()) +
  theme_bw(base_family = v_font_bold2) +
  theme(title = element_blank())



map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "고양시", vn = 6)

ggsave(filename = "./2024/20240306/v7-11.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "화성시", vn = 5)

ggsave(filename = "./2024/20240306/v7-12.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "성남시", vn = 4)

ggsave(filename = "./2024/20240306/v7-13.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "용인시", voption = "BrBG", vn = 4)

ggsave(filename = "./2024/20240306/v7-04.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


# , voption = "BrBG"
g2 = map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "화성시", voption = "BrBG", vdirection = -1)
g1 = map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "고양시", vdirection = -1)

g4 = map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "용인시", vdirection = -1)

g1
design <- "
    111
    111
    222
    222
  "
# g1 + plot_spacer() +g4 
# inset_element()
# plot_spacer() +
#   theme(plot.background = element_rect(fill = "gray20", colour = "gray20")) +
#   g1 + g4
# patchwork::plot_spacer()

g1 + g4 + plot_layout(design = design) +
  theme(plot.background = element_rect(colour = "gray100"))

# g1 / g4

ggsave(filename = "./2024/20240306/v7-21.png", 
       device = grDevices::png,
       width = 8, height = 10, dpi = 180, units = "in")






scale_fill_distiller()
map_dong2_gyeonggido4
map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "수원시")

ggsave(filename = "./2024/20240306/v7-31.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "용인시", voption = "RdBu")

ggsave(filename = "./2024/20240306/v7-32.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 


map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "고양시", voption = "RdBu")

ggsave(filename = "./2024/20240306/v7-33.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "광주시", vn = 4)
map_seongeogu2(map_dong2_gyeonggido4, map_dong2_gyeonggido2, "광명시", vn = 5)

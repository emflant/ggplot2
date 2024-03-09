source('./geo_core.R')

vkeep = 0.001
read_sf('~/data/map/Z_SOP_BND_ADM_DONG_PG/Z_SOP_BND_ADM_DONG_PG.shp')
map_dong1 = read_sf('~/data/map/Z_SOP_BND_ADM_DONG_PG/Z_SOP_BND_ADM_DONG_PG.shp') |> 
  mutate(ADM_DR_NM = iconv(ADM_DR_NM, from = "EUC-KR", to = 'UTF-8')) |> 
  st_set_crs(5181)

map_dong1

# 동대문구갑	1	동대문구	용신동, 제기동, 청량리동, 회기동, 휘경제1동, 휘경제2동, 이문제1동, 이문제2동
# 동대문구을	1	동대문구	전농제1동, 전농제2동, 답십리제1동, 답십리제2동, 장안제1동, 장안제2동
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

map_dong2 = map_dong1 |> mutate(sido_cd = str_sub(adm_cd, 1,2), .before = geometry) |> 
  mutate(sigungu_cd = str_sub(adm_cd,1,5), .before = geometry)


################################################################################
map_dong1
map_dong1 |> 
  filter(str_detect(adm_nm, '회기동|답십리' ))

map_dong2 = map_dong1 |> 
  filter(str_detect(adm_cd, '^11060' )) 
map_dong2
# stringi::stri_trans_general('선거구_서울', "Hangul-Latin")
map_dong2
ggplot(map_dong2) +
  geom_sf()

map_dong1 |> filter(str_detect(ADM_NM, '상계'))
tibble(n = '동대문구갑', dong_nm = dongdaemungu_gab)

seongeogu_seoul = read_excel('~/data/map/vote/seongeogu_seoul.xlsx', skip = 3) |> 
  janitor::clean_names()

seongeogu_seoul_f = seongeogu_seoul |> 
  mutate(dong1 = map_vec(eubmyeondongmyeong, str_split, ',')) |> 
  unnest_longer(dong1) |> 
  select(-4) |> 
  mutate(dong1 = map_chr(dong1, trimws))

seongeogu_seoul_f

# seongeogu_dongdaemun_f = seongeogu_seoul_f |> filter(gusigunmyeong == '동대문구') |> 
#   mutate(dong2 = str_extract(dong1, '제\\d+.*동$')) |> 
#   mutate(dong3 = str_sub(dong2, 2)) |> 
#   mutate(dong4 = str_replace(dong1, dong2, '')) |> 
#   mutate(dong5 = str_c(dong4, dong3)) |> 
#   mutate(dong = ifelse(is.na(dong5), dong1, dong5), .before = dong1)
#   
#   mutate(dong6 = is.na(dong5, dong1))
# 
#   z <- c(1, NaN, NA, 2, NaN)
#   na_if(z, 3)
# 
# seongeogu_seoul_f |> 
#   mutate(dong2 = str_extract(dong1, '제\\d+.*동$')) |> 
#   mutate(dong3 = str_sub(dong2, 2)) |> 
#   view()
# 
# 
# map_dong2 |> left_join(seongeogu_dongdaemun_f, by = join_by(adm_nm == dong)) |> 
#   filter(is.na(seongeogumyeong))
#   
# 
# map_dong2 |> left_join(seongeogu_dongdaemun_f, by = join_by(adm_nm == dong)) |> 
#   filter(is.na(seongeogumyeong))


#######################


seongeogu_seoul1 = seongeogu_seoul |> 
  mutate(dong1 = map_vec(eubmyeondongmyeong, str_split, ',')) |> 
  unnest_longer(dong1) |> 
  select(-4) |> 
  mutate(dong1 = map_chr(dong1, trimws))

# seongeogu_seoul2 = seongeogu_seoul1 |> filter(str_detect(dong1, '(성동구)')) |> 
#   mutate(gusigunmyeong = ifelse(str_detect(dong1, '(성동구)'), '성동구', gusigunmyeong)) |> 
#   mutate(dong1 = str_replace(dong1, '\\(성동구\\)', ''))

seongeogu_seoul2 = seongeogu_seoul1 |> 
  mutate(gusigunmyeong = ifelse(str_detect(dong1, '(성동구)'), '성동구', gusigunmyeong)) |> 
  mutate(dong1 = str_replace(dong1, '\\(성동구\\)', ''))

seongeogu_seoul3 = seongeogu_seoul2 |> 
  mutate(dong2 = str_extract(dong1, '제\\d+.*동$')) |> 
  mutate(dong3 = str_sub(dong2, 2)) |> 
  mutate(dong4 = str_replace(dong1, dong2, '')) |> 
  mutate(dong5 = str_c(dong4, dong3)) |> 
  mutate(dong = ifelse(is.na(dong5), dong1, dong5), .before = dong1)

seongeogu_seoul3

seongeogu_seoul4 = seongeogu_seoul3 |> 
  left_join(map_sigungu1 |> 
              filter(str_detect(sigungu_cd, '^11')), 
            by = join_by(gusigunmyeong == sigungu_nm)) |> 
  select(1,3,4,11)
seongeogu_seoul4


# map_dong1_seoul = map_dong2 |> filter(str_detect(sido_cd, '11')) |> 
#   st_drop_geometry()
map_dong1_seoul = map_dong2 |> filter(str_detect(sido_cd, '11'))

map_dong1_seoul
1208
2127

map_dong1_seoul2 = map_dong1_seoul |> 
  left_join(seongeogu_seoul4, by = join_by(adm_nm == dong, sigungu_cd)) |> 
  relocate(geometry, .after = last_col())

# map_dong1_seoul |> left_join(seongeogu_seoul4, by = join_by(adm_nm == dong, sigungu_cd)) |> 
#   filter(is.na(seongeogumyeong))



map_dong1_seoul2 |> distinct(seongeogumyeong)
map_dong1_seoul2
map_dong1_seoul2 |> filter(str_detect(gusigunmyeong, '동대문구'))

ggplot(map_dong1_seoul2 |> filter(str_detect(gusigunmyeong, '동대문구'))) +
  geom_sf(aes(fill = seongeogumyeong)) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  theme(legend.position = "none")

map_dong1_seoul3 = map_dong1_seoul2 |> 
  group_by(seongeogumyeong) |> 
  summarise() 
map_dong1_seoul3
map_dong1_seoul4 = map_dong1_seoul3 |> 
  mutate(sig = str_replace(seongeogumyeong, '(갑|을|병|정)$', ''), .before = geometry)

map_dong1_seoul4

str_replace('갑을병정을', '(갑|을|병)$', "")



map_dong1_seoul5 = map_dong1_seoul4 |> 
  ms_simplify(keep = 0.01)

map_dong1_seoul5

design <- "
  1222
  1222
"

g1 = ggplot() +
  geom_sf(data = map_dong1_seoul5, colour = "gray90", fill = 'gray70') +
  geom_sf(data = map_dong1_seoul5 |> filter(str_detect(sig, '강북구')),
          fill = "gray40", colour = "gray90") +
  labs(title = "서울특별시\n49개 선거구 지도") +
  # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  # facet_wrap(vars(sig)) +
  theme_void(base_family = 'GmarketSansTTFBold') +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "gray90", colour = "gray90"),
        plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
        plot.margin = margin(0.1,0.15,0.1,0.1,"in"))


g2 = ggplot() +
  # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
  geom_sf(data = map_dong1_seoul5 |> filter(str_detect(sig, '강북구')),
          aes(fill = seongeogumyeong), colour = "gray100") +
  scale_fill_viridis_d(option = "D", begin = 0.3, end = 0.8) +
  labs(title = "강북구 선거구") +
  # facet_wrap(vars(sig)) +
  theme_void(base_family = 'GmarketSansTTFBold') +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = "GmarketSansTTFMedium"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))

g1 + g2 + plot_layout(design = design)

ggsave(filename = "./2024/20240306/v1-11.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 

#영등포구갑 49.1 34.7

# ggplot() +
#   # geom_sf(data = map_dong1_seoul4, colour = "gray100") +
#   geom_sf(data = map_dong1_seoul4,
#           aes(fill = seongeogumyeong), colour = "gray100") +
#   scale_fill_viridis_d(option = "C", begin = 0.3, end = 0.7) +
#   facet_wrap(vars(sig)) +
#   theme_bw(base_family = v_font_bold2) +
#   theme(legend.position = "none")



g1 = ggplot() +
  geom_sf(data = map_dong1_seoul5, colour = "gray90", fill = 'gray70') +
  geom_sf(data = map_dong1_seoul5 |> filter(str_detect(sig, '송파구')),
          fill = "gray40", colour = "gray90") +
  labs(title = "서울특별시\n49개 선거구 지도") +
  # scale_fill_viridis_d(option = "D", begin = 0, end = 1) +
  # facet_wrap(vars(sig)) +
  theme_void(base_family = 'GmarketSansTTFBold') +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "gray90", colour = "gray90"),
        plot.title = element_text(hjust = 0, size = 12, colour = "gray30"),
        plot.margin = margin(0.1,0.15,0.1,0.1,"in"))


g2 = ggplot() +
  # geom_sf(data = map_dong1_seoul4, colour = "gray100", fill = 'gray90', linewidth = 0.3) +
  geom_sf(data = map_dong1_seoul5 |> filter(str_detect(sig, '송파구')),
          aes(fill = seongeogumyeong), colour = "gray100") +
  scale_fill_viridis_d(option = "B", begin = 0.3, end = 0.8) +
  labs(title = "송파구 선거구") +
  # facet_wrap(vars(sig)) +
  theme_void(base_family = 'GmarketSansTTFBold') +
  theme(legend.title = element_blank(),
        legend.text = element_text(family = "GmarketSansTTFMedium"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15, colour = "gray30"))

g1 + g2 + plot_layout(design = design)

ggsave(filename = "./2024/20240306/v1-12.png", 
       device = grDevices::png,
       width = 6, height = 4, dpi = 180, units = "in") 



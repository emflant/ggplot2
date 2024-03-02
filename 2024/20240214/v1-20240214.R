source('./geo_core.R')

map1 = read_sf('~/data/map/grid/%2Fgrid_border%2Fgrid_2023%2Fgrid_%EA%B0%80%EB%8B%A4%2Fgrid_%EA%B0%80%EB%8B%A4/grid_gada_1K.shp')


map1

ggplot(map1) +
  geom_sf()

map2 = read_sf('~/data/map/grid/%2Fgrid_border%2Furban%2F%EB%8F%84%EC%8B%9C%ED%99%94%EA%B2%BD%EA%B3%84_2021%2F%EB%8F%84%EC%8B%9C%ED%99%94%EA%B2%BD%EA%B3%84_2021/도시화경계_2021_도시.shp')

ggplot(map2) +
  geom_sf()

map2 |> print(n = Inf)

map2 |> 
  filter(str_detect(urban_nm, '서울'))
map3 = map2 |> 
  filter(urban_id == 'Center_019')

map3

ggplot(map3) +
  geom_sf()


map30 = read_sf('~/data/map/grid/grid_border_Furban_2021/grid_2021_subdosi.shp') 


ggplot(map30) +
  geom_sf()

snakecase::to_any_case("iDontKnow")

t = '%2Fgrid_border%2Fgrid_2023%2Fgrid_%EB%9D%BC%EB%8B%A4%2Fgrid_%EB%9D%BC%EB%8B%A4'
t |> 
  URLdecode() |> 
  str_replace_all('/', "_") |> 
  make_clean_names()
  

list.files(path = '~/data/map/grid') |> 
  enframe(name = NULL)

list.files(path = '~/data/map/grid/from') |> 
  enframe(name = NULL) |> 
  mutate(value = map_chr(value, URLdecode), .before = value) |> 
  mutate(value = str_sub(value, 32))

list.files(path = '~/data/map/grid/from') |> 
  enframe(name = NULL) |> 
  mutate(value = map_chr(value, URLdecode), .before = value) |> 
  mutate(value = str_sub(value, 32)) |> 
  mutate(trans = map_chr(value, stringi::stri_trans_general, "Hangul-Latin"))


list.files(path = '~/data/map/grid/from') |> 
  enframe(name = NULL) |> 
  mutate(value = map_chr(value, URLdecode), .before = value) |> 
  mutate(value = str_sub(value, 0, 31) )

list.files(path = '~/data/map/grid/from') |> 
  enframe(name = NULL) |> 
  mutate(file_name = map_chr(value, URLdecode), .before = value) |> 
  mutate(file_name = str_extract(file_name, '/\\w+\\.zip')) |> 
  mutate(file_name = str_sub(file_name, 0, -5)) |> 
  mutate(file_name = str_c(make_clean_names(file_name), '.zip'))

zip_list = list.files(path = '~/data/map/grid/from') |> 
  enframe(name = NULL) |> 
  mutate(file_name = map_chr(value, URLdecode), .before = value) |> 
  mutate(file_name = str_extract(file_name, '/\\w+\\.zip')) |> 
  mutate(file_name = str_sub(file_name, 0, -5)) |> 
  mutate(file_name = str_c(make_clean_names(file_name), '.zip')) |> 
  mutate(to_dir = '~/data/map/grid/to/', 
         from_dir = '~/data/map/grid/from/', 
         .before = file_name) |> 
  mutate(from_path = str_c(from_dir, value), 
         to_path = str_c(to_dir, file_name), 
         .before = to_dir) 
  
zip_list |> view()

map2_lgl(zip_list$from_path, zip_list$to_path, file.copy, overwrite = F, copy.date = T)

file.copy(from = '~/data/map/grid/from/%2Fcensus%2Fdata_2023%2Fbnd_all%2Fbnd_all_00_2023_2023.zip', 
          to = '~/data/map/grid/to//bnd_all_00_2023_2023.zip')
 # %2F

list.files(path = '~/data/map/grid', full.names = T, recursive = T) |> 
  enframe(value = "full_path") |> 
  mutate(decode = map_chr(full_path, URLdecode)) |> 
  view()
URLdecode(t)

URLdecode(c('123', '456'))

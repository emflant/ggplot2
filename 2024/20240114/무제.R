

# 경도 위도를 EPSG:5179 거리체계로 
# 위도 : latitude 가로줄, y축.
# 경도 : longitude 세로줄, x축.
lim_data = tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") |> 
  st_transform("EPSG:5179")

lim_data
tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") 

tibble(lon = c(126, 127), lat = c(36, 37)) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs("EPSG:4326") 
source('./geo_core.R')


sig = sig_with_dokdo(v = 0.001)
sig
seoul_001 = sig |> filter(str_detect(sig_cd,'^11')) 
# seoul_1 = sig |> filter(str_detect(sig_cd,'^11')) 

seoul_001 = sig |> filter(str_detect(sig_cd,'^11230')) 

ggplot() +
  geom_sf(data = seoul_001) + 
  # geom_sf(data = box2) +
  coord_sf(datum = st_crs(5179))


box1 = st_bbox(seoul_001) + c(-10000, -10000, 10000, 10000)
box2 = round(box1, -4) 
box2
gap = 500
grid1 = expand.grid(x = seq(box2$xmin, box2$xmax, by = gap), 
                    y = seq(box2$ymin, box2$ymax, by = gap))
# grid1
sf_grid1 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = gap)) |> 
  pull(geometry) |> 
  st_sfc(crs = st_crs(5179)) |> 
  st_sf(geometry = _)


# sf_grid1 |> print(n = Inf)
# ggplot() +
#   geom_sf(data = seoul_001) + 
#   geom_sf(data = sf_grid1, fill = NA) +
#   coord_sf(datum = st_crs(5179))



log_within = st_within(sf_grid1, seoul_001) |> lengths() > 0
# log_within
# sf_grid1
ggplot() +
  # geom_sf(data = seoul_001) +
  geom_sf(data = sf_grid1[log_within,], fill = "#DB8582") +
  coord_sf(datum = st_crs(5179))


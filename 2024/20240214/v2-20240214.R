
# xmin      ymin      xmax      ymax 
# 747235.2 1468716.8 1391920.1 2065895.0 

# (800000, 1500000), (800000, 1501000), (801000, 1501000), (801000, 1500000), (800000, 1500000)
rbind(c(800000, 1500000), c(800000, 1501000), c(801000, 1501000), c(801000, 1500000), c(800000, 1500000)) |> 
  list()
st_polygon(list(c(800000, 1500000), c(800000, 1501000), c(801000, 1501000), c(801000, 1500000), c(800000, 1500000)))

grid1 = tibble(gap = 1000, x1 = 800000, y1 = 1500000) |>   
  mutate(x2 = x1, y2 = y1 + gap) |> 
  mutate(x3 = x1 + gap, y3 = y1 + gap) |> 
  mutate(x4 = x1 + gap, y4 = y1) |> 
  mutate(x5 = x1, y5 = y1) |> 
  mutate(p1 = map2(x1, y1, c)) |> 
  mutate(p2 = map2(x2, y2, c)) |> 
  mutate(p3 = map2(x3, y3, c)) |> 
  mutate(p4 = map2(x4, y4, c)) |> 
  mutate(p5 = map2(x5, y5, c)) |> 
  select(gap, x1, y1, p1:p5)

grid1

grid1 |> 
  pivot_longer(cols = p1:p5) |> 
  group_by(gap, x1, y1) 

grid1 |> 
  pivot_longer(cols = p1:p5) |> 
  group_by(gap, x1, y1) |> 
  summarise(rb = list(rbind(value))) |> 
  mutate(pol = map(rb, st_polygon))
  

c(800000, 1500000)


x = 801000
y = 1500000
gap = 1000

seq(800000, 810000, by = 1000)

tibble(x = seq(800000, 810000, by = 1000), y = 1500000) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1000)) |> 
  pull(geometry)

polygon_square = function(x, y, gap) {
  p1 = c(x, y) 
  p2 = c(x, y + gap)
  p3 = c(x + gap, y + gap)
  p4 = c(x + gap, y)
  p5 = c(x, y) 
  
  st_polygon(list(rbind(p1, p2, p3, p4, p5)))  
}


expand.grid(x = 1:3, y = 5:6)

grid1 = expand.grid(x = seq(800000, 810000, by = 1000), 
            y = seq(1500000, 1520000, by = 1000))

map2 = tibble(gap = 1000, grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1000)) |> 
  pull(geometry) |> 
  st_sfc()

ggplot(map2)

tibble(x = seq(800000, 810000, by = 1000), y = 1500000) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1000)) |> 
  pull(geometry) |> 
  st_sfc()


st1 = polygon_square(100, 100, 100)
st2 = polygon_square(200, 100, 100)

list(st1, st2)
st_sfc(list(st1, st2))









ggplot(map1) +
  geom_sf()
map_df

sido = sido_with_dokdo()

st_bbox(sido)


grid1 = expand.grid(x = seq(700000, 1500000, by = 10000), 
                    y = seq(1400000, 2100000, by = 10000))
grid1
map2 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 10000)) |> 
  pull(geometry) |> 
  st_sfc(crs = st_crs(5179))
map2

ggplot() +
  geom_sf(data = map2) +
  geom_sf(data = sido)


map2
inter1 = st_contains(map2, sido)
inter1
sel = lengths(inter1) > 0
sel
map2
map2[sel,]
ggplot() +
  geom_sf(data = inter1) 
nz_height

library(spData)

result2 = st_within(map2, sido)
result2 |> print(n = Inf)


map3 = map2[inter1]

ggplot() +
  geom_sf(data = map3) 





grid1 = expand.grid(x = seq(1, 10, by = 1), 
                    y = seq(1, 10, by = 1))
grid1
map2 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1)) |> 
  pull(geometry) |> 
  st_sfc()

sam1 = list(rbind(c(2,8), c(8,10), c(7,4), c(2,8))) |> 
  st_polygon() |> 
  st_sfc()
  
sam1 = list(rbind(c(3,7), c(9,9), c(8,3), c(3,7))) |> 
  st_polygon() |> 
  st_sfc()

ggplot() +
  geom_sf(data = map2) +
  geom_sf(data = sam1) +
  coord_sf(xlim = c(1,11), ylim = c(1,11)) +
  theme_bw()

gg = function(con){
  ggplot() +
    geom_sf(data = sf_map2[con,]) +
    geom_sf(data = sam1, fill = NA) +
    coord_sf(xlim = c(1,11), ylim = c(1,11)) +
    theme_bw()
}

log_within = st_within(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sf_map2[log_within,]) +
  geom_sf(data = sam1, fill = NA) +
  coord_sf(xlim = c(1,11), ylim = c(1,11)) +
  theme_bw()


log_intersects = st_intersects(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_intersects,], fill = NA) 



log_touches = st_touches(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_touches,], fill = NA) 


log_overlaps = st_overlaps(map2, sam1) |> 
  lengths() > 0


ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_overlaps,], fill = NA) 


log_contains = st_contains(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_contains,], fill = NA) 

log_covers = st_covers(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_covers,], fill = NA) 


log_disjoint = st_disjoint(map2, sam1) |> 
  lengths() > 0

ggplot() +
  geom_sf(data = sam1) +
  geom_sf(data = sf_map2[log_disjoint,], fill = NA) 





ggplot() +
  geom_sf(data = sam2) +
  geom_sf(data = sf_map2[log_disjoint,], fill = "black", alpha = 0.5) +
  # geom_sf(data = sf_map2[log_intersects,], fill = "blue", alpha = 0.5) +
  geom_sf(data = sf_map2[log_overlaps,], fill = "blue", alpha = 0.5) +
  geom_sf(data = sf_map2[log_touches,], fill = "green", alpha = 0.5) +
  geom_sf(data = sf_map2[log_within,], fill = "red", alpha = 0.5) +
  theme_bw()


source('./geo_core.R')


sido = sido_with_dokdo(vkeep = 0.0003)
sido |> 
  mutate(row = row_number()) |> 
  mutate(yn = row == sample(n()))
ggplot(sido) +
  geom_sf() +
  theme_bw() +
  theme(legend.position = "none")



sido2 = sido |> 
  mutate(cc = CTPRVN_CD == 47, .before = geometry)


ggplot(sido |> 
         mutate(row = row_number()) |> 
         mutate(yn = row == sample(n()))) +
  geom_sf(aes(fill = yn), colour = "gray100") +
  scale_fill_manual(values = c('gray90', 'blue')) +
  theme_void() +
  theme(legend.position = "none")



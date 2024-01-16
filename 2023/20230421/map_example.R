matrix(rep(c(10,20,30,40), 6), ncol = 4, byrow = T) %>% 
  as.tibble() %>% 
  unite('z', V1:V4) %>% 
  mutate(z2 = str_split(z, '_')) %>% 
  mutate(z3 = map(z2, sample, 4))


matrix(rep(c(10,20,30,40), 6), ncol = 4, byrow = T) %>% 
  as.tibble() %>% 
  mutate(n = row_number()) %>% 
  group_by(n) %>% 
  nest(x = V1:V4)

matrix(rep(c(10,20,30,40), 6), ncol = 4, byrow = T) %>% 
  as.tibble()

matrix(rep(c(10,20,30,40), 6), ncol = 4, byrow = T) %>% 
  as.tibble() %>%
  mutate(cc = pmap(list(V1,V2,V3,V4), c))


seq(10, 40, 10)

matrix(rep(c(10,40), 6), ncol = 2, byrow = T) %>% 
  as.tibble %>% 
  mutate(a = map2(V1, V2, seq, 10)) %>% 
  mutate(z3 = map(a, sample, 4)) %>% 
  view

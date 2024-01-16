source('./core.R')

set.seed(129409238)
t1 = tibble(x = sample(LETTERS, 5),
            y = sample(0:9, 5)) |> 
  unite("data", x:y, sep = "_")
t1
t1 |> 
  separate(data, into=c('letter', 'number'), 
           sep = '_', remove = F)


set.seed(27840)
t2 = tibble(x = sample(LETTERS, 5),
            y = sample(0:9, 5)) |> 
  unite("data", x:y, sep = "")
t2
t2 |> 
  mutate(letter = str_sub(data, 1, 1)) |> 
  mutate(number = str_sub(data, 2, 2))

t2 |> 
  extract(data, into = c('letter', 'number'), 
          regex = '(.)(.)', remove = F)

t2 |>  
  extract(data, into = c('letter', 'number'), 
          regex = '(\\w)(\\w)', remove = F)


t2 |>  
  extract(data, into = c('letter', 'number'), 
          regex = '([[:alpha:]])([[:digit:]])', remove = F)

t2 %>% 
  extract(data, into = c('letter', 'number'), 
          regex = '([[:digit:]])([[:alpha:]])', remove = F)

t2


t1 = enframe(c(4,8,2,9), name = NULL)
t2 = filter(t1, value > 5)
t3 = summarise(t2, total = sum(value))

summarise(filter(enframe(c(4,8,2,9), name = NULL), 
                 value > 5), 
          total = sum(value))
print(t3)

enframe(c(4,8,2,9), name = NULL) %>% 
  filter(value > 5) %>% 
  summarise(total = sum(value))


enframe(c(4,8,2,9), name = NULL) |> 
  filter(value > 5) |> 
  summarise(total = sum(value))

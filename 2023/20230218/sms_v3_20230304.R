

sms_txt = read_lines('~/Documents/blog/202302_creditcard/master/202303/sms.txt') %>% 
  enframe()
sms_txt

sms_txt2 = sms_txt %>% 
  filter(str_length(value) != 0) %>% 
  mutate(n1 = str_count(value, '\\[Web발신\\]')) %>% 
  filter(!str_detect(value, '.+:$'), !str_detect(value, '전송이 정상적으로')) %>% 
  mutate(n2 = cumsum(n1)) %>% 
  # filter(n1 == 0) %>% 
  select (n2, value) %>% 
  group_by(n2) %>% 
  nest() %>%
  ungroup() %>% 
  mutate(data2 = map(data, pull)) %>% 
  mutate(data3 = map_chr(data2, paste0, collapse = '')) %>% 
  mutate(data4 = str_remove_all(data3, '\\s')) %>% 
  select(sms_id = data4)

sms_txt2

sms_txt3 = sms_txt2 %>% 
  mutate(카드사명 = str_extract(sms_id, "^.+승인") %>% str_sub(8,-3)) %>% 
  mutate(날짜1 = str_extract(sms_id, '\\d{2}/\\d{2}')) %>% 
  mutate(날짜2 = str_replace_all(날짜1, '/', '-')) %>% 
  mutate(날짜3 = str_c('2023-', 날짜2)) %>% 
  mutate(시각 = str_c(str_extract(sms_id, '\\d{2}:\\d{2}'), ':00')) %>% 
  # mutate(금액 = str_extract(sms_id, '\\d+,*,\\d+원')) %>% 
  mutate(금액 = str_extract(sms_id, '\\d+[\\d|,]+원')) %>% 
  mutate(금액숫자 = str_replace_all(금액, ',|원', '') %>% as.integer()) %>% 
  mutate(가맹점명 = str_extract(sms_id, '\\d{2}:\\d{2}.+$') %>% str_sub(6)) %>% 
  mutate(가맹점명 = str_remove(가맹점명, '누적\\d+[\\d|,]+원$')) %>% 
  mutate(last_tm = now()) %>% 
  select(sms_id, card_company_tx = 카드사명, approval_date_tx = 날짜3, approval_time_tx = 시각, 
         approval_amount_tx = 금액, approval_amount_amt = 금액숫자, shop_name_tx = 가맹점명, last_tm) %>% 
  distinct()
  
# str_extract_all(',100원', '\\d+[\\d|,]+원')
# sms_txt3 %>% view

# sms_txt3 %>% 
#   select(sms_id, approval_amount_tx, shop_name_tx) %>% 
#   mutate(shop_name_tx1 = str_remove(shop_name_tx, '누적\\d+[\\d|,]+원$')) %>% 
#   print(n = Inf)

dbDisconnect(con)
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'prod', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 password = '1111')


dbExecute(con, "delete from app03.card_sms_dtl ced where substring(approval_date_tx, 1,7) = '2023-03' ")
# dbRemoveTable(con, Id(schema = "app03", table = "card_sms_dtl"))
# dbRemoveTable(con, Id(schema = "app03", table = "card_sms_dtl_log"))

dbWriteTable(con, 
             Id(schema = "app03", table = "card_sms_dtl"), 
             value = sms_txt3, append = T)

dbWriteTable(con, 
             Id(schema = "app03", table = "card_sms_dtl_log"), 
             value = sms_txt3, append = T)

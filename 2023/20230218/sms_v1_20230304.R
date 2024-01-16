
sms_text = read_excel('~/Documents/blog/202302_creditcard/master/202303/creditcard_sms.xlsx') %>% 
  mutate(문자내용_한줄 = str_replace_all(문자내용, '[[:space:]]', '')) %>%
  mutate(카드사명 = str_extract(문자내용_한줄, "^.+승인") %>% str_sub(8,-3)) %>% 
  mutate(날짜1 = str_extract(문자내용_한줄, '\\d{2}/\\d{2}')) %>% 
  mutate(날짜2 = str_replace_all(날짜1, '/', '-')) %>% 
  mutate(날짜3 = str_c('2023-', 날짜2)) %>% 
  mutate(시각 = str_c(str_extract(문자내용_한줄, '\\d{2}:\\d{2}'), ':00')) %>% 
  mutate(금액 = str_extract(문자내용_한줄, '\\d+,*,\\d+원')) %>% 
  mutate(금액숫자 = str_replace_all(금액, ',|원', '') %>% as.integer()) %>% 
  mutate(가맹점명 = str_extract(문자내용_한줄, '\\d{2}:\\d{2}.+$') %>% str_sub(6)) %>% 
  mutate(last_tm = now()) %>% 
  select(sms_id = 문자내용_한줄, card_company_tx = 카드사명, approval_date_tx = 날짜3, approval_time_tx = 시각, 
         approval_amount_tx = 금액, approval_amount_amt = 금액숫자, shop_name_tx = 가맹점명, sms_tx = 문자내용, info_nm = 메모, last_tm) 

# append(1:5, 0:1, after = 3)
sms_text %>% view

dbDisconnect(con)
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'test', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 password = '1111')


dbExecute(con, "delete from app03.card_sms_dtl ced where substring(approval_date_tx, 1,7) = '2023-03' ")
# dbRemoveTable(con, Id(schema = "app03", table = "card_sms_dtl"))
# dbRemoveTable(con, Id(schema = "app03", table = "card_sms_dtl_log"))

dbWriteTable(con, 
             Id(schema = "app03", table = "card_sms_dtl"), 
             value = sms_text, append = T)

dbWriteTable(con, 
             Id(schema = "app03", table = "card_sms_dtl_log"), 
             value = sms_text, append = T)

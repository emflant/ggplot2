
source('./core.R')
# source('./2023/20230218/v3_20230218.R') # 23.2 월 파일.
source('./2023/20230218/v3_202303.R') # 23.3월 카드이용내역 엑셀 파일.
# source('./2023/20230218/sms_v3_20230304.R') # 23.3월 카드sms 전송내역 파일 읽어서 처리.


# 895d8432fd8549c6e1354036957bce5f38586181
lotte2

kb2 %>% print(n = Inf)

all_cards 
all_cards %>% colnames()
all_cards %>% 
  filter(cancel_tx == 'Y')

hyundai2
all_cards


con
# all_cards %>% 
#   write_excel_csv(file = '~/Documents/blog/202302_creditcard/20230218/all_card.xlsx')

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'prod', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 password = '1111')

# dbDisconnect(con)
# dbRemoveTable(conn = con, Id(schema = "app03", table = "card_excel_dtl"))

# dbExecute(con, "TRUNCATE TABLE app03.card_excel_dtl")

dbExecute(con, "delete from app03.card_excel_dtl ced where substring(approval_date_tx, 1,7) = '2023-03' ")

all_cards
# dbRemoveTable(conn = con, Id(schema = "app03", table = "card_excel_dtl_log"))
dbWriteTable(con, 
             Id(schema = "app03", table = "card_excel_dtl"), 
             value = all_cards, append = T)
dbWriteTable(con, 
             Id(schema = "app03", table = "card_excel_dtl_log"), 
             value = all_cards, append = T)

# dbListTables(con)
# dbListObjects(con, DBI::Id(schema = "app03"))
# dbListFields(con, Id(schema = "app03", table = 'slip_dtl'))
# dbReadTable(con, Id(schema = "app03", table = 'slip_dtl'))

timestamp()


now()

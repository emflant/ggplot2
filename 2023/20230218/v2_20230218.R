source('./core.R')


# cols = c('카드사명', '승인일자', '승인시각', '카드구분', 
#          '카드번호', '승인번호', '가맹점명', '승인금액',  
#          '할인금액', '취소여부', '이용구분')
##################################################
# 하나카드 엑셀파일 가공하기.
##################################################
hana1 = read_excel('~/Documents/blog/202302_creditcard/20230220/hana_20230220.xls',
           skip = 3) %>% 
  filter(!is.na(이용시간)) 

hana2 = hana1 %>% 
  mutate(카드사명 = '하나카드') %>% 
  mutate(승인일자 = str_replace_all(이용일, '\\.', '-')) %>%
  mutate(승인시각 = 이용시간) %>%
  mutate(카드구분 = str_extract(이용카드, '..')) %>%
  mutate(카드번호 = str_extract(이용카드, '\\d+')) %>%
  mutate(할인금액 = `매입할인\n금액`) %>% 
  mutate(취소여부 = ifelse(매입취소금액 > 0, 'Y', 'N')) %>% 
  select(카드사명, 승인일자, 승인시각, 카드구분, 
         카드번호, 승인번호, 가맹점명, 승인금액, 
         할인금액, 취소여부, 이용구분)

##################################################
# 현대카드 엑셀파일 가공하기.
##################################################
hyundai1 = read_excel('~/Documents/blog/202302_creditcard/20230220/hyundaicard_20230220.xlsx',
           col_type = c('date', 'date', 'text', 'text', 'text', 'numeric',
                                  'text', 'numeric', 'text', 'text', 'text'),
           skip = 2) %>% 
  filter(!is.na(승인시각))
  
hyundai2 = hyundai1 %>% 
  mutate(카드사명 = '현대카드') %>% 
  mutate(승인일자 = as.character(승인일)) %>% 
  mutate(승인시각 = format(승인시각, format = "%H:%M:%S")) %>% 
  mutate(카드구분 = 카드구분) %>% 
  mutate(카드번호 = 카드종류) %>% 
  mutate(승인번호 = 승인번호) %>% 
  mutate(가맹점명 = 가맹점명) %>% 
  mutate(승인금액 = 승인금액) %>% 
  mutate(할인금액 = NA) %>% 
  mutate(취소여부 = ifelse(취소일 == '-', 'N', 'Y')) %>% 
  mutate(이용구분 = 이용구분) %>% 
  select(카드사명, 승인일자, 승인시각, 카드구분, 
         카드번호, 승인번호, 가맹점명, 승인금액, 
         할인금액, 취소여부, 이용구분) 

##################################################
# 국민카드 엑셀파일 가공하기.
##################################################
kb1 = read_excel('~/Documents/blog/202302_creditcard/20230220/kb_20230220.xls',
           skip = 6) 

kb2 = kb1 %>% 
  mutate(카드사명 = '국민카드') %>% 
  mutate(승인일자 = 이용일) %>% 
  mutate(승인시각 = str_c(`이용\n시간`, ':00')) %>%
  mutate(카드구분 = NA) %>%
  mutate(카드번호 = 이용카드명) %>%
  mutate(승인번호 = 승인번호) %>%
  mutate(가맹점명 = 이용하신곳) %>%
  mutate(승인금액 = `국내이용금액\n(원)`) %>%
  mutate(할인금액 = 할인금액) %>%
  mutate(취소여부 = NA) %>%
  mutate(이용구분 = 결제방법) %>%
  select(카드사명, 승인일자, 승인시각,카드구분,
         카드번호, 승인번호, 가맹점명, 승인금액,
         할인금액, 취소여부, 이용구분)

##################################################
# 신한카드 엑셀파일 가공하기.
##################################################
shinhan1 = read_excel('~/Documents/blog/202302_creditcard/20230220/shinhan_20230220.xls')

shinhan2 = shinhan1 %>% 
  mutate(카드사명 = '신한카드') %>% 
  mutate(승인일자 = str_extract(이용일시, '\\d{4}/\\d{2}/\\d{2}') %>% 
           str_replace_all('/', '-')) %>% 
  mutate(승인시각 = str_extract(이용일시, '\\d{2}:\\d{2}') %>% str_c(':00')) %>%
  mutate(카드구분 = ifelse(본인구분 == '본', '본인', '가족')) %>%
  mutate(카드번호 = 이용카드) %>%
  mutate(승인번호 = 승인번호) %>%
  mutate(가맹점명 = 가맹점명) %>%
  mutate(승인금액 = 이용금액) %>%
  mutate(할인금액 = NA) %>%
  mutate(취소여부 = NA) %>%
  mutate(이용구분 = 이용구분) %>%
  select(카드사명, 승인일자, 승인시각, 카드구분,
         카드번호, 승인번호, 가맹점명, 승인금액,
         할인금액, 취소여부, 이용구분)  

##################################################
# 우리카드 엑셀파일 가공하기.
##################################################
woori1 = read_excel('~/Documents/blog/202302_creditcard/20230220/woori_20230220.xls',
           skip = 1)

woori2 = woori1 %>% 
  mutate(카드사명 = '우리카드') %>% 
  mutate(승인일자 = str_c(year(ymd(결제예정일) - months(1)), '-',
                      str_extract(이용일, '\\d{2}.\\d{2}') %>% str_replace_all('\\.', '-'))) %>%
  mutate(승인시각 = str_extract(이용일, '\\d{2}:\\d{2}:\\d{2}')) %>%
  mutate(카드구분 = NA) %>%
  mutate(카드번호 = 이용카드) %>%
  mutate(승인번호 = 승인번호) %>%
  mutate(가맹점명 = `이용가맹점(은행)명`) %>%
  mutate(승인금액 = as.numeric(str_replace_all(`이용금액(원)`, ',', ''))) %>%
  mutate(할인금액 = NA) %>%
  mutate(취소여부 = ifelse(`취소금액(원)` > 0, "Y", "N")) %>%
  mutate(이용구분 = 이용구분) %>%
  select(카드사명, 승인일자, 승인시각, 카드번호,
         승인번호, 가맹점명, 승인금액, 할인금액,
         취소여부, 이용구분)

  

##################################################
# 최종 엑셀파일 만들기.
##################################################

all_cards = hana2 %>% union_all(hyundai2) %>% 
  union_all(kb2) %>% 
  union_all(shinhan2) %>% 
  union_all(woori2) %>% 
  mutate(승인금액 = as.integer(승인금액),
         할인금액 = as.integer(할인금액)) %>% 
  mutate(card_id_tx = str_c(카드사명, '|', 카드번호),
         approval_id_tx = str_c(승인일자, '-', 승인번호))

# write_excel_csv(all_cards, file = '~/Documents/blog/202302_creditcard/20230220/cards.xlsx')
rm_list = ls() %>% enframe() %>% 
  filter(str_detect(value, '^kb\\d{1}$|^hana\\d{1}$|^hyundai\\d{1}$|^shinhan\\d{1}$|^woori\\d{1}$')) %>% 
  pull(value) 

# 변수를 너무 많이 만들어서 정리.
rm(list = rm_list)
rm(list = c('rm_list'))




snakecase::to_any_case(c("iDontKnow", "ALL_CAPS"))

snakecase::to_any_case(c("iDontKnow", "ALL_CAPS", "한글"), transliterations = "Latin-ASCII")

snakecase::to_any_case("한글", transliterations = "Latin-ASCII")
snakecase::to_any_case("한글", transliterations = "Any-Latin")

stringi::stri_trans_list()
stringi::stri_trans_general("한글", "latin")



stringi::stri_trans_general('stringi', 'latin-greek')

tr
stringi::stri_trans_general("한글", 'Latin-ASCII')

stringi::stri_trans_general("한글", 'Hangul-Latin')
stringi::stri_trans_general("한글", 'Any-Latin')


t = '%2Fgrid_border%2Fgrid_2023%2Fgrid_%EB%9D%BC%EB%8B%A4%2Fgrid_%EB%9D%BC%EB%8B%A4'
t |> 
  URLdecode() |> 
  str_replace_all('/', "_") |> 
  stringi::stri_trans_general( 'Any-Latin')

hangul_text = "안녕하세요"
latin_text = stringi::stri_trans_general(hangul_text, "Hangul-Latin")

annyeonghaseyo

hangul_text = "닭고기"
(latin_text = stringi::stri_trans_general(hangul_text, "Hangul-Latin"))
(hangul_text2 = stringi::stri_trans_general(latin_text, "Latin-Hangul"))


hangul_text1 = "계란"
hangul_text1 = "삵"
hangul_text1 = "오늘 날씨가 아주 맑았다. 그리고 아침에 일어나 간단하게 씨리얼과 사과 반쪽을 먹었다."
(latin_text = stringi::stri_trans_general(hangul_text1, "Hangul-Latin"))
(hangul_text2 = stringi::stri_trans_general(latin_text, "Latin-Hangul"))


tibble(str = '한글', lang = stringi::stri_trans_list())
tibble(str = '한글', lang = stringi::stri_trans_list()) |> 
  mutate(trans1 = map2_chr(str, lang, stringi::stri_trans_general)) |> 
  filter(str_detect(trans1, "han")) |> 
  print(n = Inf)





# team <- "Chicago Bulls"
# url <- nba_endpoints(team)
# resp <- httr::GET(url = url)
# resp_cont <- httr::content(x = resp, type = "text/html", encoding = "UTF-8")
# tbs <- xml2::xml_find_all(resp_cont, "//td")
# lis <- xml2::xml_find_all(resp_cont, "//li")
# num_rows <- floor(length(tbs)/15)/2
# print(tbs[[num_rows*15-13]])
#
#

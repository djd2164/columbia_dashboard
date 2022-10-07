library(httr)
library(jsonlite)
library(dplyr)
library(stringr)


link = "https://compstat.nypdonline.org/api/reports/13/datasource/list"
headers = c(
  'accept' = "application/json, text/plain, */*",
  'accept-language' = "en-US,en;q=0.9",
  'content-type' = "application/json;charset=UTF-8",
  'sec-ch-ua' = "\"Google Chrome\";v=\"95\", \"Chromium\";v=\"95\", \";Not A Brand\";v=\"99\"",
  'sec-ch-ua-mobile' = "?0",
  'sec-ch-ua-platform' = "\"Windows\"",
  'sec-fetch-dest' = "empty",
  'sec-fetch-mode' = "cors",
  'sec-fetch-site' = "same-origin"
)

data = "{\"filters\":[{\"key\":\"PRECINCTKey\",\"label\":\"Precinct\",\"values\":[\"Citywide\"]},\
    {\"key\":\"BOROKey\",\"label\":\"Patrol Borough\",\"values\":[\"Citywide\"]},{\"key\":\"RECORDID\",\"label\":\"SELECTION\",\"values\":[\"28D_COMPLAINTS_TotalMajor7\"]}]}"


r <- httr::POST(url = link, 
                httr::add_headers(.headers=headers), body = data)

content_list <- content(r)



df <- data.frame(matrix(unlist(content_list), nrow=length(content_list), byrow=TRUE))
df <- data_frame(df)
df = df[c(1,3)]

df[c('Lat', 'Lon')] <- str_split_fixed(df$X1, ",", 2)
df <- df[-c(1)]

df$Category = str_extract_all(df$X3, ">(.*)</h5")

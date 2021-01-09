#CNN
#https://edition.cnn.com/search?q=coronavirus

library(rvest)
library(dplyr)

for (i in 1:68){
  url_page<-paste("https://edition.cnn.com/search?q=coronavirus&size=10&from=",(i-1)*10,"&page=",i,
                  sep = "")
  #tag 추출
  url_page %>% 
    html_nodes(".cnn-search__result-headline")
  
  html_news<-read_html(url_news)
  write(html_news, file = paste("cnn news",i)
}


url_page1<-"https://edition.cnn.com/search?q=coronavirus&size=10&from=0&page=1"
html_news<-read_html(url_page1)
html_news

#tag 추출
html_tag1<-html_news %>% 
  html_nodes("body > div.pg-search.pg-wrapper > div.pg-no-rail.pg-wrapper > div > div.l-container > div.cnn-search__right > div > div.cnn-search__results-list")
View(html_tag1)



url_news<-"https://edition.cnn.com/search?q=coronavirus&size=10&page=1"
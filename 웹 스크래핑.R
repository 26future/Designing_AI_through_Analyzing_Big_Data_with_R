# https://www.nytimes.com/section/world/asia

library(rvest)
library(dplyr)

url_news<-"https://www.nytimes.com/search?query=coronavirus"
html_news<-read_html(url_news)
html_news

#tag 추출
html_tag<-html_news %>% 
            html_nodes(".css-46b038 a")
html_tag[1]

#link 추출
links<-html_attr(html_tag,'href')
str(links)
links
links[1]
#link_data<-as.data.frame(links)
#link_data
#View(link_data)
#link_data[1,]


#url list
url_list<-c()
for (i in 1:12){
  url_list[i]<-paste("https://www.nytimes.com", links[i], sep = '')
}
url_list
url_list[1]

#텍스트 추출
article_text<-article %>% 
  html_nodes(".css-1fanzo5") %>% 
  html_text()
article_text[4]
str(article_text)

article<-read_html(url_list[1])
article_text<-article %>% 
  html_nodes(".css-1fanzo5") %>% 
  html_nodes(":not(script)") %>% 
  html_nodes(":not(div)") %>%
  html_nodes(":not(script)") %>% 
  html_text()
article_text[11:20]

article<-read_html(url_list[2])
article_text<-article %>% 
  html_nodes(".css-1fanzo5")
article


html_text(article_text)
article_text[[1]]
write.csv(article_text,file=paste("article",i,sep = ""))


article_text<-article %>% 
  html_nodes("css-exrw3m evys1bk0") %>% 
  html_text()
article_text[1:10]

article<-read_html(url_list[1])
article_text<-article %>% 
  html_nodes(".css-exrw3m.evys1bk0") %>% 
  #html_nodes(":not(#NYT_MID_MAIN_CONTENT_REGION)") %>% 
  #html_nodes(":not(script)") %>% 
  html_text()
article_text[21:30]
article_text

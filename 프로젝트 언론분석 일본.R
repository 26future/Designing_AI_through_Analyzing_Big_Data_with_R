#asahi   done saving
###done extracting articles
library(rvest)
library(dplyr)

url="http://www.asahi.com/ajw/search/results/?keywords=coronavirus&p=1"

asahi_page<-read_html(url, encoding="UTF-8")
asahi_page

#page number


#urls
asahi_urls<-asahi_page %>% 
  html_nodes("#Main .ListBlock li a") %>% 
  html_attr('href')
asahi_urls[2]

url_data<-data.frame(asahi_urls)
url_data
View(url_data)
url_data[1,1]

#title
article_title<-asahi_page %>% 
  html_nodes(".SearchResult_Headline em") %>% 
  html_text()
article_title

#text
article<-read_html(paste("http://www.asahi.com",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".ArticleText") %>% 
  html_text()
article_text[1]
str(article_text)
#####################################################################################
###saving articles with title
#25/26~42
#pages
for (i in 1:25){
  url=paste("http://www.asahi.com/ajw/search/results/?keywords=coronavirus&p=",i,sep = "")
  asahi_page<-read_html(url, encoding="UTF-8")
  
  #urls in page i
  asahi_urls<-asahi_page %>% 
    html_nodes("#Main .ListBlock li a") %>% 
    html_attr('href')
  
  url_data<-data.frame(asahi_urls)
  
  
  #titles
  article_title<-asahi_page %>% 
    html_nodes(".SearchResult_Headline em") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("http://www.asahi.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".ArticleText") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}




####################################################################################
#####################################################################################
#mainichi
###done extracting articles

url1="https://mainichi.jp/english/search?q=coronavirus&t=kiji&s=date&p=1"

mainichi_page<-read_html(url1, encoding="UTF-8")
mainichi_page

#page numbers


#urls
mainichi_urls<-mainichi_page %>% 
  html_nodes(".list-typeD li a") %>% 
  html_attr('href')
mainichi_urls

url_data<-data.frame(mainichi_urls)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-mainichi_page %>% 
  html_nodes(".midashi") %>% 
  html_text()
article_title

#text
article<-read_html(paste("http:",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".main-text .txt") %>% 
  html_text()

article_text
str(article_text)
#################################################################################
###saving articles with title
#36/37~58
#pages
for (i in 1:36){
  url1=paste("https://mainichi.jp/english/search?q=coronavirus&t=kiji&s=date&p=",i,sep = "")
  
  mainichi_page<-read_html(url1, encoding="UTF-8")
  
  #urls in page i
  mainichi_urls<-mainichi_page %>% 
    html_nodes(".list-typeD li a") %>% 
    html_attr('href')
  
  url_data<-data.frame(mainichi_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-mainichi_page %>% 
    html_nodes(".midashi") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("http:",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".main-text .txt") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}





#####################################################################################
#####################################################################################
#japan today
###done extracting articles

url2="https://japantoday.com/search?keyword=coronavirus&page=1"

jptoday_page<-read_html(url2, encoding="UTF-8")
jptoday_page

#page numbers


#urls
jptoday_urls<-jptoday_page %>% 
  html_nodes(".media .media-body h3 a") %>% 
  html_attr('href')
jptoday_urls

url_data<-data.frame(jptoday_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-jptoday_page %>% 
  html_nodes(".media .media-body h3 a") %>% 
  html_text()
article_title

#text
article<-read_html(paste("https://japantoday.com",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".text-large.mb-40 p") %>% 
  html_text()
article_text
  str(article_text)
######################################################################################
###saving articles with title
# ~40
#pages
for (i in 1:40){
  url2=paste("https://japantoday.com/search?keyword=coronavirus&page=",i,sep="")
  
  jptoday_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  jptoday_urls<-jptoday_page %>% 
    html_nodes(".media .media-body h3 a") %>% 
    html_attr('href')
  
  url_data<-data.frame(jptoday_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-jptoday_page %>% 
    html_nodes(".media .media-body h3 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://japantoday.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".text-large.mb-40 p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


#################################################################################
##################################################################################

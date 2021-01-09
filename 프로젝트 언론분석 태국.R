#thailand  done saving
#bangkokpost
###done extracting articles

library(rvest)
library(dplyr)

url="https://search.bangkokpost.com/search/result?start=0&q=coronavirus&category=news&refinementFilter=&sort=newest&rows=10"


bkpost_page<-read_html(url, encoding="UTF-8")
bkpost_page

#urls
bkpost_urls<-bkpost_page %>% 
  html_nodes(".detail h3 a") %>%
  html_attr('href') 
bkpost_urls

#!is.na(bkpost_urls)
#bkpost_urls_refine <-bkpost_urls[!is.na(bkpost_urls)]


url_data<-data.frame(bkpost_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-bkpost_page %>% 
  html_nodes(".detail h3 a") %>% 
  html_text()
article_title

#!is.na(bkpost_urls)
#article_title<-article_title[!is.na(bkpost_urls)]


#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".articl-content p") %>% 
  html_text()
article_text

article_text[2]
str(article_text)

#######################################################################################
###saving articles with title
#~50/51~92
#pages
for (i in 44:50){
  url=paste("https://search.bangkokpost.com/search/result?start=",10*(i-1),"&q=coronavirus&category=news&refinementFilter=&sort=newest&rows=10",sep="")
  bkpost_page<-read_html(url, encoding="UTF-8")
  
  #urls in page i
  bkpost_urls<-bkpost_page %>% 
    html_nodes(".detail h3 a") %>%
    html_attr('href') 
  
  url_data<-data.frame(bkpost_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-bkpost_page %>% 
    html_nodes(".detail h3 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".articl-content p") %>% 
      html_text()
    
    #title+article text
     #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}



#######################################################################################
#######################################################################################
#thethaiger
###done extracting articles

url1="https://thethaiger.com/page/1?s=coronavirus"

tg_page<-read_html(url1, encoding="UTF-8")
tg_page



#urls
tg_urls<-tg_page %>% 
  html_nodes(".mvp-main-blog-in li a") %>% 
  html_attr('href')
tg_urls


url_data<-data.frame(tg_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-tg_page %>% 
  html_nodes(".mvp-main-blog-in h2") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes("#mvp-content-main p") %>% 
  html_text()
article_text

str(article_text)
###############################################################################
###saving articles with title

#pages
for (i in 1:2){
  url1=paste("https://thethaiger.com/page/",i,"?s=coronavirus",sep = "")
  tg_page<-read_html(url1, encoding="UTF-8")
  

  #urls in page i
  tg_urls<-tg_page %>% 
    html_nodes(".mvp-main-blog-in li a") %>% 
    html_attr('href')
  
  url_data<-data.frame(tg_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-tg_page %>% 
    html_nodes(".mvp-main-blog-in h2") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes("#mvp-content-main p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


################################################################################
##################################################################################
#pattaya mail
###done extracting articles


url2="https://www.pattayamail.com/page/1?s=coronavirus"

ptmail_page<-read_html(url2, encoding="UTF-8")
ptmail_page


#urls
ptmail_urls<-ptmail_page %>% 
  html_nodes(".td-ss-main-content .item-details h3 a") %>% 
  html_attr('href')
ptmail_urls


url_data<-data.frame(ptmail_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-ptmail_page %>% 
  html_nodes(".td-ss-main-content .item-details h3 a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".td-post-content p") %>% 
  html_text()
article_text

str(article_text)
######################################################################################
###saving articles with title
# 12/13~18
#pages
for (i in 1:2){
  url2=paste("https://www.pattayamail.com/page/",i,"?s=coronavirus",sep = "")
  ptmail_page<-read_html(url2, encoding="UTF-8")
  
  
  #urls in page i
  ptmail_urls<-ptmail_page %>% 
    html_nodes(".td-ss-main-content .item-details h3 a") %>% 
    html_attr('href')
  
  url_data<-data.frame(ptmail_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-ptmail_page %>% 
    html_nodes(".td-ss-main-content .item-details h3 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".td-post-content p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}



######################################################################################
#######################################################################################
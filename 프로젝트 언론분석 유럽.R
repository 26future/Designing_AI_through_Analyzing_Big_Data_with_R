#euro news   done saving except #radio france international
###done extracting articles

library(rvest)
library(dplyr)

url="https://www.euronews.com/search?query=coronavirus&p=1"

euro_page<-read_html(url, encoding="UTF-8")
euro_page

#page numbers


#urls
euro_urls<-euro_page %>% 
  html_nodes("h3 a") %>% 
  html_attr('href')
euro_urls[1]


url_data<-data.frame(euro_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#title
article_title<-euro_page %>% 
  html_nodes("h3 a") %>% 
  html_text()
article_title



#text
article<-read_html(paste("https://www.euronews.com",url_data[1],sep = ""))
article_text<-article %>% 
  html_nodes("p") %>% 
  html_text()
article_text

str(article_text)
##############################################################################
###saving articles with title
# ~7/8~11

#pages
for (i in 1:7){
  url=paste("https://www.euronews.com/search?query=coronavirus&p=",i,sep="")
  euro_page<-read_html(url, encoding="UTF-8")
  
  #urls in page i
  euro_urls<-euro_page %>% 
    html_nodes("h3 a") %>% 
    html_attr('href')
  
  url_data<-data.frame(euro_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-euro_page %>% 
    html_nodes("h3 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://www.euronews.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes("p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}

##########################################################################
############################################################################

#Deutsche Welle
###done extracting articles
#100개/~18페이지

url1="https://www.dw.com/search/?languageCode=en&item=coronavirus&contentType=ARTICLE&searchNavigationId=9097&sort=DATE&resultsCounter=0"

dw_page<-read_html(url1, encoding="UTF-8")
dw_page

#page numbers


#urls
dw_urls<-dw_page %>% 
  html_nodes(".searchResult a") %>% 
  html_attr('href')
dw_urls


url_data<-data.frame(dw_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-dw_page %>% 
  html_nodes("h2") %>% 
  html_text()
article_title



#text
article<-read_html(paste("https://www.dw.com",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".longText p") %>% 
  html_text()
article_text
####################################################################################

###saving articles with title >>>>>>>>>>>>>>>>>>>>>>>>>>>>all at once
###done extracting articles
#100개 10/11~18페이지

#pages
for (i in 18:18){
  url1=paste("https://www.dw.com/search/?languageCode=en&item=coronavirus&contentType=ARTICLE&searchNavigationId=9097&sort=DATE&resultsCounter=",10*i,sep="")

  dw_page<-read_html(url1, encoding="UTF-8")
  
  #urls in page i
  dw_urls<-dw_page %>% 
    html_nodes(".searchResult a") %>% 
    html_attr('href')
  
  url_data<-data.frame(dw_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-dw_page %>% 
    html_nodes("h2") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://www.dw.com",url_data[1,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".longText p") %>% 
      html_text()
    
    #title+article text
    tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(tot_article,paste("article",i,"-",j,sep = ""))
  }
  
}




#########################################################################
##########################################################################
#the sun
###done extracting articles

url2="https://www.thesun.co.uk/page/1/?s=coronavirus"

sun_page<-read_html(url2, encoding="UTF-8")
sun_page

#page numbers


#urls
sun_urls<-sun_page %>% 
  html_nodes(".sun-row.teaser .teaser__copy-container a") %>% 
  html_attr('href')
sun_urls


url_data<-data.frame(sun_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[2,1]

#titles
article_title<-sun_page %>% 
  html_nodes(".sun-row.teaser .teaser__copy-container a") %>% 
  html_text()
article_title


#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes("p") %>% 
  html_text()
article_text
#############################################################################

###saving articles without title
#51/52~88
#pages
for (i in 1:51){
  url2=paste("https://www.thesun.co.uk/page/",i,"/?s=coronavirus",sep="")
  
  sun_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  sun_urls<-sun_page %>% 
    html_nodes(".sun-row.teaser .teaser__copy-container a") %>% 
    html_attr('href')
  
  url_data<-data.frame(sun_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-sun_page %>% 
    html_nodes(".sun-row.teaser .teaser__copy-container a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes("p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}




##########################################################################
##########################################################################

#France 24
###done extracting articles

#from page 2
url3="https://www.france24.com/en/tag/coronavirus/2/#pager"

france_page<-read_html(url3, encoding="UTF-8")
france_page

#page numbers


#urls
france_urls<-france_page %>% 
  html_nodes(".m-item-list-article a") %>% 
  html_attr('href')
france_urls


url_data<-data.frame(france_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-france_page %>% 
  html_nodes(".m-item-list-article a .article__infos p") %>% 
  html_text()
article_title[5]


#text
article<-read_html(paste("https://www.france24.com",url_data[5,1],sep = ""))
article_text<-article %>% 
  html_nodes("article") %>% 
  html_nodes(":not(.article__infos)") %>%
  html_nodes("p") %>% 
  html_text()
article_text
######################################################################

###saving articles with title 
#~5/6~9
#pages
for (i in 2:5){
  url3=paste("https://www.france24.com/en/tag/coronavirus/",i,"/#pager",sep = "")
  
  france_page<-read_html(url3, encoding="UTF-8")
  
  #urls in page i
  france_urls<-france_page %>% 
    html_nodes(".m-item-list-article a") %>% 
    html_attr('href')
  
  url_data<-data.frame(france_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-france_page %>% 
    html_nodes(".m-item-list-article a .article__infos p") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://www.france24.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes("article") %>% 
      html_nodes(":not(.article__infos)") %>%
      html_nodes("p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}




###########################################################################
###########################################################################
#radio france international ###############################

#from page2
url4="http://www.rfi.fr/en/tag/coronavirus/2/#pager"

rfi_page<-read_html(url4, encoding="UTF-8")
rfi_page

#page numbers


#urls
rfi_urls<-rfi_page %>% 
  html_nodes(".m-item-list-article a") %>% 
  html_attr('href')
rfi_urls


url_data<-data.frame(rfi_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-rfi_page %>% 
  html_nodes(".m-item-list-article a .article__infos p") %>% 
  html_text()
article_title[1]


#text
article<-read_html(paste("http://www.rfi.fr",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".t-content__body.u-clearfix p") %>% 
  html_text()
article_text
####################################################################################
###saving articles with title 
#~2/3~4
#pages
for (i in 2:2){
  url4=paste("http://www.rfi.fr/en/tag/coronavirus/",i,"/#pager",sep = "")
  
  rfi_page<-read_html(url4, encoding="UTF-8")
  
  #urls in page i
  rfi_urls<-rfi_page %>% 
    html_nodes(".m-item-list-article a") %>% 
    html_attr('href')
  
  url_data<-data.frame(rfi_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-rfi_page %>% 
    html_nodes(".m-item-list-article a .article__infos p") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("http://www.rfi.fr",url_data[1,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".t-content__body.u-clearfix p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}




###############################################################################
##################################################################################
#the week
###


url5="https://www.theweek.co.uk/search/site/coronavirus/page/0/0"

week_page<-read_html(url5, encoding="UTF-8")
week_page

#page numbers


#urls
week_urls<-week_page %>% 
  html_nodes("li div h2 a") %>% 
  html_attr('href')
week_urls


url_data<-data.frame(week_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-week_page %>% 
  html_nodes("li div h2 a") %>% 
  html_text()
article_title[1]


#text
article<-read_html(paste("https:",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes("p") %>% 
  html_text()
article_text
########################################################################################
###saving articles with title 

#pages
for (i in 2:3){
  url4=paste("http://www.rfi.fr/fr/tag/coronavirus/",i,"/#pager",sep = "")
  
  rfi_page<-read_html(url4, encoding="UTF-8")
  
  #urls in page i
  rfi_urls<-rfi_page %>% 
    html_nodes(".m-item-list-article a") %>% 
    html_attr('href')
  
  url_data<-data.frame(rfi_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-rfi_page %>% 
    html_nodes(".m-item-list-article a .article__infos p") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://www.france24.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes("article") %>% 
      html_nodes(":not(.article__infos)") %>%
      html_nodes("p") %>% 
      html_text()
    
    #title+article text
    tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(tot_article,paste("article",i,"-",j,sep = ""))
  }
  
}




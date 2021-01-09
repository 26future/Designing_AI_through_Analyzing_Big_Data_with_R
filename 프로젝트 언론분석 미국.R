#USA
#usa today

###done extracting articles

url="https://www.usatoday.com/search/?q=coronavirus&page=1"
install.packages("rvest")
library(rvest)
install.packages("dplyr")

library(dplyr)

ustoday_page<-read_html(url, encoding="UTF-8")
ustoday_page


#urls
ustoday_urls<-ustoday_page %>% 
                html_nodes(".gnt_se_a") %>% 
                html_attr('href')
ustoday_urls

library(stringr)
non_videos<-str_starts(ustoday_urls,"/videos", negate = TRUE)
ustoday_urls<-ustoday_urls[non_videos]


url_data<-data.frame(ustoday_urls)
url_data
View(url_data)
url_data[5,1]


#titles
article_title<-ustoday_page %>% 
  html_nodes(".gnt_se_hl") %>% 
  html_text()
article_title<-article_title[non_videos]
article_title[1]
length(article_title)
str(article_title)

#text
article<-read_html(paste("https://www.usatoday.com",url_data[1,1],sep = ""))
article_text<-article %>% 
                 html_nodes(".gnt_ar_b_p") %>% 
                 html_text()
article_text
str(article_text)

#title+article text
tot_article<-paste(article_title,article_text,sep=" ")

##############################################################################
###saving articles with title

#pages
for (i in 1:5){
  url=paste("https://www.usatoday.com/search/?q=coronavirus&page=",i, sep = "")
  ustoday_page<-read_html(url, encoding = "UTF-8")
  
  #urls in page i
  ustoday_urls<-ustoday_page %>% 
    html_nodes(".gnt_se_a") %>% 
    html_attr('href')
  
  non_videos<-str_starts(ustoday_urls,"/videos", negate = TRUE)
  ustoday_urls<-ustoday_urls[non_videos]
  
  url_data<-data.frame(ustoday_urls, stringsAsFactors = FALSE)
  
  #titles
  article_title<-ustoday_page %>% 
    html_nodes(".gnt_se_hl") %>% 
    html_text()
  article_title<-article_title[non_videos]

  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://www.usatoday.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".gnt_ar_b_p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


###############################################################################
#Corpus
my.text.location <- '/Users/thkim6258/Documents/MultiCampus/Data Analysis/Source Code/R/article'
corpus_USA <- VCorpus(DirSource(my.text.location))

##############################################################################
#preprocessing

##################################################################################33
##################################################################################
#daily news

url2="https://www.nydailynews.com/search/coronavirus/100-y/ALL/date/1/"

daily_page<-read_html(url2, encoding="UTF-8")
daily_page


#urls
daily_urls<-daily_page %>% 
  html_nodes(".r-mb.h7 .no-u") %>% 
  html_attr('href')
daily_urls

url_data<-data.frame(daily_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-daily_page %>% 
  html_nodes(".r-mb.h7 .no-u") %>% 
  html_text()
article_title[1]

#text
article<-read_html(paste("https://www.nydailynews.com",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes(".clln.clln-crd.overflow-auto.cs__ctn__border-bottom p") %>% 
  html_text()
article_text
#########################################################################################
###saving articles with title
#5/6~10
#pages
for (i in 1:5){
    url2=paste("https://www.nydailynews.com/search/coronavirus/100-y/ALL/date/",i,"/",sep="")
    daily_page<-read_html(url2, encoding="UTF-8")
    
    #urls in page i
    daily_urls<-daily_page %>% 
      html_nodes(".r-mb.h7 .no-u") %>% 
      html_attr('href')
    
    url_data<-data.frame(daily_urls, stringsAsFactors = FALSE)
    
    
    #titles
    article_title<-daily_page %>% 
      html_nodes(".r-mb.h7 .no-u") %>% 
      html_text()
    
    #article text
    for (j in 1:length(article_title)){
      article<-read_html(paste("https://www.nydailynews.com",url_data[j,1],sep = ""))
      article_text<-article %>% 
        html_nodes(".clln.clln-crd.overflow-auto.cs__ctn__border-bottom p") %>% 
        html_text()
      
      #title+article text
      #tot_article<-paste(article_title[j],article_text,sep=" ")
      
      #save as csv
      write.csv(article_text,paste("article",i,"-",j,sep = ""))
    }
    
}







###################################################################################
##################################################################################
#am new york
library(rvest)
library(dplyr)


url2="https://www.amny.com/page/1/?s=coronavirus"

amny_page<-read_html(url2, encoding="UTF-8")
amny_page


#urls
amny_urls<-amny_page %>% 
  html_nodes(".row.gridlove-posts article .box-inner-p .box-inner-ellipsis .entry-title.h3 a") %>% 
  html_attr('href')
amny_urls

url_data<-data.frame(amny_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-time_page %>% 
  html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".padded p") %>% 
  html_text()
article_text
#########################################################################################
###saving articles with title
#1/2~3
#pages
for (i in 1:2){
  url2=paste("https://time.com/search/?q=coronavirus&page=",i,sep="")
  time_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  time_urls<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_attr('href')
  
  url_data<-data.frame(time_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".padded p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


############################################################
###########################################################
#huffington post
###extracting urls

url1="https://search.huffpost.com/search;_ylt=Awr9H6qTF15eiEcAsUpsBmVH;_ylu=X3oDMTEza3NiY3RnBGNvbG8DZ3ExBHBvcwMxBHZ0aWQDBHNlYwNwYWdpbmF0aW9u?p=coronavirus&fr=huffpost&b=11&pz=10&bct=0&xargs=0"

hpost_page<-read_html(url1, encoding="UTF-8")
hpost_page


#urls
hpost_urls<-hpost_page %>% 
   html_nodes(".compArticleList li .thmb") %>% 
   html_attr('href')
hpost_urls

url_data<-data.frame(hpost_urls, stringsAsFactors = FALSE)
url_data

View(url_data)

#title
article_title<-hpost_page %>% 
  html_nodes("#results #cols #left #main ol li .compArticleList li .thmb") %>%
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes("div p") %>% 
  html_text()
article_text
####################################################################################
###

#pages
for (i in 1:2){
  url1=paste("https://search.huffpost.com/search;_ylt=Awr9BN47h1NeecAAezBsBmVH;_ylu=X3oDMTEza3NiY3RnBGNvbG8DZ3ExBHBvcwMxBHZ0aWQDBHNlYwNwYWdpbmF0aW9u?p=coronavirus&pz=10&fr=huffpost&bct=0&b=",10*(i-1)+1,"&pz=10&bct=0&xargs=0",sep="")
  hpost_page<-read_html(url1, encoding="UTF-8")
  
  #urls in page i
  hpost_urls<-hpost_page %>% 
    html_nodes(".thmb") %>% 
    html_attr('href')

  url_data<-data.frame(hpost_urls, stringsAsFactors = FALSE)
  
  #titles
  article_title<-hpost_page %>% 
    html_nodes(".pb-10 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes("div p") %>% 
      html_text()
    
    #title+article text
    tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(tot_article,paste("article",i,"-",j,sep = ""))
  }
  
}

##########################################################
###########################################################
#TIME
###done extracting articles


url2="https://time.com/search/?q=coronavirus&page=15"

time_page<-read_html(url2, encoding="UTF-8")
time_page


#urls
time_urls<-time_page %>% 
  html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
  html_attr('href')
time_urls

url_data<-data.frame(time_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-time_page %>% 
  html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".padded p") %>% 
  html_text()
article_text
#########################################################################################
###saving articles with title

#pages
for (i in 1:11){
  url2=paste("https://time.com/search/?q=coronavirus&page=",i,sep="")
  time_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  time_urls<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_attr('href')
  
  url_data<-data.frame(time_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".padded p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}




#######################################################################################
#########################################################################################

url2="https://abcnews.go.com/search?searchtext=coronavirus&sort=date&page=1"

amny_page<-read_html(url2, encoding="UTF-8")
amny_page


#urls
amny_urls<-amny_page %>% 
  html_nodes(".ContentRoll .ContentRoll__Item a") %>% 
  html_attr('href')
amny_urls

url_data<-data.frame(amny_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-time_page %>% 
  html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".padded p") %>% 
  html_text()
article_text
#########################################################################################
###saving articles with title
#1/2~3
#pages

url2="https://abcnews.go.com/search?searchtext=coronavirus&sort=date&page=1"
for (i in 1:2){
  url2=paste("https://time.com/search/?q=coronavirus&page=",i,sep="")
  time_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  time_urls<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_attr('href')
  
  url_data<-data.frame(time_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-time_page %>% 
    html_nodes(".partial.tile.media.image-top.margin-16-right.search-result .media-body a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".padded p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


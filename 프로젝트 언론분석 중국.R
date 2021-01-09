#china   done saving except "guangming"
#CCTV
###텍스트 추출까지 완료

library(rvest)
library(dplyr)

url="http://so.cntv.cn/language/english/index.php?qtext=coronavirus&type=1&sort=date&page=1&vtime=-1&datepid=1&history=yes"

CCTV_page<-read_html(url, encoding="UTF-8")
CCTV_page

#page numbers


#urls
CCTV_urls<-CCTV_page %>% 
  html_nodes(".txtcyc li a") %>% 
  html_attr('href')
CCTV_urls


url_data<-data.frame(CCTV_urls, stringsAsFactors = FALSE)
str(url_data)
View(url_data)
url_data[1,1]


#titles
article_title<-CCTV_page %>% 
  html_nodes("ul li h1 a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".text p") %>% 
  html_text()
article_text


str(article_text)
#######################################################################################
###saving articles with title
#~8/9~14
#pages
for (i in 9:14){
  url=paste("http://so.cntv.cn/language/english/index.php?qtext=coronavirus&type=1&sort=date&page=",i,"&vtime=-1&datepid=1&history=yes",sep="")
  CCTV_page<-read_html(url, encoding="UTF-8")
  
  #urls in page i
  CCTV_urls<-CCTV_page %>% 
    html_nodes(".txtcyc li a") %>% 
    html_attr('href') 
  
  url_data<-data.frame(CCTV_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-CCTV_page %>% 
    html_nodes("ul li h1 a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".text p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}



##########################################################################
##########################################################################
#guangming
###텍스트 추출까지 완료  ##saving: search X

#2페이지부터 추출
url1="https://en.gmw.cn/node_32067_2.htm"

gm_page<-read_html(url1, encoding="UTF-8")
gm_page

#page numbers


#urls
gm_urls<-gm_page %>% 
  html_nodes(".channel-newsGroup a") %>% 
  html_attr('href')
gm_urls

url_data<-data.frame(gm_urls, stringsAsFactors = FALSE)
str(url_data)
View(url_data)
url_data[1,1]
dim(url_data)


#titles
article_title<-gm_page %>% 
  html_nodes(".channel-newsGroup a") %>% 
  html_text()
article_title

#text
article<-read_html(paste("https://en.gmw.cn/",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes("#ArticleContent p") %>% 
  html_text()
article_text
#####################################################################################
###saving articles with title

#pages
for (i in 2:3){
  url1=paste("https://en.gmw.cn/node_32067_",i,".htm",sep="")
  gm_page<-read_html(url1, encoding="UTF-8")
  
  #urls in page i
  gm_urls<-gm_page %>% 
    html_nodes(".channel-newsGroup a") %>% 
    html_attr('href')
  
  url_data<-data.frame(gm_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-gm_page %>% 
    html_nodes(".channel-newsGroup a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("https://en.gmw.cn/",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes("#ArticleContent p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}





####################################################################################
####################################################################################
#beijing review 
###done extracting articles

url2="http://was.cipg.org.cn/was5/web/search?page=1&channelid=209889&searchword=coronavirus&keyword=coronavirus&perpage=10&outlinepage=6"

beijing_page<-read_html(url2, encoding="UTF-8")
beijing_page

#page numbers


#urls
beijing_urls<-beijing_page %>% 
  html_nodes("ul li tr a") %>% 
  html_attr('href')
beijing_urls

url_data<-data.frame(beijing_urls, stringsAsFactors = FALSE)
str(url_data)
View(url_data)
url_data[1,1]
dim(url_data)


#titles
article_title<-beijing_page %>% 
  html_nodes("ul li tr a") %>% 
  html_text()
article_title

#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes(".TRS_Editor p") %>% 
  html_text()
article_text
################################################################################
###saving articles with title
#14/15~21
#pages
for (i in 14:14){
  url2=paste("http://was.cipg.org.cn/was5/web/search?page=",i,"&channelid=209889&searchword=coronavirus&keyword=coronavirus&perpage=10&outlinepage=6",sep = "")
  beijing_page<-read_html(url2, encoding="UTF-8")
  
  #urls in page i
  beijing_urls<-beijing_page %>% 
    html_nodes("ul li tr a") %>% 
    html_attr('href')
  
  url_data<-data.frame(beijing_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-beijing_page %>% 
    html_nodes("ul li tr a") %>% 
    html_text()

  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".TRS_Editor p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}






###################################################################################
#####################################################################################
#china today
###done extracting articles


url3="http://was.cipg.org.cn/was5/web/search?page=1&channelid=262357&searchword=coronavirus&keyword=coronavirus&perpage=10&outlinepage=6"

ctoday_page<-read_html(url3, encoding="UTF-8")
ctoday_page

#page numbers


#urls
ctoday_urls<-ctoday_page %>% 
  html_nodes(".CT_centerBox ul li a") %>% 
  html_attr('href')
ctoday_urls

url_data<-data.frame(ctoday_urls, stringsAsFactors = FALSE)
str(url_data)
View(url_data)
url_data[5,1]
dim(url_data)


#titles
article_title<-ctoday_page %>% 
  html_nodes(".CT_centerBox ul li a") %>% 
  html_text()
article_title[5]

#text
article<-read_html(url_data[5,1])
article_text<-article %>% 
  html_nodes(".TRS_Editor p") %>% 
  html_text()
article_text
############################################################################
###saving articles with title
#18/19~28
#pages
for (i in 10:18){
  url3=paste("http://was.cipg.org.cn/was5/web/search?page=",i,"&channelid=262357&searchword=coronavirus&keyword=coronavirus&perpage=10&outlinepage=6",sep = "")
  ctoday_page<-read_html(url3, encoding="UTF-8")
  
  #urls in page i
  ctoday_urls<-ctoday_page %>% 
    html_nodes(".CT_centerBox ul li a") %>% 
    html_attr('href')
  
  url_data<-data.frame(ctoday_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-ctoday_page %>% 
    html_nodes(".CT_centerBox ul li a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes(".TRS_Editor p") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}









######################################################################################
######################################################################################
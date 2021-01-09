#korea  2월10일 기준
#korea herald 
###done extracting articles

library(rvest)
library(dplyr)

url="http://www.koreaherald.com/search/index.php?q=coronavirus&sort=1&mode=list&np=1&mp=1"

herald_page<-read_html(url, encoding="UTF-8")
herald_page

#urls
herald_urls<-herald_page %>% 
  html_nodes(".main_sec_li li a") %>% 
  html_attr('href')
herald_urls


url_data<-data.frame(herald_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-herald_page %>% 
                   html_nodes("li .main_l_t1") %>% 
                   html_text()
article_title



#text
article<-read_html(paste("http://www.koreaherald.com",url_data[1,1],sep = ""))
article_text<-article %>% 
                  html_nodes(".view_con_t") %>% 
                  html_text()

article_text
str(article_text)

####################################################################################
###saving articles with title
##~32/33~50
#pages
for (i in 1:32){
  url=paste("http://www.koreaherald.com/search/index.php?q=coronavirus&sort=1&mode=list&np=",i,"&mp=1",sep="")
  herald_page<-read_html(url, encoding="UTF-8")
  
  #urls in page i
  herald_urls<-herald_page %>% 
    html_nodes(".main_sec_li li a") %>% 
    html_attr('href')
  
  url_data<-data.frame(herald_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-herald_page %>% 
    html_nodes("li .main_l_t1") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("http://www.koreaherald.com",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes(".view_con_t") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


#############################################################################
##########################################################################
#kbs ~30/31~
###done extracting articles

url1="http://world.kbs.co.kr/service/search.htm?page=1&lang=e&search_date=5&keyword=coronavirus&kubun=n"

kbs_page<-read_html(url1, encoding="UTF-8")
kbs_page

#urls
kbs_urls<-kbs_page %>% 
  html_nodes(".list_link_area a") %>% 
  html_attr('href')
kbs_urls

library(stringr)
kbs_urls<-str_replace_all(kbs_urls,"./","/")
kbs_urls

url_data<-data.frame(kbs_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-kbs_page %>% 
  html_nodes(".list_link_area a") %>% 
  html_text()
article_title


#text
article<-read_html(paste("http://world.kbs.co.kr/service",url_data[1,1],sep = ""))
article_text<-article %>% 
  html_nodes("#ele1 > div.body_txt.fr-view") %>% 
  html_text()
article_text

str(article_text)

####################################################################################
###saving articles with title
#~30/31~52
#pages
for (i in 31:52){
  url1=paste("http://world.kbs.co.kr/service/search.htm?page=",i,"&lang=e&search_date=5&keyword=coronavirus&kubun=n",sep="")
  kbs_page<-read_html(url1, encoding="UTF-8")
  
  #urls in page i
  kbs_urls<-kbs_page %>% 
    html_nodes(".list_link_area a") %>% 
    html_attr('href')
  
  kbs_urls<-str_replace_all(kbs_urls,"./","/")

  url_data<-data.frame(kbs_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-kbs_page %>% 
    html_nodes(".list_link_area a") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(paste("http://world.kbs.co.kr/service",url_data[j,1],sep = ""))
    article_text<-article %>% 
      html_nodes("#ele1 > div.body_txt.fr-view") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}



#############################################################################
############################################################################

#korea times
###done extracting articles

url2="https://www.koreatimes.co.kr/www2/common/search.asp?kwd=coronavirus&pageNum=1&pageSize=10&category=TOTAL&sort=&startDate=&endDate=&date=all&srchFd=&range=&author=all&authorData=&mysrchFd="

ktimes_page<-read_html(url2, encoding="UTF-8")
ktimes_page


#urls

ktimes_urls<-ktimes_page %>% 
  html_nodes("td tr td a") %>% 
  html_attr('href') 
ktimes_urls<-ktimes_urls[1:10]
ktimes_urls

url_data<-data.frame(ktimes_urls, stringsAsFactors = FALSE)
url_data
View(url_data)
url_data[1,1]

#titles
article_title<-ktimes_page %>% 
  html_nodes("tr td a u") %>% 
  html_text()
article_title



#text
article<-read_html(url_data[1,1])
article_text<-article %>% 
  html_nodes("#startts span") %>% 
  html_text()
article_text

article_text[2]
str(article_text)

###################################################################################
###saving articles with title
#~45/46~71
#pages
for (i in 1:45){
  url2=paste("https://www.koreatimes.co.kr/www2/common/search.asp?kwd=coronavirus&pageNum=",i,"&pageSize=10&category=TOTAL&sort=&startDate=&endDate=&date=all&srchFd=&range=&author=all&authorData=&mysrchFd=",sep = "")
  ktimes_page<-read_html(url2, encoding="UTF-8")  
  
  #urls in page i
  ktimes_urls<-ktimes_page %>% 
    html_nodes("td tr td a") %>% 
    html_attr('href') 
  ktimes_urls<-ktimes_urls[1:10]
  
  url_data<-data.frame(ktimes_urls, stringsAsFactors = FALSE)
  
  
  #titles
  article_title<-ktimes_page %>% 
    html_nodes("tr td a u") %>% 
    html_text()
  
  #article text
  for (j in 1:length(article_title)){
    article<-read_html(url_data[j,1])
    article_text<-article %>% 
      html_nodes("#startts span") %>% 
      html_text()
    
    #title+article text
    #tot_article<-paste(article_title[j],article_text,sep=" ")
    
    #save as csv
    write.csv(article_text,paste("article",i,"-",j,sep = ""))
  }
  
}


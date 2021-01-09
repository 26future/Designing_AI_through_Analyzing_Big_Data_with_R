library(rJava)
library(KoNLP)
library(httpuv)
install.packages("rgdal")
library(rgdal)
install.packages("geojsonio")
library(geojsonio)
install.packages("rgeos")
library(rgeos)
install.packages("Sejong")
install.packages("hash")
install.packages("tau")
install.packages("RSQLite")
library(Sejong)
library(hash)
library(tau)
library(RSQLite)
sentence <- '아버지가 방에 들어가신다'
extractNoun(sentence)

my.text.location<-"c:/gspark/ymbaek_논문"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper
mykorean<-mypaper[[19]]$content
#전처리
mytext<-str_replace_all(mykorean, "[[:lower:]]","")
mytext
mytext<-str_replace_all(mytext, "\\(","")
mytext<-str_replace_all(mytext, "\\)","")
mytext
mytext<-str_replace_all(mytext, "‘","")
mytext<-str_replace_all(mytext, "’","")
mytext
mytext<-str_replace_all(mytext, " · ",", ")
mytext
#명사 추출
noun.mytext<-extractNoun(mytext)
noun.mytext 
table(noun.mytext)

# 참여
# 참석   -> 참가
# 참가

#웹 스크래핑
#1. 포털, 신문사, 언론사,...
#TEXT -> DTM -> TFIDF 구성 -> 상관계수

#2.한글 논문 문서 
#DTM => TFIDF 구성
#TF는 높고, IDF가 낮은 단어 추출








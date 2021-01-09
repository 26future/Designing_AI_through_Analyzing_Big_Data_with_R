#json 파일을 읽기 위한 패키지
install.packages("jsonlite")
library(jsonlite)

#json파일 읽기
data<-fromJSON("sample.json")
data
str(data)
data<-data.frame(data)
data

#컬럼 이름 변경
names(data)<-c("id","like","share","comment","msg","time")
data

#dataframe -> json
dataJson<-toJSON(data)
dataJson
#파일로 저장
write(dataJson, "data.json")

#####################################################################
#엑셀 파일을 읽기 위한 패키지
install.packages("readxl")
library(readxl)
cust_profile<-read_excel("cust_profile.xlsx", sheet = "cust_profile", 
                          range = "B3:E8", 
                          col_names = TRUE, 
                          na="NA",
                          skip=2)
cust_profile

##########################################################################
#txt 파일 읽기
dataset_1<-read.table("dataset_1.txt",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE,
                      na.strings = "")
dataset_1

###########################################################################
#xml 데이터 읽기
install.packages("XML")
library(XML)
res<-xmlToDataFrame("test.xml")
res

#다른 방법
res2<-xmlParse(file="test.xml")
res2
rt<-xmlRoot(res2)
rt[[2]]
rt[[1]]
rt[[1]][[1]]

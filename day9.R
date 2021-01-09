install.packages("stringr")
library(stringr)
rwiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

str_extract(rwiki, "software environment") #"software environment" 추출
#regexpr(), regmatches() => base 함수(R에 기본적으로 설치됨)
str_extract_all(rwiki, "software environment") #전체 문장 참조
str_extract_all(rwiki, "software environment", simplify = TRUE) #matrix
#matrix: 수치 데이터, 타입이 모두 동일
#dataframe: 컬럼별로 데이터 타입이 다름

#정규식 사용
#첫 문자가 대문자로 시작하는 단어 추출
#대문자 1개, 알파벳 문자 0개 이상
myextract<-str_extract_all(rwiki, "[[:upper:]]{1}[[:alpha:]]{0,}") 
table(myextract)

str_locate(rwiki, "software environment") #위치 확인
str_locate_all(rwiki, "software environment")
#list: 모든 자료 구조를 포함할 수 있음

#첫번째 글자가 대문자로 시작되는 모든 단어들의 위치 출력
mylocate<-str_locate_all(rwiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
class(mylocate) #list
class(mylocate[[1]]) #matrix
dim(mylocate[[1]])

mydata<-data.frame(mylocate[[1]])
mydata
mydata$myword<-myextract[[1]]
mydata

#my data에 myword.length 열 추가
#myword.length에는 myword의 길이 저장
#length(mydata$myword) -> 23: myword에 들어있는데 데이터의 개수
mydata$myword.length<-mydata$end-mydata$start+1
head(mydata)

#두 단어->한 단어
str_replace(rwiki, "software environment","software_environment")
temp<-str_replace_all(rwiki, "software environment","software_environment")
str_extract_all(temp,"software_environment|software|environment")
table(str_extract_all(temp,"software_environment|software|environment"))

#R->R_computer.language_ 로 변경
#C->C_computer.language_로 변경
temp<-str_replace_all(rwiki,"R","R_computer.lnguage_")
temp<-str_replace_all(temp,"C","C_computer.language_")

#temp에서 _computer.language_ 표현이 붙은 부분에는 어떤 단어들이 있고
#빈도가 어떤지 출력
#c_computer.language_ R_computer.language_
#      1                  9
table(str_extract_all(temp, "[[:alnum:]]{1}_computer.language_"))

#텍스트 데이터의 문단을 구분(줄바꿈)
rwikipara<-str_split(rwiki,"\n")
rwikipara

#문단별로 문장을 구분(.)
rwikisent<-str_split(rwikipara[[1]],"\\. ")

#str_split_fixed 함수
class(rwikisent)
rwikisent[[1]] #첫문단의 문장들
unlist(rwikisent) #unlist:list->vector
my2sentences<-unlist(rwikisent)[c(4,7)] #4,7번 문장 추출
my2sentences

#각 문장의 단어수를 출력
mylength1<-length(unlist(str_split(my2sentences[1]," ")))
mylength2<-length(unlist(str_split(my2sentences[2]," ")))
mylength1;mylength2

#str_split_fixed함수
myfixed.short<-str_split_fixed(my2sentences," ",5) #5개의 단어로 분리
myfixed.long<-str_split_fixed(my2sentences," ",13)
myfixed.long

#rwikisent 문장*단어 행렬 구성
rep(3,5) #3을 5번 반복
length.sentences<-rep(NA,length(unlist(rwikisent)))

for (i in 1:length(length.sentences)){
  length.sentences[i]<-length(unlist(str_split(unlist(rwikisent)[i]," ")))
} #각 문장의 단어의 개수
length.sentences
max.length.sentences<-max(length.sentences)

sent.word.matrix<-str_split_fixed(unlist(rwikisent)," ",max.length.sentences)
sent.word.matrix  #모든 문장이 21개로 분리
mydata<-data.frame(sent.word.matrix)
mydata

#rownames(mydata) #sent.1 sent.2 ...sent.7
rownames(mydata)<-paste("abc",1:max.length.sentences,sep =".")
#colnames(mydata) #word.1 ...word.21
colnames(mydata)<-paste("abc",1:max.length.sentences,sep =".")

mydata[,1] #각 문장의 첫번재 단어
mydata[3,1:10]

#단어'R' 등장 횟수
rwiki
class(rwiki)
str_count(rwiki,"R")
#str_count(rwikipara,"R")
rwikipara[[1]] #리스트의 첫번재 요소: 길이가 2인 벡터
str_count(rwikipara[[1]],"R")
#str_count(rwikisent,"R")
str_count(rwikisent[[1]] ,"R") #첫번째 문단
str_count(rwikisent[[2]],"R") #두번째 문단
str_count(unlist(rwikisent), "R")

#rwikisent에서 R이라는 단어가 등장한 후에 
#stat으로 시작하는 단어가 등장하는 빈도
#1 1 0 0 0 0 0
#모든문자: .
str_count(unlist(rwikisent),"R.{1,}stat[[:lower:]]")

#s,S 구분이 필요 없는 경우
str_count(unlist(rwikisent),"R.{1,}(s|S)tat[[:lower:]]")
unlist(rwikisent)[1:2]

str_extract_all(unlist(rwikisent)[1],"R.{1,}(s|S)tat[[:lower:]]")
str_extract_all(unlist(rwikisent)[1],"R.{1,}(s|S)tat[[:lower:]]")

#R과 stat사이에 R이라는 표현이 있으면 안됨
str_count(unlist(rwikisent),"R[[:lower:][A-Q][s-z][:digit:][:space:]]{1,}(s|S)tat[[:alpha:]]{1,}")
str_count(unlist(rwikisent),"R{1}[^R]{1,}(s|S)tat[[:alpha:]]{1,}")

#substr(), str_sub()
str_sub(unlist(rwikisent[1],1,30))

str_dup("software",3) #3번 복제
rep("software",3)
paste(rep("software",3),collapse = "")
str_dup("software",3)==paste(rep("software",3),collapse = "")

str_length(unlist(rwikisent))
nchar(unlist(rwikisent))

name<-c("Joe","Jack","Jackie","Jefferson")
donation<-c("$1","$111","$11111","$1111111")
mydata<-data.frame(name,donation)
mydata

str_pad(mydata$name, width=15, side = "right") #왼쪽 정렬
#side=right 공백문자를 오른쪽으로 이동
str_pad(mydata$name, width=15, side = "left") #오른쪽 정렬
str_pad(mydata$name, width=15, side = "both") #가운데 정렬
str_pad(mydata$name, width=15, side = "both", pad = "~") #공백을 ~로 채움
donation2<-str_pad(mydata2$name, width=15, side = "both", pad = "~")
name2
donation2
mydata2<-data.frame(name2,donation2)
mydata2
str_length(mydata2$name2)
str_length(mydata$name)

#패딩된 공백문자를 제거
name3<-str_trim(mydata2$name2, side='right') #오른쪽 공백문자 제거
name3

#양쪽에 패딩(~)기호를 모두 제거
donation3<-str_trim(str_replace_all(mydata2$donation2,"~"," "),side="both")
mydata3<-data.frame(name3,donation3)
mydata
mydata3
all(mydata3==mydata)

##########################################################################
#오늘 나는 멀티캠퍼스가 있는 역삼동에 간다
#나는 오늘 역삼동에 있는 멀티캠퍼스에 간다
#2-gram: 두 문장의 유사도
#나는 오늘
#오늘 역삼동에
#...
#멀티캠퍼스에 간다

mytext<-c("software environment",
          "software  environment",
          "software\tenvironment")
mytext

#white space 제거
str_split(mytext," ")
#단어의 길이 구하기
sapply(str_split(mytext," "),length)
lapply(str_split(mytext," "),length)

#1개 이상의 공백문자->1개의 공백문자
mytext.nowhitespace<-str_replace_all(mytext,"[[:space:]]{1,}"," ") 
mytext.nowhitespace
#단어의 길이 구하기
sapply(str_split(mytext.nowhitespace," "),length)
lapply(str_split(mytext.nowhitespace," "),length)

mytext<-"The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"
myword<-unlist(str_extract_all(mytext,boundary("word")))
table(myword)
table(tolower(myword))

#고유명사
myword<-str_replace(myword,"Trump","Trump_unique_")
myword<-str_replace(myword,"States","States_unique_")
table(myword)

###########################################################################
mytext<-c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
mytext

#숫자 삭제
str_replace_all(mytext, "[[:digit:]]{1,}[[:space:]]{1,}","")
#공백으로 분리
mytext2<-str_split(str_replace_all(mytext, "[[:digit:]]{1,}[[:space:]]{1,}","")," ")
mytext2[[1]]
str_c(mytext2[[1]],collapse = " ")
str_c(mytext2[[2]],collapse = " ")

#나는 1995년에 태어났습니다. => 숫자 제거 X
#숫자 자료임을 표시
mytext3<-str_split(str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}",
                                   "_number_")," ")
mytext3

# . :문장 구분X, 생략 표시
mytext<-"Kim et al.(2020) argued that the state of"
#성 et al. (년도) => 하나의 단어로 교체
#                 => _reference_
str_split(mytext, "\\. ")

mytext<-c("She is an actor","She is the actor")
#a, an, the => stopwords
mystopword<-"(\\ban )|(\\the )"
str_replace_all(mytext,mystopword,"")

library(tm)
length(stopwords("en"))
length(stopwords("SMART"))

#어근동일화 프로그램
mytext<-c("I am a boy. You are a boy. He might be a boy.")
mystemmer.func <- function(mytextobj){
  #am, are, is, was, were, be => be
  mytext<-str_replace_all(mytext,"(\\bam )|(\\bare )|(\\bwas )|(\\bwere )|(\\bbe )","be ")
  mytext
}
mytext.stem<-mystemmer.func(mytext)
mytext.stem
#I be a boy. You be a boy...

########################################################################
"오늘 강남에서 맛있는 스파게티를 먹었다."
"강남에서 먹었던 오늘의 스파게티는 맛있었다."
#글자 n-gram 기반 유사도
#n=2
#오늘,늘 , 강, 강남...,다.
#강남,남에,...,다.

install.packages("tidytext")
library(tidytext)
install.packages("tidyr")
library(tidyr)

#감성 어휘 사전 -> 감성 분석
install.packages("textdata")

get_sentiments("bing") #긍정/부정 두가지로 분류
mynrc<-data.frame(get_sentiments("nrc"))
mynrc
table(mynrc$sentiment)



################################################################
"오늘 강남에서 맛있는 스파게티를 먹었다."
"강남에서 먹었던 오늘의 스파게티는 맛있었다."
#유사도 출력
#ngram이 작은 값이 분모
#감성사전 기반 감성 분석
#알고리즘:  레벤슈타인 거리
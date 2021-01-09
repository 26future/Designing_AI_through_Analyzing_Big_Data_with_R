#텍스트 분석, r함수
myvector<-c(1:6,'a')
myvector
mylist<-list(1:6,'a')
mylist

obj1<-1:4
obj1
obj2<-6:10
obj3<-list(obj1,obj2)
obj3

mylist<-list(obj1,obj2,obj3)
mylist
#벡터:[], 리스트:[[]]

#리스트에서 자료 추출시, [1]을 사용해서 리스트를 추출
#[[1]]을 사용하면 벡터를 추출할 수 있다
mylist[[3]][1]
mylist[[3]][[1]]

mylist
mylist[[3]][[1]][2]

#unlist:리스트를 벡터형식으로 리턴
mylist<-list(1:6,'a')
mylist
unlist(mylist)
myvector==unlist(mylist)

mean(mylist[[1]][1:6])
#mean(unlist(mylist)[1:6]) #문자로 변환되었기 때문에 에러

name1<-"Donald"
myspace<-" "
name2<-"Trump"
list(name1,myspace,name2)
unlist(list(name1,myspace,name2))
#unlist:하나의 문자 형태의 객체로 합치고자 할 때
name<-c("갑","을","병","정")
gender<-c(2,1,1,2)
mydata<-data.frame(name,gender)
mydata
#attr():속성값을 저장하거나 추출할때 사용
#메타데이터:데이터의 데이터
#ex)gender의 메타데이터: 성별을 의미함 (데이터에 대한 부가 설명)

attr(mydata$name,"what the variable means")<-"응답자 이름"
mydata$name
attr(mydata$gender,"what the variable means")<-"응답자 성별"
mydata$gender

myvalues<-gender
for (i in 1:length(gender)){
  myvalues[i]<-ifelse(gender[i]==1,"남성","여성")
}
myvalues

attr(mydata$gender,"what the value means")<-myvalues
mydata$gender

mydata$gender.character<-attr(mydata$gender,"what the value means")
mydata

#리스트->lapply
mylist<-list(1:4, 6:10, list(1:4, 6:10))
mylist
lapply(mylist[[3]],mean)

#tapply는 텍스트 데이터에 대해 사용
wordlist<-c("the","is","a","the")
df1<-c(3,4,2,4) #문서1에서 wordlist의 단어가 등장한 횟수
#the:3, is:4, a:2, the:4
df2<-rep(1,4) #1을 4개 출력 #문서2에서 wordlist의 단어가 등장한 횟수
df2
tapply(df1,wordlist,length)
tapply(df1,wordlist,sum) #가중치 계산
tapply(df2,wordlist,length)
tapply(df2,wordlist,sum)

#알파벳 출력 함수
letters[3]
LETTERS[3]
letters[1:26]
tolower("Eye")
toupper("Eye")

#nchar함수: 문자수를 세는 함수
nchar("Korea")
nchar("한국")
nchar("Korea",type="bytes")
nchar("한국",type="bytes")
nchar("Korea ")
nchar("Korea\t")
nchar("Korea\t",type="bytes")
nchar("Korea, Republic of")
nchar("Korea, 
      Republic of")
nchar("Korea, \nRepublic of")

#문장을 단어로 분리
mysentence<-"Learning R is so interesting"
strsplit(mysentence, split = " ")
#단어를 문자로 분해
mywords<-strsplit(mysentence, split = " ")
mywords

strsplit(mywords[[1]][5],split="")

#초기화 
myletters<-list(rep(NA,5))
myletters

for(i in 1:5){
  myletters[i]<-strsplit(mywords[[1]][i],split="")
}
myletters

#문자를 합쳐서 단어로 구성
#paste(1,2,3)
paste(myletters[[1]], collapse="#")
paste(myletters[[1]], collapse="")

mywords2<-list(rep(NA,5))
for(i in 1:5){
  mywords2[i]<-paste(myletters[[i]],collapse ="")
}
mywords2
paste(mywords2,collapse =" "

rwiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."      

#문단 단위로 구분
rwikipara<-strsplit(rwiki, split="\n")
rwikisent<-strsplit(rwikipara[[1]], split="\\. ")

rwikisent[[1]] #첫번째 문단
rwikisent[[2]] #두번째 문단

for (i in 1:2){
  rwikiword[[i]]=strsplit(rwikisent[[i]],split=" ")
}
rwikiword
rwikiword[[1]][[2]][3] #'language'출력

#regexpr함수: 정규표현식, 처음 등장하는 텍스트의 위치 출력
mysentence<-"Learning R is so interesting"
regexpr("ing",mysentence)

loc.begin<-as.vector(regexpr("ing", mysentence))
loc.length<-attr(regexpr("ing",mysentence),'match.length')          
loc.end<-loc.begin+loc.length-1                     

#grepexpr 은 패턴이 등장하는 모든 텍스트 위치 출력
gregexpr("ing",mysentence)
#발견된 패턴의 수
length(gregexpr("ing",mysentence)[[1]])

loc.begin<-as.vector(gregexpr("ing", mysentence)[[1]])
loc.begin
loc.length<-attr(gregexpr("ing",mysentence)[[1]],'match.length')          
loc.length
loc.end<-loc.begin+loc.length-1 
loc.end

regexpr("interesting", mysentence)

regexec("interestin(g)", mysentence)
regexec("so (interestin(g))", mysentence)
#so의 시작위치, interesting의 시작위치, g의 시작위치 출력

mysentences<-unlist(rwikisent)
regexpr("software", mysentences) #단어가 없는 경우: -1 출력
gregexpr("software", mysentences)

sub("ing","ING",mysentence) #소문자'ing'를 대문자'ING'로 변경
gsub("ing","ING",mysentence)

mytemp<-regexpr("software", mysentences)
my.begin<-as.vector(mytemp)
my.begin
my.begin[my.begin==-1]<-NA
my.begin

my.end<-my.begin + attr(mytemp, "match.length")-1
my.end

length(my.begin)
mylocs<-matrix(NA, nrow = length(my.begin), ncol=2)
mylocs

#열 이름 바꾸기
colnames(mylocs)<-c("begin","end")
mylocs

#행 이름 바꾸기
#paste("hi",1:3,sep=".")
#paste("hi","hello")
rownames(mylocs)<-paste("sentence",1:length(my.begin), sep=".")
mylocs

for (i in 1:length(my.begin)){
  mylocs[i,]<-cbind(my.begin[i],my.end[i])
}
mylocs

#grep,grepl: 특정 표현이 텍스트에 있는지 확인
mysentences
grep("software",mysentences) #1,2,5번 문장에서 표현이 발견
grepl("software",mysentences) #있으면 TRUE, 없으면 FALSE

#고유명사 처리
"Donald Trump" => "Donald_Trump"

sent1<-rwikisent[[1]][1]
new.sent1<-gsub("R Foundation for Statistical Computing", 
                "R_Foundation_for_Statistical_Computing",sent1)

#sent1단어 개수
#new.sent1 단어 개수
sum(table(strsplit(sent1, split=" ")))
sum(table(strsplit(new.sent1, split=" ")))

new.sent1

#단어 제거
drop.sent1<-gsub("and |by |for |the","",new.sent1) 
#첫번째 단어를 두번째 단어로 변경
sum(table(strsplit(drop.sent1, split=" ")))

mysentence
mypattern<-regexpr("ing",mysentence)
regmatches(mysentence, mypattern)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence, mypattern)

#invert옵션: 반대 표현
mypattern<-regexpr("ing",mysentence)
regmatches(mysentence, mypattern, invert = TRUE)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence, mypattern, invert=TRUE)
strsplit(mysentence, split="ing") # "" 제거
gsub("ing","",mysentence)

substr(mysentence,1,20)

substr(mysentences,1,20)

my2sentence<-c("Learning R is so interesting",
               "He is a fascinating singer")
#ing로 끝나는 모든 단어를 검출
mypattern0<-gregexpr("ing",my2sentence)
regmatches(my2sentence,mypattern0)

#ing앞에 알파벳 표현 확인=>[[:alpha:]]
mypattern1<-gregexpr("[[:alpha:]]+(ing)",my2sentence)
regmatches(my2sentence,mypattern1)

#[[:alpha:]]+ing => \\b
mypattern2<-gregexpr("[[:alpha:]]+(ing)\\b",my2sentence)
regmatches(my2sentence,mypattern2)

#7개 문장 모두에 대해 ing로 끝나는 영어 단어 출력
mypattern3<-gregexpr("[[:alpha:]]+(ing)\\b",mysentences)
myings<-regmatches(mysentences,mypattern3)

#문서 전체에서 ing로 끝나는 영어 단어를 모두 추출하고
#빈도수를 조사
table(unlist(myings))

mypattern<-gregexpr("[[:alpha:]]+(ing)\\b", tolower(mysentences))
mypattern
myings<-regmatches(tolower(mysentences),mypattern)
table(unlist(myings))

#대소문자 구분없이 stat~으로 시작되는 단어 추출
mypattern<-gregexpr("(stat)[[:alpha:]]+",tolower(mysentences))
regmatches(tolower(mysentences),mypattern)

mypattern<-gregexpr("[[:upper:]]", mysentences)
my.uppers<-regmatches(mysentences, mypattern)
table(unlist(my.uppers))

mypattern<-gregexpr("[[:lower:]]", mysentences)
my.lowers<-regmatches(mysentences, mypattern)
table(unlist(my.lowers))

mypattern<-gregexpr("[[:upper:]]", toupper(mysentences))
my.alphas<-regmatches(toupper(mysentences), mypattern)
mytable<-table(unlist(my.alphas))
mytable
mytable[mytable==max(mytable)]
sum(mytable)

#빈도수 시각화
library(ggplot2)
class(mytable)
#ggplot할때 데이터 프레임으로 변환한 다음 시각화
mydata<-data.frame(mytable)
ggplot(data = mydata, aes(x=Var1, y=Freq, fill=Var1))+
  geom_bar(stat='identity')+
#guides(fill=FALSE)+  #범례
  geom_hline(aes(yintercept=median(mytable)))+
  xlab("알파벳")+
  ylab("빈도수")


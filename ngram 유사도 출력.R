################유사도 출력#############################
text1<-"오늘 강남에서 맛있는 스파게티를 먹었다."
text2<-"강남에서 먹었던 오늘의 스파게티는 맛있었다."

library(tm)
#구두점 제거
text1<-removePunctuation(text1)
text2<-removePunctuation(text2)

#한 글자씩 나누기
text1.splited<-str_split(text1,"")
text2.splited<-str_split(text2,"")
text1.splited
text2.splited

#각 문장의 단어수
length1<-length(unlist(text1.splited))
length2<-length(unlist(text2.splited))
length1 #21
length2 #23

subtext1<-list()
subtext1
for (i in 1:length1-1){
  subtext1<-substr(text1, start = i, stop = i+1)
}
subtext1

exam<-read.csv('Data/csv_exam.csv')
exam
library(dplyr)
library(ggplot2)

exam %>% summarise(mean_math=sum(math))

#반별 수학점수의 평균
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mm=mean(math),
            sm=sum(math),
            md=median(math),
            cnt=n()) #그룹에 속하는 멤버의 수를 출력

mpg
#제조사를 기준으로 그룹화
#시내 주행 연비 평균 출력
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mc=mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer,drv) %>% 
  summarise(mc=mean(cty)) %>% 
  head(10)

#mpg데이터를 회사별로 그룹화, suv 추출
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
#tot=cty와 hwy의 평균값
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(mt=mean(tot)) %>% 
#mt를 기준으로 내림차순 정렬
  arrange(desc(mt)) %>% 
  head(5)

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))  
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,80,40,80,75))  
#'id'를 기준으로 합치기
total<-left_join(test1,test2, by='id')
total
exam

name<-data.frame(class=c(1,2,3,4,5),
                 teacher=c("kim","lee","park","choi","cho"))
#exam에 name이 join
exam_new<-left_join(exam,name, by="class")
exam_new

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))  
test2<-data.frame(id=c(6,7,8,9,10),
                  final=c(70,80,40,80,75))
#위아래 방향으로 연결
ta<-bind_rows(test1,test2)
ta

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))  
test2<-data.frame(id=c(6,7,8,9,10),
                  midterm=c(70,80,40,80,75))
ta<-bind_rows(test1,test2)
ta

exam %>% filter(english>=80)
exam %>% filter(class==1 & math>=50)
#class가 1,3,5인 값 추출
exam %>% filter(class %in% c(1,3,5))

exam %>% 
  select(id,math)

#test컬럼 추가(mutate)
#english >=60 =>pass, fail
exam %>% 
  mutate(test=ifelse(english>=60, "pass","fail")) %>% 
  arrange(test)

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))  
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,80,40,80,75))
test1
test2
left_join(test1,test2,by="id")

#결측값
df<-data.frame(sex=c("M","F",NA,"M","F"),
               score=c(5,4,3,5,NA))
df
#결측값이 있는지 확인
is.na(df)
#빈도조사
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))
#score열의 평균 출력
mean(df$score)
sum(df$score)

#score가 NA인 데이터만 출력
df %>% filter(is.na(score))
#score가 NA가 아닌 데이터만 출력
df %>% filter(!is.na(score))  
#score 결측치 제거
df_nomiss<-df %>% filter(!is.na(score))  
mean(df_nomiss$score)
sum(df_nomiss$score)

#score,sex컬럼에서 na가 아닌 데이터만 추출
df_nomiss<-df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

df
#결측치 없는 데이터만 추출
df_nomiss2<-na.omit(df) 
df_nomiss2

mean(df$score, na.rm = T) #결측값을 제외하고 연산
sum(df$score, na.rm = T)

exam<-read.csv("csv_exam.csv")
exam

exam[c(3,8,15),"math"] #3,8,15행 'math'열
exam[c(3,8,15),"math"]<-NA
exam

exam %>% summarise(mm=mean(math))

exam %>% summarise(mm=mean(math, na.rm=T),
                   sm=sum(math, na.rm=T),
                   ned=median(math, na.rm=T))
#대체
#math
#math열 값이 na이면 55를 대입
exam$math<-ifelse(is.na(exam$math),55,exam$math)
exam$math
table(is.na(exam$math))
mean(exam$math)

#outlier를 찾아서 제거
df<-data.frame(sex=c(1,2,1,3,2,1),
               score=c(5,4,3,6,2,6))
#sex열: 1,2가 아니면 outlier
#score: 5이상이면 outlier
table(df$sex)
table(df$score)

df$sex<-ifelse(df$sex==3,NA,df$sex)
df$score<-ifelse(df$score>=5,NA,df$score)

df %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(ms=mean(score))

boxplot(mpg$hwy) 
boxplot(mpg$hwy)$stats
median(mpg$hwy)

mpg$hwy<-ifelse(mpg$hwy<12 | mpg$hwy>37, NA ,mpg$hwy,)
table(is.na(mpg$hwy))

#drv를 기준으로 그룹화
#mean_hwy<-hwy의 평균,결측값은 제외
mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy, na.rm = T))

table(is.na(df$score))

#배경 설졍
ggplot(data = mpg, aes(x=displ, y=hwy))
ggplot(data = mpg, aes(x=displ, y=hwy))+
  geom_point()+    #산점도 그래프
  xlim(3,6)+
  ylim,(10,30)

df_mpg<-mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hway=mean(hwy))
df_mpg

ggplot(data = df_mpg, aes(x=drv, y=mean_hway))+geom_col()

economics
ggplot(data = economics, aes(x=date,y=unemploy))+
  geom_line()
ggplot(data = economics, aes(x=date,y=unemploy))+
  geom_point()


install.packages("foreign")
library(foreign) #SPSS 파일 로드
library(dplyr) #전처리
library(ggplot2) #시각화
library(readxl) #엑셀파일

raw_welfare<-read.spss(file="Data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare<-raw_welfare

str(welfare)

View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare<-rename(welfare,
       sex=h10_g3,       #성별
       birth=h10_g4,
       marriage=h10_g10,
       religion=h10_g11,
       code_job=h10_eco9,
       income=p1002_8aq1,
       code_region=h10_reg7)   #지역 코드
welfare
View(welfare)
class(welfare$sex)  #type함수와 동일
table(welfare$sex)

#이상치를 결측값으로 처리
welfare$sex=ifelse(welfare$sex==9, NA, welfare$sex)
table(is.na(welfare$sex))

#1=>'male', 2=>'female
welfare$sex<-ifelse(welfare$sex==1,"male","female")
table(welfare$sex)
qplot(welfare$sex) #막대그래프 출력

class(welfare$income)
summary(welfare$income) #기술통계치
qplot(welfare$income)+
  xlim(0,1000)

summary(welfare$income)

#이상치 결측값 처리
welfare$income<-ifelse(welfare$income %in% c(0,9999), NA, welfare$income) 
 #0부터9999사이에 속하는 경우

table(is.na(welfare$income))

sex_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mi=mean(income))

ggplot(data = sex_income,
       aes(x=sex, y=mi))+
  geom_col()

summary(welfare$birth)
table(is.na(welfare$birth))

#9999=>NA
welfare$birth<-ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))
welfare$age<-2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

#연령별 수입
age_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mi=mean(income))
head(age_income)
ggplot(data=age_income,
       aes(x=age, y=mi))+
  geom_line()

welfare<-welfare %>% 
  mutate(ageg=ifelse(age<30, "young",ifelse(age<=59,"middle","old")))
welfare$ageg
table(welfare$ageg)
qplot(welfare$ageg)

#연령대별(초년,중년,장년) 월 수입의 평균
ageg_income<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mi=mean(income))
ageg_income
ggplot(data = ageg_income,
       aes(x=ageg, y=mi))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

#성별 월급 차이는 연령대별로 다를까?
sex_income<-welfare %>%
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mi=mean(income))
sex_income
ggplot(data = sex_income, aes(x=ageg, y=mi, fill=sex))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

ggplot(data = sex_income, aes(x=ageg, y=mi, fill=sex))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits=c("young","middle","old"))

#성별(sex),연령별(age) 월급 평균표
sex_age<-welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mi=mean(income))
head(sex_age)

ggplot(data=sex_age, aes(x=age, y=mi, col=sex))+geom_line()

welfare$code_job
#직업 코드별 인원수 확인
table(welfare$code_job)

library(readxl)
list_job<-read_excel("Data/Koweps_Codebook.xlsx")
list_job
#2번 sheet를 읽기
list_job<-read_excel("Data/Koweps_Codebook.xlsx", sheet=2, col_names = T)
list_job
str(welfare)
welfare$code_job
welfare<-left_join(welfare,list_job, id="code_job")
welfare$job
welfare$code_job

#welfare에서 code_job이 na가 아닌 데이터에 대해
#code_job, job열을 추출
welfare %>%
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(20)

job_income<-welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mi=mean(income))
head(job_income)

#상위 10개 직업 추출
top10<-job_income %>% 
  arrange(desc(mi)) %>% 
  head(10)

ggplot(data = top10, aes(x=job, y=mi))+
  geom_col()+
  coord_flip()

ggplot(data = top10, aes(x=reorder(job,-mi), y=mi))+
  geom_col()+
  coord_flip() #축이 바뀜

#############################################################
welfare
View(welfare)
welfare %>% 
  filter(!is.na(sex)) %>% 
  group_by(job)

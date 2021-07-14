# MotionSense Data Analysis 
### 모션 센서 데이터 분석

#### Data Source : https://github.com/mmalekzadeh/motion-sense 

### Data 
#### ● 피실험자 24명의 모션 센서 데이터 셋
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125541835-98f356b5-d23a-4c0b-9fe4-48a3947debf6.png" alt="factorio thumbnail"/>
</p> 

#### ● 24명의 실험자의 downstairs, upstairs, walking, jogging, sitting, standing실험에 대한 가속도, 자이로 센서 값이 들어있음  
#### ● 50Hz sample rate (1초에 값이 50개 기록되도록 함)
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125542060-60d7de6f-dbdd-425a-8995-b07631f4e8a7.png" alt="factorio thumbnail"/>
</p> 

##### Package Used
```
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RWeka)
library(pracma)
library(quantmod)
library(seewave)
library(rematch)
library(rlang)
library(changepoint)
```

#### Before Analysis
```
#경로설정
setwd("C:/Users/h0103/Downloads/A_DeviceMotion_data/A_DeviceMotion_data")

#디렉토리확인
d<-getwd()
print(d)

#폴더에 있는 파일 읽어오기
fls <-dir(d,recursive=TRUE)
fls

library(stringr)

# 객체 생성
for(f in fls){ #파일 이름을 하나하나 읽어와서 끝까지 반복
  a <-file.path(str_c(d,"/",f)) #폴더경로명과 파일명을 붙여야함(str_c)
  temp <-read.csv(a) #csv파일 읽어서 temp객체에 담기
  assign(f,temp) # 객체를 temp에 있는 객체명은 f
  
}
#mag함수 만들기 : x,y,z축의 3개의 값을 가진 센서값을 하나의 크기로 만들어주는 함수
mag <-function(df,column){
  df[,str_c("mag",column)]<- # 센서명을 넣어줌 행은 안건들임 (sqrt(.x^2+.y^2+.z^2)구하는 식)
    with(df,sqrt(get(str_c(column,".x"))^2+get(str_c(column, ".y"))^2+get(str_c(column, ".z"))^2))
  return(df)
  
}

#가속도센서 x,y,z값 구하기
mag(`wlk_15/sub_1.csv`,"userAcceleration")
```

### 1.한 사람의 데이터(sub_1)만 뽑아와서 분석
#### ①walking실험 데이터만 뽑아와서 시각화 (시간에 따른 가속도값)
```
user1 <-fls[str_detect(fls,"sub_1.csv")] # sub_1.->점으로 끝나면 패턴으로 인식하기 때문에 sub_1/.
#sub_1의 walking데이터 파일명만 추리기
user1_walking<-user1[str_detect(user1,"wlk")] #str_detect의 TRUE값만 user_Walking에 들어감

#빈 df생성
user1_walking_total<- data.frame()

for(f in user1_walking){ #user1의 walking 데이터를 차례때로 받음
  temp<-get(f) # 이름말고 내용을 가져와라
  
  user1_walking_total <-rbind(user1_walking_total,
                              temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                            id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2]))
}
unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1])) #정수값 뽑아서 regmatches에 넣어줌

#변수만들기
user1_walking_total<- mag(user1_walking_total,"userAcceleration")

#시간값은 데이터 순번으로 새로운 변수 생성
user1_walking_total<-user1_walking_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()

#시각화
ggplot(user1_walking_total,aes(x=time,y=maguserAcceleration))+geom_line()+facet_wrap(.~exp_no,nrow=3)
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125542694-711a19b8-b4a1-40bc-bae2-7f6533dbede3.png" alt="factorio thumbnail"/>
</p> 

#### ②jogging실험 데이터만 뽑아서 시각화 (시간에 따른 가속도값)
```
user1_jogging<-user1[str_detect(user1,"jog")]
user1_jogging_total<- data.frame()

for(f in user1_jogging){
  temp<-get(f) # 이름말고 내용을 가져와라
  
  user1_jogging_total <-rbind(user1_jogging_total,
                              temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                            id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2]))
}

user1_jogging_total<- mag(user1_jogging_total,"userAcceleration")

user1_jogging_total<-user1_jogging_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()

#시각화
ggplot(user1_jogging_total,aes(x=time,y=maguserAcceleration))+geom_line()+facet_wrap(.~exp_no,nrow=3)
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125542921-6bab6bd6-92f2-4070-a799-cff109101272.png" alt="factorio thumbnail"/>
</p> 

#### ③upstairs 실험 데이터만 뽑아서 시각화 (시간에 따른 중력값)
```
user1_upstair <- user1[str_detect(user1,"ups")]
user1_upstair_total <- data.frame()

for (f in user1_upstair){
  temp<-get(f)
  user1_upstair_total<-rbind(user1_upstair_total,
                             temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                           id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2]))
}

user1_upstair_total<-mag(user1_upstair_total,"gravity")
user1_upstair_total<-user1_upstair_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()

#시각화
ggplot(user1_upstair_total,aes(x=time,y=maggravity))+geom_line()+facet_wrap(.~exp_no,nrow=3)
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125543705-cdd41d61-131b-488a-bb11-890f09bff96d.png" alt="factorio thumbnail"/>
</p> 

#### ④downstairs 실험 데이터만 뽑아서 시각화 (시간에 따른 중력값)
```
user1_downstair <- user1[str_detect(user1,"dws")]
user1_downstair_total <- data.frame()

for (f in user1_downstair){
  temp<-get(f)
  user1_downstair_total<-rbind(user1_downstair_total,
                             temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                           id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2]))
}

user1_downstair_total<-mag(user1_downstair_total,"gravity")
user1_downstair_total<-user1_downstair_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()

#시각화
ggplot(user1_downstair_total,aes(x=time,y=maggravity))+geom_line()+facet_wrap(.~exp_no,nrow=3)
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125544073-1ac3e73e-c686-4c3f-adfa-c52991e06eca.png" alt="factorio thumbnail"/>
</p> 

### 2.전체 데이터 분석
```
HAR_total<-data.frame()

for(f in fls){
  temp<-get(f)
  
  HAR_total <-rbind(HAR_total,
                    temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                  id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2],
                                  activity=unlist(str_split(f,"\\_"))[1]))
}

#rss정의
rss <- function(x) rms(x)*(length(x))^0.5

HAR_total <- mag(HAR_total,"userAcceleration")
HAR_total <- mag(HAR_total,"rotationRate")
HAR_total <- mag(HAR_total, "gravity")

#가속도, 회전, 중력 등의 변수에 대해 통계특질 생성
HAR_summary<-HAR_total%>%group_by(id,exp_no,activity)%>%dplyr::summarize_at(.vars=c("maguserAcceleration","magrotationRate",
"maggravity","attitude.roll","attitude.pitch","attitude.yaw"),.funs=c(mean,min,max,sd,skewness,rms,rss))

for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  f <- mag(f, "gravity")
  assign(d,f)
}

#피크관련 통계량
peak_rslt2 <- data.frame()

for(d in fls){
  f <- get(d)
  p1 <- findpeaks(f$magrotationRate,threshold=4)
  p2 <- findpeaks(f$maguserAcceleration,threshold=4)
  p3 <- findpeaks(f$maggravity,threshold=4)
  peak_rslt2 <- rbind(peak_rslt2,data.frame(d,
                                            
                                            f_n1 = ifelse(!is.null(p1), dim(p1)[1],0),
                                            f_n2 = ifelse(!is.null(p2), dim(p2)[1],0),
                                            f_n3 = ifelse(!is.null(p3), dim(p3)[1],0),
                                            p_interval1 = ifelse(!is.null(p1), ifelse(dim(p1)[1]>2, mean(diff(p1[,2])),0),0),
                                            p_interval2 = ifelse(!is.null(p2), ifelse(dim(p2)[1]>2, mean(diff(p2[,2])),0),0),
                                            p_interval3 = ifelse(!is.null(p3), ifelse(dim(p3)[1]>2, mean(diff(p3[,2])),0),0),
                                            p_interval_std1 = ifelse(!is.null(p1), ifelse(dim(p1)[1]>2, std(diff(p1[,2])),0),0),
                                            p_interval_std2 = ifelse(!is.null(p2), ifelse(dim(p2)[1]>2, std(diff(p2[,2])),0),0),
                                            p_interval_std3 = ifelse(!is.null(p3), ifelse(dim(p3)[1]>2, std(diff(p3[,2])),0),0),
                                            p_mean1 = ifelse(!is.null(p1),mean(p1[,1]),0),
                                            p_mean2 = ifelse(!is.null(p2),mean(p2[,1]),0),
                                            p_mean3 = ifelse(!is.null(p3),mean(p3[,1]),0),
                                            p_max1 = ifelse(!is.null(p1), max(p1[,1]),0),
                                            p_max2 = ifelse(!is.null(p2), max(p2[,1]),0),
                                            p_max3 = ifelse(!is.null(p3), max(p3[,1]),0),
                                            p_min1 = ifelse(!is.null(p1), min(p1[,1]),0),
                                            p_min2 = ifelse(!is.null(p2), min(p2[,1]),0),
                                            p_min3 = ifelse(!is.null(p3), min(p3[,1]),0),
                                            p_std1 = ifelse(!is.null(p1), std(p1[,1]),0),
                                            p_std2 = ifelse(!is.null(p2), std(p2[,1]),0),
                                            p_std3 = ifelse(!is.null(p3), std(p3[,1]),0)
  ))
}

#파고율
temp<- data.frame()
for (d in fls){
  f <- get(d)
  f <- f%>%select(magrotationRate,maguserAcceleration,maggravity)
  cfR <- crest(f$magrotationRate, 50, plot=TRUE)
  cfA <- crest(f$maguserAcceleration, 50, plot=TRUE)
  cfg <- crest(f$maggravity, 50, plot=TRUE)
  temp <- rbind(temp, data.frame(d, cfR=cfR$C, cfA=cfA$C, cfg=cfg$C))
}

#d를 기준으로 피크특징분석과 파고율 합침
peak_final_new <- merge(peak_rslt2, temp, by="d")

#id_f 사용자 정의함수 생성(d에서 필요한 정보 추출용)
id_f<-function(x){
  exp_no=unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[2]
  activity=unlist(str_split(x,"\\_"))[1]
  return (cbind(exp_no, id, activity))
}

#정보추출해서 df생성
temp <- data.frame()
for(i in 1:nrow(peak_final_new)){
  temp<-rbind(temp,id_f(peak_final_new$d[i]))
}

peak_final_new2 <- cbind(peak_final_new, temp)

#통계특징과 피크특징 합치기
HAR_peak <- merge(peak_final_new2, HAR_summary, by=c("id","exp_no","activity")) 

ch_pt_pelt <- data.frame() 

for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  f <- mag(f, "gravity")
  rslt <- sapply(f%>%select(magrotationRate,maguserAcceleration,maggravity),cpt.mean,method="PELT")
  rslt_cpts1 <- cpts(rslt$magrotationRate)
  rslt_cpts2 <- cpts(rslt$maguserAcceleration)
  rslt_cpts3 <- cpts(rslt$maggravity)
  rslt2 <- sapply(f%>%select(magrotationRate,maguserAcceleration,maggravity),cpt.var,method="PELT")
  rslt2_cpts1 <- cpts(rslt2$magrotationRate)
  rslt2_cpts2 <- cpts(rslt2$maguserAcceleration)
  rslt2_cpts3 <- cpts(rslt2$maggravity)
  rslt3 <- sapply(f%>%select(magrotationRate,maguserAcceleration,maggravity),cpt.meanvar,method="PELT")
  rslt3_cpts1 <- cpts(rslt3$magrotationRate)
  rslt3_cpts2 <- cpts(rslt3$maguserAcceleration)
  rslt3_cpts3 <- cpts(rslt3$maggravity)
  
  ch_pt_pelt <- rbind(ch_pt_pelt, data.frame(d,cp1=length(rslt_cpts1),cp2=length(rslt_cpts2),cp3 = length(rslt_cpts3), 
  cp4=length(rslt2_cpts1),cp5=length(rslt2_cpts2),cp6 = length(rslt2_cpts3), cp7=length(rslt3_cpts1),cp8=length(rslt3_cpts2), 
  cp9 = length(rslt3_cpts3) ))
}

for(d in fls){
  f <- get(d)
  f <- mag(f, "rotationRate")
  f <- mag(f, "userAcceleration")
  f <- mag(f, "gravity")
  assign(d,f)
}

id_f <- function(x){
  exp_no = unlist(regmatches(x,gregexpr("[[:digit:]]+", x)[1]))[1]
  id=unlist(regmatches(x,gregexpr("[[:digit:]]+",x)[1]))[2]
  activity=unlist(str_split(x,"\\_"))[1]
  return(cbind(exp_no,id,activity))
}

temp <- data.frame()
for(i in 1:nrow(ch_pt_pelt)){
  temp <- rbind(temp,id_f(ch_pt_pelt$d[i]))
}

#변화분석에 통계특징과 피크특징 합쳐놓은 것 합치기
ch_pt_pelt <- cbind(ch_pt_pelt, temp)
HAR_peak_chpt_pelt <- merge(HAR_peak, ch_pt_pelt, by = c("id","exp_no","activity","d"))

final <- HAR_peak_chpt_pelt%>%ungroup()%>%select(-d,-exp_no,-id)
```
#### 
```
#바로 검색이 안되는 알고리즘 이름 정의
RF<-make_Weka_classifier("weka/classifiers/trees/RandomForest")
Bayes_net<-make_Weka_classifier("weka/classifiers/bayes/BayesNet")

m3 <- RF(as.factor(activity)~., data=final)
summary(m3) #학습모델
```
```
e3 <- evaluate_Weka_classifier(m3, numFolds =10, complexity = TRUE, class = TRUE) #평가
e3
```



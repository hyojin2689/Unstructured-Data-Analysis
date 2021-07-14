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
library(Rweka)
library(pracma)
library(quantmod)
library(seewave)
library(rematch)
library(rlang)
library(changepoint)
library(
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



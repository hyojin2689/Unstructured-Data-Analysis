### Hypotensive Prediction project
##### 저혈압 예측 모델 생성 프로젝트
##### ⠀
##### Data Source : http://aibig.sch.ac.kr/main.do (250hz) (순천향대학교 AI&빅데이터 센터의 데이터 사용)
##### ⠀⠀
#### Package Used
```
library(dplyr)
library(stringr)
library(tidyverse)
library(e1071)
library(changepoint)
library(RWeka)
```
#### ⠀⠀
#### Data Preprocessing
##### -
```
SRATE<-250
MINUTES_AHEAD=1
Data_set<-data.frame() #샘플 생성 후 저장할 공간 

#moving average 사용자 정의 함수 생성
ma<-function(x,n=5){stats::filter(x,rep(1/n,n),sides=2)}

#전처리 (:불필요한 행과 문자 삭제 및 필요한 값만 추출) 후 객체화
for (file in fls){
  df<- read.csv(file)
  df<-df[-c(1,2),] 
  df$signal<-str_replace(df$signal,'\\[','')
  df$signal<-str_replace(df$signal,'\\]','')
  c = str_split(df$signal,",")
  class <- c()
  for(i in 1:length(c)){
    class <- c(class, c[[i]][2])
  }
  df$signal<-class
  assign(file,df) #전처리 된 파일들을 객체화해서 다시 file에 넣어줌
}

Data_set<-list() #샘플 생성 후 저장할 공간

#데이터셋만들기
for (file in fls)){
  IBP<-as.numeric(get(file)$IBP) #문자인 signal컬럼 내용을 숫자로 변환
  i<-1
  IBP_data<-data.frame()
  while(i<length(IBP)-SRATE*(1+1+MINUTES_AHEAD)*60){
    segx <- IBP[i:(i+SRATE*1*60-1)] #1분간의 데이터 관찰
    segy<-IBP[(i+SRATE*(1+MINUTES_AHEAD)*60):(i+SRATE*(1+1+MINUTES_AHEAD)*60-1)] #1분뒤 저혈압 발생여부를 1분동안 관찰하여 class화 
    segxd<-IBP[i:(i+SRATE*(1+MINUTES_AHEAD)*60-1)]
    if(is.na(mean(segx))| #데이터 제외조건들
       is.na(mean(segy))|
       max(segx)>200| min(segx)<20|
       max(segy)>200| min(segy)<20|
       max(segx)-min(segx)<30|
       max(segy)-min(segy)<30|(min(segxd,na.rm=T)<=50)){
    }else{ #나머지의 경우
      segy<-ma(segy,2*SRATE)
      event<-ifelse(min(segy,na.rm=T)<=50,1,0)
      IBP_data<-rbind(IBP_data,cbind(t(segx),event))
    } #저혈압 조건 : 평균 혈압을 계산하여 50이하면 저혈압
    i<-i+1*60*SRATE #1분 간격으로 샘플 만들기
  }
  Data_set[[file]]<-IBP_data
}  

#전체 데이터 합치기 : 파일들을 하나의 데이터 프레임으로 합치기(final_data)
final_data<-data.frame()
for(file in fls){
  data<- Data_set[[file]]
  final_data<-rbind(final_data,data)
}
```




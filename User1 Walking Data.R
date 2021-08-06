#경로설정
setwd("C:/Users/h0103/Downloads/A_DeviceMotion_data/A_DeviceMotion_data")

#디렉토리확인
d<-getwd()
print(d)

#폴더에 있는 파일 읽어오기
fls <-dir(d,recursive=TRUE)
fls

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

#한사람 데이터만 추출하여 시각화 (sub_1)
user1 <-fls[str_detect(fls,"sub_1.csv")] # sub_1.->점으로 끝나면 패턴으로 인식하기 때문에 sub_1/.
#sub_1의 walking데이터 파일명만 추리기
user1_walking<-user1[str_detect(user1,"wlk")] #str_detect의 TRUE값만 user_Walking에 들어감

library(dplyr)
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
user1_walking_total<- mag(user1_walking_total,"rotationRate")
user1_walking_total<- mag(user1_walking_total,"gravity")

#시간값은 데이터 순번으로 새로운 변수 생성
user1_walking_total<-user1_walking_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()
user1_walking_total<-user1_walking_total[c(14,16,17,18,19)]

View(user1_walking_total)

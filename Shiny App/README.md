## **Develop Shiny App about Walking data of user1 in Mobile phone sensor data** πΆπ±
### **μ€νμ 1μ νΈλν° μΌμ λ°μ΄ν° μ κ±·κΈ°μ€ν λ°μ΄ν°μ λν μ€μ΄λ μ± κ΅¬ν**
###### β 
#### βμ€νμ 1μ walking μ€νμ λν λ°μ΄ν° νμ΄λΈ μ λ³΄
#### βwalking μ€νμμ μκ°μ λ°λ₯Έ κ°μλ, μ€λ ₯, νμ μ¨ κ°μ κ·Έλνλ‘ μκ°ν 
###### β 
#### Package Used
```
library(stringr)
library(dplyr)
library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(shinythemes)
```
###### β 

### Data 
#### β νΌμ€νμ 24λͺμ λͺ¨μ μΌμ λ°μ΄ν° μ
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125541835-98f356b5-d23a-4c0b-9fe4-48a3947debf6.png" alt="factorio thumbnail"/>
</p> 

#### β 24λͺμ μ€νμμ downstairs, upstairs, walking, jogging, sitting, standingμ€νμ λν κ°μλ, μμ΄λ‘ μΌμ κ°μ΄ λ€μ΄μμ  
#### β 50Hz sample rate (1μ΄μ κ°μ΄ 50κ° κΈ°λ‘λλλ‘ ν¨)
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125542060-60d7de6f-dbdd-425a-8995-b07631f4e8a7.png" alt="factorio thumbnail"/>
</p> 

#### Data Source : https://github.com/mmalekzadeh/motion-sense 
###### β 
### 1.Data Preprocessing
#### βuser1_walking_total λ°μ΄ν° μμ±
```
#κ²½λ‘μ€μ 
setwd("C:/Users/h0103/Downloads/A_DeviceMotion_data/A_DeviceMotion_data")

#λλ ν λ¦¬νμΈ
d<-getwd()
print(d)

#ν΄λμ μλ νμΌ μ½μ΄μ€κΈ°
fls <-dir(d,recursive=TRUE)
fls

# κ°μ²΄ μμ±
for(f in fls){ #νμΌ μ΄λ¦μ νλνλ μ½μ΄μμ λκΉμ§ λ°λ³΅
  a <-file.path(str_c(d,"/",f)) #ν΄λκ²½λ‘λͺκ³Ό νμΌλͺμ λΆμ¬μΌν¨(str_c)
  temp <-read.csv(a) #csvνμΌ μ½μ΄μ tempκ°μ²΄μ λ΄κΈ°
  assign(f,temp) # κ°μ²΄λ₯Ό tempμ μλ κ°μ²΄λͺμ f
  
}
#magν¨μ λ§λ€κΈ° : x,y,zμΆμ 3κ°μ κ°μ κ°μ§ μΌμκ°μ νλμ ν¬κΈ°λ‘ λ§λ€μ΄μ£Όλ ν¨μ
mag <-function(df,column){
  df[,str_c("mag",column)]<- # μΌμλͺμ λ£μ΄μ€ νμ μκ±΄λ€μ (sqrt(.x^2+.y^2+.z^2)κ΅¬νλ μ)
    with(df,sqrt(get(str_c(column,".x"))^2+get(str_c(column, ".y"))^2+get(str_c(column, ".z"))^2))
  return(df)
  
}

#κ°μλμΌμ x,y,zκ° κ΅¬νκΈ°
mag(`wlk_15/sub_1.csv`,"userAcceleration")

#νμ¬λ λ°μ΄ν°λ§ μΆμΆνμ¬ μκ°ν (sub_1)
user1 <-fls[str_detect(fls,"sub_1.csv")] # sub_1.->μ μΌλ‘ λλλ©΄ ν¨ν΄μΌλ‘ μΈμνκΈ° λλ¬Έμ sub_1/.
#sub_1μ walkingλ°μ΄ν° νμΌλͺλ§ μΆλ¦¬κΈ°
user1_walking<-user1[str_detect(user1,"wlk")] #str_detectμ TRUEκ°λ§ user_Walkingμ λ€μ΄κ°

library(dplyr)
#λΉ dfμμ±
user1_walking_total<- data.frame()

for(f in user1_walking){ #user1μ walking λ°μ΄ν°λ₯Ό μ°¨λ‘λλ‘ λ°μ
  temp<-get(f) # μ΄λ¦λ§κ³  λ΄μ©μ κ°μ ΈμλΌ
  
  user1_walking_total <-rbind(user1_walking_total,
                              temp%>%mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[1],
                                            id=unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1]))[2]))
}
unlist(regmatches(f,gregexpr("[[:digit:]]+", f)[1])) #μ μκ° λ½μμ regmatchesμ λ£μ΄μ€

#λ³μλ§λ€κΈ°
user1_walking_total<- mag(user1_walking_total,"userAcceleration")
user1_walking_total<- mag(user1_walking_total,"rotationRate")
user1_walking_total<- mag(user1_walking_total,"gravity")

#μκ°κ°μ λ°μ΄ν° μλ²μΌλ‘ μλ‘μ΄ λ³μ μμ±
user1_walking_total<-user1_walking_total%>%group_by(exp_no)%>%mutate(time=row_number())%>%ungroup()
user1_walking_total<-user1_walking_total[c(14,16,17,18,19)]

View(user1_walking_total)
```
<p align="center">
  <img src="https://user-images.githubusercontent.com/80669371/125882173-9ffca611-3c48-4d84-bfc7-13cde61fe4ea.png" alt="factorio thumbnail"/>
</p> 


### 2.Develop Shiny App
```
ui <- fluidPage(
  titlePanel(title="Walking data of user1 in Mobile phone sensor data."),
  sidebarPanel( #νλ©΄λΆν 
    sidebarPanel( textInput(inputId = 'vars',
                            label='variables',
                            value='acceleration')), #λ¬Έμμ΄ μλ ₯
    selectInput('x','x',choices=c('maguserAcceleration','maggravity','magrotationRate'),selected='maguserAcceleration'),
    selectInput('y','Y',choices="time",selected='time'), #μλ ₯λ°μ΄ν° μ ν
    submitButton(text='Apply the changes',icon=icon(name='update')), #μ μΆλ²νΌμμ±
    helpText('Press the button above to update your changes.')), #λμλ§
  mainPanel(
    uiOutput(outputId = 'mainUI'), 
    textOutput(outputId = 'activity'),#μΌλ°λ¬Έμμ΄λ‘μΆλ ₯
    tabsetPanel( #μΆλ ₯ν­μ μ¬λ¬κ°μ ν­μΌλ‘ λλκΈ°
      type='tabs',
      tabPanel(h5('walking by var'),title='plot',plotOutput(outputId='line1')), #κ·ΈλνμΆλ ₯
      tabPanel(h5('user1_walking_total'),title='data table',DTOutput(outputId='user1')) #νμ΄λΈ μΆλ ₯
    )
  )
)

server <- function(input,output,session) {
  output$user1=renderDT( #λ°μ΄ν°νμ΄λΈ
    user1_walking_total,options=list(lengthChange=FALSE)
  )
  output$activity <-renderPrint({ #μΆλ ₯λ  λ¬Έμμ΄ μμ±
    str_c('Work data for user1 according to ', input$vars, sep='')
  })
  output$line1<-renderPlot({ #μΆλ ₯ν  κ·Έλν μμ±
    ggplot(user1_walking_total,aes_string(x=input$y,y=input$x))+geom_line()+
      facet_wrap(.~exp_no,ncol=1)
  })
}

shinyApp(ui=ui, server=server)
```
###### β 
### 3.Deploy Link π
#### https://shinhyojin.shinyapps.io/newshiny/



# Unstructured-data-analysis-in-R

## MotionSense Data Analysis 
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

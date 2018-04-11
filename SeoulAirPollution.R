install.packages("dplyr")
library(dplyr)
library(ggplot2)

setwd("C:/github/CA_study-klp")

Seoul_airdata <- read.csv("SeoulHourlyAvgAirPollution.csv", header=T, stringsAsFactors=F)
#data road

colnames(Seoul_airdata) <- c("Date/Time","Location","NO2","O3","CO","SO2","FineDust","UltrafineDust")
#colname change

Seoul_airdata$`Date/Time` <- as.character(Seoul_airdata$`Date/Time`)

Seoul_airdata$Date <- substr(Seoul_airdata$`Date/Time`,1,8)
Seoul_airdata$Time <- substr(Seoul_airdata$`Date/Time`,9,12)

Sad <- data.frame(Seoul_airdata[9:10],Seoul_airdata[2:8])

#데이터 순서 정렬하기
Sad <- Sad[c(order(Sad$Location, Sad$Date, Sad$Time)),]

#결측치 개수
table(is.na(Sad)) #NA

#데이터 요약
summary(Sad)
table(Sad$Location)


#그래프 그려보기
g <- ggplot(data=Sad, aes(x = paste(Date,Time), y = UltrafineDust))
g+geom_point()



#위치별로 묶어서 평균 알아보기
Loc_mean <- Sad %>% group_by(Location) %>% summarise(NO2=mean(NO2,na.rm=TRUE),
                                         O3=mean(O3,na.rm=TRUE),
                                         CO=mean(CO,na.rm=TRUE),
                                         SO2=mean(SO2,na.rm=TRUE),
                                         FineDust=mean(FineDust,na.rm=TRUE),
                                         UltrafineDust=mean(UltrafineDust,na.rm=TRUE))

Loc_mean[Loc_mean$NO2==max(Loc_mean$NO2),]
#최대값 알아보기



#시간별로 묶어서 평균 알아보기
DT_mean <- Sad %>% group_by(DT=paste(Date,Time)) %>% summarise(NO2=mean(NO2,na.rm=TRUE),
                                                     O3=mean(O3,na.rm=TRUE),
                                                     CO=mean(CO,na.rm=TRUE),
                                                     SO2=mean(SO2,na.rm=TRUE),
                                                     FineDust=mean(FineDust,na.rm=TRUE),
                                                     UltrafineDust=mean(UltrafineDust,na.rm=TRUE))



#회귀분석

#정규분포 확인
hist(Sad$FineDust)
qqnorm(Sad$FineDust)
qqline(Sad$FineDust)
#직선에서 점들이 크게 벗어나지 않는다면 정규분포를 따른다고 할 수 있다.
#NO2

#회귀모델만들기 *미세먼지
model <- lm(FineDust~,data=Sad)
summary(model)

#회귀모델만들기 *초미세먼지포하
model <- lm(FineDust~NO2+O3+CO+SO2+UltrafineDust ,data=Sad, na.action = na.omit)
summary(model)

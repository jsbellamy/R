#Data Manipulation
library(dplyr)
library(hflights)
View(hflights)
#select

select(hflights,FlightNum,ArrTime,DepTime)->flight1

#mutate

mutate(hflights,ActualGroundTime=ActualElapsedTime-AirTime)->flight1
mutate(flight1,AverageSpeed=Distance/AirTime*60)->flight1
mutate(flight1,TotalTaxii=TaxiIn+TaxiOut)->flight1
mutate(flight1,TimeLoss=ArrDelay+DepDelay)->flight1
View(flight1)

#filter
filter(hflights,Distance>3000)->flight1

#arrange
arrange(hflights,DepDelay)->flight1

#summarise
summarise(hflights,min_dist=min(Distance),max_dist=max(Distance))

#pipe
hflights %>% select(contains("Time")) %>% filter(AirTime>60)->flight1

#data visualization

library(ggplot2)

#linear regression
View(mtcars)
library(caTools)
sample.split(mtcars$mpg,SplitRatio=0.65)->mysplit
train<-subset(mtcars,mysplit==T)
test<-subset(mtcars,mysplit==F)
lm(mpg~.,data = train)->mod1
predict(mod1,test)->result
cbind(actual=test$mpg,predicted=result)->final
as.data.frame(final)->final
cbind(final,error=final$actual-final$predicted)->final
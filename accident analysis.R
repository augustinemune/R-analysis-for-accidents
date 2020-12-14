# Making API requests in R
# necessary libraries 
install.packages(c("httr", "jsonlite", "zeallot", "ggplot2", "knitr", "rmarkdown", "modelr", "reprex", "stringr", "tidyverse"))
library(broom)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(Hmisc)
res = GET("https://data.cityofnewyork.us/resource/h9gi-nx95.json")
res
data = fromJSON(rawToChar(res$content))
names(data)
summary(data)
person_injured = data[data$number_of_persons_injured >= 1, ]
count(person_injured)
colSums(is.na(data))


person_killed = data[data$number_of_persons_killed >= 1, ]
count(person_killed)

pedestrians_injured = data[data$number_of_pedestrians_injured >= 1, ]
count(pedestrians_injured)

pedestrians_killed = data[data$number_of_pedestrians_killed >= 1, ]
count(pedestrians_killed)

cyclist_injured = data[data$number_of_cyclist_injured >= 1, ]
count(cyclist_injured)

cyclist_killed = data[data$number_of_cyclist_killed >= 1, ]
count(cyclist_killed)

motorist_injured = data[data$number_of_motorist_injured >= 1, ]
count(motorist_injured)

motorist_killed = data[data$number_of_motorist_killed >= 1, ]
count(motorist_killed)



qplot(number_of_persons_injured, crash_time, data = data)
qplot(number_of_persons_killed, data = data) + ggtitle("title")
qplot(number_of_persons_injured, data = data, geom = "dotplot")

type1 <- data$borough == "BRONX"
type4 <- str_extract_all(data$borough, '(?<=bi:component name="\\w{1,100}" type=")[^"]+')
type2 = ifelse(data$borough == "BROOKLYN",0,1)
type3 = ifelse(data$borough == "MANHATTAN",0,1)
print(type2)
type11 <- sapply(type1, switch, "True"=1,"False"=0)

PE<-cbind(type1,type2, type3)
PE<-as.data.frame(PE)
head(PE)
Summary(PE)
length(type11)
sum(is.na(data$borough == "BRONX"))
sum(is.na(data$borough == "BROOKLYN"))
sum(is.na(data$borough == "MANHATTAN"))


sum(is.na(data$contributing_factor_vehicle_1 == "Unspecified"))
sum(is.na(data$contributing_factor_vehicle_1 == "Reaction"))
type5 = ifelse(data$contributing_factor_vehicle_1 == "Unspecified", 0,1)
x = length(which(type5 == 1))
summary(x)
type6 = ifelse(data$contributing_factor_vehicle_1 == "Reaction", 0,1)
y = length(which(type6 == 1))
summary(y)
type7 = ifelse(data$contributing_factor_vehicle_1 == "Other Vehicular", 0,1)
z = length(which(type7 == 1))
summary(z)

time_Crash = data$crash_time
frame<-as.data.frame(time_Crash)
frame$happened<-ifelse(time_Crash>"18:00" & time_Crash<"23:00", 2, 1)
frame$happened2<-ifelse(time_Crash>"6:00" & time_Crash<"10:00", 2, 1)
frame
summary(frame)
count2 <- length(which(frame$happened == 2))
count2
count1 <- length(which(frame$happened == 1))
count1
count3 <- length(which(frame$happened == 2))
count3

person_injured = data$number_of_persons_injured
person_killed = data$number_of_persons_killed
pendestrian_injury = data$number_of_pedestrians_injured
pendestrian_killed = data$number_of_pedestrians_killed
cyclist_injury = data$number_of_cyclist_injured
cyclist_killed = data$number_of_cyclist_killed
motorist_injury = data$number_of_motorist_injured
motorist_killed = data$number_of_motorist_killed


bar <- c(count1, count2)
barplot(bar,
        main = "Number of accidents that happended",
        xlab="Number of accidents",
        ylab="Count",
        )

vehic = data$vehicle_type_code1

frame
summary(frame)
count2 <- length(which(frame$happened == 2))
count2
count1 <- length(which(frame$happened == 1))
count1
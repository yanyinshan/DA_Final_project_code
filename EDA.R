rm(list = ls())
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data\\Lab")
general_data <- read.csv("general_data.csv")
colnames(general_data)
general_data2 <- general_data[,c(8,9,10,11,12,13,19,21,22,23,24,25,26,27,28,29,46,47,48,49)+1]

write.csv(general_data2,"general_data2.csv")


states <- as.data.frame(summary(general_data2$STATE))
colnames(states) <- "NumOfStorm"
states <- as.data.frame(cbind(Area = rownames(states), states$NumOfStorm))
states <- states[-1,]
colnames(states) <- c("Area", "NumOfStorm")

class(states$NumOfStorm)
states$NumOfStorm <- as.character(states$NumOfStorm)
states$NumOfStorm <- as.numeric(states$NumOfStorm)
states <- states[order(states$NumOfStorm, decreasing = T),]

barplot(states$NumOfStorm[1:5],names.arg = states$Area[1:5], main ="The Top 5 States That Have Most Storms")
general_data2$TotalLoss <- general_data2$DAMAGE_CROPS + general_data2$DAMAGE_PROPERTY


xx <- general_data2$DAMAGE_CROPS
xx[is.na(xx)] <- 0
general_data2$DAMAGE_CROPS <- xx

xx <- general_data2$DAMAGE_PROPERTY
xx[is.na(xx)] <- 0
general_data2$DAMAGE_PROPERTY <- xx

# pivot table
library(dplyr)
library(tidyr)



general_data$DAMAGE_PROPERTY
class(general_data$EVENT_ID)
# frequency by states choropleth map
Year_PropertyLoss <- general_data2 %>%
        select(STATE,DAMAGE_PROPERTY,YEAR) %>%
        group_by(YEAR) %>%
        summarise(PropertyLoss = sum(DAMAGE_PROPERTY))


States_PropertyLoss <- States_PropertyLoss[order(States_PropertyLoss$PropertyLoss,decreasing = T),]
barplot(States_PropertyLoss$PropertyLoss[1:5],names.arg = States_PropertyLoss$STATE[1:5], main = "The Property Loss")


States_CropLoss <- general_data2 %>%
        select(STATE,DAMAGE_CROPS) %>%
        group_by(STATE) %>%
        summarise(CropLoss = sum(DAMAGE_CROPS))
States_CropLoss <- States_CropLoss[order(States_CropLoss$CropLoss,decreasing = T),]
barplot(States_CropLoss$CropLoss[1:5],names.arg = States_CropLoss$STATE[1:5],main = "The Crops Loss")

States_PropertyLoss_by_year

summary(as.factor(general_data2$YEAR))

# Year loss

Year_PropertyLoss <- general_data2 %>%
        select(STATE,DAMAGE_PROPERTY,YEAR) %>%
        group_by(YEAR) %>%
        summarise(PropertyLoss = sum(DAMAGE_PROPERTY))


Year_CropLoss <- general_data2 %>%
        select(YEAR,DAMAGE_CROPS) %>%
        group_by(YEAR) %>%
        summarise(CropLoss = sum(DAMAGE_CROPS))

Year_PropertyLoss$GROUP <- "PropertyLoss"
Year_CropLoss$GROUP <- "CropLoss"

colnames(Year_PropertyLoss) <- c("YEAR","LOSS","GROUP")
colnames(Year_CropLoss) <- c("YEAR","LOSS","GROUP")


Year_loss_combine2 <- rbind(Year_PropertyLoss,Year_CropLoss)

summary(Year_loss_combine2)

Year_loss_combine2$LOSS <- as.numeric(Year_loss_combine2$LOSS)
Year_loss_combine2$GROUP <- as.factor(Year_loss_combine2$GROUP)

ggplot(Year_loss_combine2, aes(x = YEAR, y = LOSS, fill = GROUP)) + 
        geom_area(alpha=0.6 , size=1, colour="black")+
        ggtitle("The Loss caused by Storms ")


total_year <- Year_loss_combine$PropertyLoss+ Year_loss_combine$CropLoss
mean(total_year)

# yearly death

general_data2$TotalDeath <- general_data2$DEATHS_DIRECT + general_data2$DEATHS_INDIRECT

Year_DirectDeath <- general_data2 %>%
        select(YEAR,DEATHS_DIRECT) %>%
        group_by(YEAR) %>%
        summarise(CropLoss = sum(DEATHS_DIRECT))

Year_IndirectDeath <- general_data2 %>%
        select(YEAR,DEATHS_INDIRECT) %>%
        group_by(YEAR) %>%
        summarise(CropLoss = sum(DEATHS_INDIRECT))
Year_DirectDeath$GROUP <- "Direct"
Year_IndirectDeath$GROUP <- "Indirect"
Year_Death_combine2 <- rbind(Year_DirectDeath,Year_IndirectDeath)

colnames(Year_Death_combine2) <-c("YEAR","Death","GROUP")

ggplot(Year_Death_combine2, aes(x = YEAR, y = Death, fill = GROUP)) + 
        geom_area(alpha=0.6 , size=1, colour="black")+
        ggtitle("The Death caused by Storms ")

# Time calculation

# pivot table
library(dplyr)
library(tidyr)


time_s <- as.character(general_data2$BEGIN_DATE_TIME)
time_e <- as.character(general_data$END_DATE_TIME)

time_s_hours <- c(NA)
temp = 0
#convert all to hours
for(i in time_s){
        temp = temp +1
        all_dash <- unlist(gregexpr(pattern ='/',i))
        space_pos <- unlist(gregexpr(pattern =' ',i))
        last <- unlist(gregexpr(pattern =':',i))
        
        month <- as.numeric(substr(i,0,all_dash[1]-1)) * 30 * 24 * 60
        day <- as.numeric(substr(i,all_dash[1]+1,all_dash[2]-1)) * 24 * 60
        year <- as.numeric(substr(i, all_dash[2]+1, space_pos-1)) *365 * 24 * 60
        hours <- as.numeric(substr(i, space_pos+1,last - 1))* 60
        mins <- as.numeric(substr(i,last[1]+1,last[2]-1))
        totalhours <- year + month + day + hours
        time_s_hours[temp] <- totalhours
        print(temp)
}

time_e_hours <- c(NA)
temp = 0
#convert all to hours
for(i in time_e){
        temp = temp +1
        all_dash <- unlist(gregexpr(pattern ='/',i))
        space_pos <- unlist(gregexpr(pattern =' ',i))
        last <- unlist(gregexpr(pattern =':',i))
        
        month <- as.numeric(substr(i,0,all_dash[1]-1)) * 30 * 24 * 60
        day <- as.numeric(substr(i,all_dash[1]+1,all_dash[2]-1)) * 24 * 60
        year <- as.numeric(substr(i, all_dash[2]+1, space_pos-1)) *365 * 24 * 60
        hours <- as.numeric(substr(i, space_pos+1,last - 1))* 60
        mins <- as.numeric(substr(i,last[1]+1,last[2]-1))
        totalhours <- year + month + day + hours
        time_e_hours[temp] <- totalhours
        print(temp)
}

duration <- time_e_hours - time_s_hours
duration_test <- na.omit(duration)

summary(duration)
hist(duration_test[duration_test>0],breaks = 40)
class(duration)


timedf <- as.data.frame(cbind(time_s,time_e))

timedf$duration <- duration


xx <- na.omit(duration_test)

lh <- quantile(duration_test[duration_test>0 ], prob = 0.10)
lh
uh <- quantile(duration_test[duration_test>0 ], prob = 0.85)
uh
duration
duration_test2 <- duration_test[duration_test>0 ]
duration_test2 <- duration_test2[duration_test2 >= lh & duration_test2 <= uh]
hist(duration_test2,breaks = 60)


duration_test2 <- as.data.frame(duration_test2)
library(ggplot2)
library(ggplot2)
durationOfStroms <- duration_test2
summary(duration_test2$duration_test2)
ggplot(durationOfStroms,aes(x= duration_test2)) + geom_histogram(aes(y=..density..),binwidth = 200, colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(duration_test2)),
                                                                              color="blue", linetype="dashed", size=1)+
        ggtitle("The Storms' Duration Distribution(Mins)")

write.csv()
hist(xx)
timedf$loss <- as.numeric(general_data$DAMAGE_CROPS) + as.numeric(general_data$DAMAGE_PROPERTY)
timedf$loss[timedf$loss >0 ] <-1
timedf$month <- as.factor(general_data2$MONTH_NAME)

plot_timedf <- timedf[timedf$duration > 0, ]
plot_timedf <- plot_timedf[plot_timedf$duration >= lh & plot_timedf$duration <= uh,  ]
plot_timedf <- plot_timedf[complete.cases(plot_timedf),]
plot_timedf$loss <- as.numeric(plot_timedf$loss)
plot_timedf$loss[plot_timedf$loss == 0] <- "No Damage Caused"
plot_timedf$loss[plot_timedf$loss == 1] <- "Damage Caused"
plot_timedf$loss <- as.factor(plot_timedf$loss)
ggplot(plot_timedf,aes(x= duration, fill = loss)) + geom_histogram(aes(y=..density..),binwidth = 100)+
        geom_density(alpha=.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(duration)),color="blue", linetype="dashed", size=1)+
        ggtitle("The Storms' Duration Distribution(Mins)")


month_order <- c("January","February","March","April","May","June","July","August","September","October","November","December")
month_seq <- c(1:12)

month <- as.data.frame(cbind(month_order, month_seq))

class(general_data$MONTH_NAME)
summary(general_data$MONTH_NAME)
month <- as.character(plot_timedf$month)
month <- trimws(month)
plot_timedf$month <- month

plot_timedf$month <- as.factor(plot_timedf$month)
plot_timedf$month <- as.character(plot_timedf$month)
plot_timedf <- merge(plot_timedf, month, by.x = "month", by.y = "month_order",all.x = T)
plot_timedf$month_seq <- as.factor(plot_timedf$month_seq)

colnames(plot_timedf) <- c("month_Eng" ,    "time_s" ,   "time_e"   , "duration",  "loss"   ,   "month")
ggplot(plot_timedf,aes(x= month,fill = loss)) + geom_bar()
        

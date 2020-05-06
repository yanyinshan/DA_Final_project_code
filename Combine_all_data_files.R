# this script is to test the logic. 
# pivot table

rm(list = ls())

library(dplyr)
library(tidyr)

library(choroplethr)
library(choroplethrMaps)
library(installr)
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data\\Lab\\Data")
files <- list.files()
all_data <- read.csv('Stormdata_1996.csv')
for (i in files) {
        if(i == "Stormdata_1996.csv"){
                print('pass')
        }
        else{
                xx <- read.csv(i)
                xx <- as.data.frame(xx)
                all_data <- rbind(all_data, xx)
                print(i)
        }
        
}

xx <- as.data.frame(summary(all_data$STATE))
xx

sum(xx)
colnames(file2004)
colnames(all_data)
file2004 <- read.csv('Stormdata_2004.csv')
xx <- summary(file2004$STATE)
xx
print("hi")


write.csv(all_data,"all_data.csv")

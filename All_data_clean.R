# this script is to test the logic. 
# pivot table

rm(list = ls())

library(dplyr)
library(tidyr)

library(choroplethr)
library(choroplethrMaps)
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data")
files <- list.files()
files <- files[6:23]
files
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
# this is what variables we need
general_data <- all_data[,c(7,8,9,10,13,16,18,20,21,22,23,24,25,26,28,29,45,46,47,48)]


# clean the damage_property and crop column
#general_data_test <- general_data[c(1:10000),]
damage_property <- general_data$DAMAGE_PROPERTY

damage_property[damage_property == ""] <- 0
damage_property <- as.character(damage_property)
temp <- 0
for(i in damage_property){
        temp = temp + 1
        text <- as.character(i)
        text_last <- substr(text,nchar(text),nchar(text))
        if(text_last == "K"){
                value_text <- as.numeric(substr(text,0,nchar(text)-1)) * 1000
                damage_property[temp] <- value_text
        }
        else{
                if(text_last == "M"){
                        value_text <- as.numeric(substr(text,0,nchar(text)-1)) * 1000000
                        damage_property[temp] <- value_text
                }
                else{
                        print(i)
                }
        }
        print(temp)
}

damage_crop <- as.character(general_data$DAMAGE_CROPS)
damage_crop[damage_crop ==""] <- 0

temp <- 0
for(i in damage_crop){
        temp = temp + 1
        text <- as.character(i)
        text_last <- substr(text,nchar(text),nchar(text))
        if(text_last == "K"){
                value_text <- as.numeric(substr(text,0,nchar(text)-1)) * 1000
                damage_crop[temp] <- value_text
        }
        else{
                if(text_last == "M"){
                        value_text <- as.numeric(substr(text,0,nchar(text)-1)) * 1000000
                        damage_crop[temp] <- value_text
                }
                else{
                        print(i)
                }
        }
        print(temp)
}

#xx <- cbind(damage_property,as.character(general_data$DAMAGE_PROPERTY))

#xx <- cbind(damage_crop,as.character(general_data$DAMAGE_CROPS))
#xx


general_data$DAMAGE_PROPERTY <- as.numeric(damage_property)
general_data$DAMAGE_CROPS <- as.numeric(damage_crop)


write.csv(general_data,"General_data.csv")

complete_data <- general_data[complete.cases(general_data),]

rm(list = ls())


rm(list = ls())
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data")

list.files()
all_data <- read.csv("all_data.csv")

summary(all_data$STATE)
colnames(all_data)
general_data <- all_data

# clean data
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

##xx <- cbind(damage_property,as.character(general_data$DAMAGE_PROPERTY))

##xx <- cbind(damage_crop,as.character(general_data$DAMAGE_CROPS))
##xx


general_data$DAMAGE_PROPERTY <- as.numeric(damage_property)
general_data$DAMAGE_CROPS <- as.numeric(damage_crop)
colnames(general_data)

##c(8,9,10,11,12,13,19,21,22,23,24,25,26,27,29,46,47,48,49)
general_data2 <- general_data[,c(8,9,10,11,12,13,19,21,22,23,24,25,26,27,29,46,47,48,49)]
##write.csv(general_data,"general_data.csv")

# plot


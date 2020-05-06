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



names(all_data)
general_data <- all_data[,c(1:24)]

states <- read.csv("States_abbri.csv", header = T)
colnames(states) <- c("States", "Abbreviation")

data_new <- as.data.frame(cbind(as.character(general_data$STATE), general_data$EVENT_ID))
data_new$v3 <- general_data$CZ_NAME
colnames(data_new) <- c("states","ID","CZ_Name")
new <- merge(data_new,states, all.x = T,by.x = "states", by.y = "States", )

new_combine <- cbind(new,paste(new$Abbreviation,toupper(new$CZ_Name)))
temp <- names(new_combine)
temp[5] <- 'state_county'
colnames(new_combine) <- temp
county_frequency_raw <- new_combine %>% 
        select(state_county,ID,Abbreviation) %>%
        group_by(state_county) %>%
        summarise(NumofStorms = length(ID))

county_frequency_raw$state <- substr(county_frequency_raw$state_county, start = 1, stop = 2)

number_of_county <- county_frequency_raw %>%
        select(state) %>%
        group_by(state) %>%
        summarise(Numofcounty = length(state))
# second part
data1 <- read.csv('FIPS.csv',header = F)
data1$V4 <- toupper(paste(data1$V3,data1$V2))
data1$V1 <- as.character(data1$V1)
data1[1,1] <- c('1001')


#merge
map_data <- merge(county_frequency_raw, data1, by.x = "state_county",by.y ="V4",all.x = T )

map_data1 <- map_data[complete.cases(map_data),]

mapdata2 <- as.data.frame(cbind(map_data1$V1,map_data1$NumofStorms))

data(df_pop_county)

df_pop_county$value <- 0
df_pop_county <- merge(df_pop_county, mapdata2,by.x = "region",by.y = "V1",all.x = T)
df_pop_county$value <- df_pop_county$V2

df_pop_county <- df_pop_county[,c(1:2)]
df_pop_county$value <- as.numeric(df_pop_county$value)
df_pop_county[is.na(df_pop_county)] <- 0

States_frequency <- general_data %>%
        select(STATE,EVENT_ID) %>%
        group_by(STATE) %>%
        summarise(NumofStorms = length(EVENT_ID))


on_map_record <- map_data1 %>%
        select(NumofStorms,V3) %>%
        group_by(V3) %>%
        summarise(sum(NumofStorms))


States_frequency <- merge(States_frequency, states,by.x = 'STATE',by.y = 'States')
States_frequency <- merge(States_frequency,on_map_record, by.x = 'Abbreviation', by.y = 'V3')
temp <- names(States_frequency)
temp[4] <- 'on_map'
colnames(States_frequency) <- temp
States_frequency$no_map <- States_frequency$NumofStorms - States_frequency$on_map
States_frequency <- merge(States_frequency,number_of_county, by.x = "Abbreviation", by.y = "state")
States_frequency$no_map_avg = floor(States_frequency$no_map / States_frequency$Numofcounty)


add_avg <- as.data.frame(cbind(States_frequency$Abbreviation,States_frequency$no_map_avg))
add_avg$V1 <- States_frequency$Abbreviation

map_data1 <- merge(map_data1, add_avg,by.x = "V3",add_avg,by.y = "V1")
map_data1$total <- map_data1$NumofStorms+map_data1$V2.y

mapdata2 <- as.data.frame(cbind(map_data1$V1,map_data1$total))

df_pop_county$value <- 0
df_pop_county <- merge(df_pop_county, mapdata2,by.x = "region",by.y = "V1",all.x = T)
df_pop_county$value <- df_pop_county$V2

df_pop_county <- df_pop_county[,c(1:2)]
df_pop_county$value <- as.numeric(df_pop_county$value)
df_pop_county[is.na(df_pop_county)] <- 0
temp <- dim(df_pop_county)[1]
state_num <- c(NA)
for(i in 1:temp){
        if(nchar(df_pop_county[i,1]) ==4){
                state_num[i] = substr(df_pop_county[i,1], start = 1, stop = 1)
        }
        else{
                state_num[i] = substr(df_pop_county[i,1], start = 1, stop = 2)
        }
}
df_pop_county$state_num <- state_num

state_code_num <- general_data %>%
        select(STATE,STATE_FIPS) %>%
        group_by(STATE)

state_code_num <- unique.data.frame(state_code_num)

state_num <- merge(df_pop_county, state_code_num, by.x ="state_num", by.y = "STATE_FIPS", all.x = T )
add_avg <- merge(add_avg, states, by.x = "V1", by.y = "Abbreviation")
state_num <- merge(state_num, add_avg, by.x = "STATE",by.y = "States", all.x = T)
df_pop_county <- as.data.frame(cbind(state_num$region,state_num$new_value))
colnames(df_pop_county) <- c("region","value")
# plot

county_choropleth(df_pop_county)

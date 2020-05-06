# this script is to test the logic. 
rm(list = ls())
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data")
files <- list.files()
files

all_data <- read.csv('Stormdata_1996.csv')


names(all_data)
general_data <- all_data[,c(1:24)]

# month

hist(all_data$BEGIN_YEARMONTH,breaks = 12)
hist(all_data$BEGIN_TIME, breaks = 24)
##### anormally high frequence on  day1 or time 0, this may indicates that the time is misrecorded. 

# choropleth map URL: "https://rstudio-pubs-static.s3.amazonaws.com/324400_69a673183ba449e9af4011b1eeb456b9.html"

# pivot table
library(dplyr)
library(tidyr)

class(general_data$EVENT_ID)
# frequency by states choropleth map
States_frequency <- general_data %>%
        select(STATE,EVENT_ID) %>%
        group_by(STATE) %>%
        summarise(NumofStorms = length(EVENT_ID))

barplot(States_frequency$NumofStorms, names.arg = States_frequency$STATE)

Magnitude <- general_data %>%
        select(MAGNITUDE,STATE) %>%
        group_by(MAGNITUDE) %>%
        summarise(Magnitude_ = length(STATE))


colnames(States_frequency) <- c('region','value')
state_names <- tolower(States_frequency$region)
States_frequency$region <- state_names
state_choropleth(States_frequency, title = "Storms", legend = "", num_colors = 9,
                 zoom = NULL, reference_map = FALSE)


# county maps
#first find the county frequency
county_dataset <- cbind(general_data$EVENT_ID, paste(general_data$STATE, general_data$CZ_NAME) )
general_data <- cbind(general_data, paste(general_data$STATE, general_data$CZ_NAME))

temp <- names(general_data)
temp[25] <- "Unique_CZ"
colnames(general_data) <- temp

county_frequency_raw <- general_data %>% 
        select(Unique_CZ,EVENT_ID) %>%
        group_by(Unique_CZ) %>%
        summarise(NumofStorms = length(EVENT_ID))
        
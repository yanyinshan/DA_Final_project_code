rm(list = ls())
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data\\Lab\\Data")
files <- list.files()
files_2 <-files[-c(1:7)]
files_2 <-files_2[-2]
files <- files[1:7]
all_data_part1 <- read.csv('Stormdata_1996.csv')
for (i in files) {
        if(i == "Stormdata_1996.csv"){
                print('pass')
        }
        else{
                xx <- read.csv(i,header = TRUE)
                xx <- as.data.frame(xx)
                all_data_part1 <- rbind(all_data_part1, xx)
                print(i)
        }
        
}
all_data_part2 <- read.csv('Stormdata_2003.csv')
for (i in files_2) {
        if(i == "Stormdata_2003.csv"){
                print('pass')
        }
        else{
                xx <- read.csv(i,header = TRUE)
                xx <- as.data.frame(xx)
                all_data_part2  <- rbind(all_data_part2, xx)
                print(i)
        }
        
}


states <- summary(all_data_part2$STATE)
states
# slicing the data
names(all_data)


# 2004 data has problems

# choropleth map URL: "https://rstudio-pubs-static.s3.amazonaws.com/324400_69a673183ba449e9af4011b1eeb456b9.html"
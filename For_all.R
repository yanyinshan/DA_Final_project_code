rm(list = ls())
setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data\\Lab\\Data")
files <- list.files()

files
all_data <- read.csv('Stormdata_1996.csv')
for (i in files) {
        if(i == "Stormdata_1996.csv"){
                print('pass')
        }
        else{
                xx <- read.csv(i,header = TRUE)
                xx <- as.data.frame(xx)
                all_data <- rbind(all_data, xx)
                print(i)
        }
        
}

# slicing the data
names(all_data)

# choropleth map URL: "https://rstudio-pubs-static.s3.amazonaws.com/324400_69a673183ba449e9af4011b1eeb456b9.html"
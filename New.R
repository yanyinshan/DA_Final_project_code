setwd("C:\\Users\\Yinshan\\Desktop\\Study\\RPI\\2020 SPRING\\ITWS 6600 Data Analytics\\Project1\\Data\\Lab")

list.files()
all_data <- read.csv("General_data.csv")

complete_data <- na.omit(all_data)
complete_data <- complete_data[,-17]

#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html


states <- as.data.frame(summary(all_data$STATE))
states



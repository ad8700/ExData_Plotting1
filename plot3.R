#Import Libraries
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)


#If data directory does not exist, create it

if(!dir.exists("./data")) {
  dir.create("./data")
} else {
  print("Data directory already exists")
}

#download the data to the data directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dest <- "./data/household_power_consumption.zip"
download.file(fileUrl, dest)

#unzip the file
unzip(dest, exdir="./data")

#Read in the data from file
raw_df <- read.table(file.choose(), 
                     sep= ";", 
                     header=TRUE, 
                     na.strings="?", 
                     stringsAsFactors=FALSE, 
                     dec=".")
#head(raw_df)
#sapply(raw_df, class)

#convert the Date and Time variables to Date/Time classes
raw_df$Date <- with(raw_df, dmy(Date))
raw_df$Dt <- with(raw_df, ymd(Date) + hms(Time))

#Only use data from the dates 2007-02-01 and 2007-02-02
filtered_df <- filter(raw_df, Date >= '2007-02-01', Date <= '2007-02-02')

#Create a day variable for the day name and IDs for each row
filtered_df$day <- wday(filtered_df$Date, label=TRUE)
filtered_df$id <- 1:nrow(filtered_df)
#head(filtered_df)

#Create a plot of the 3 sub_metering variables
#with ggplot

ggplot(data=filtered_df, aes(x=Dt)) +
  geom_line(y=filtered_df$Sub_metering_1, size=0.15, color="grey") +
  geom_line(y=filtered_df$Sub_metering_2, size=0.15, color="red") +
  geom_line(y=filtered_df$Sub_metering_3, size=0.15, color="blue") +
  ylab("Energy sub metering") +
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=5),
        #axis.title=element_text(size=5),
        legend.box.just = "right",
        legend.justification=c("right","top")) +
  ylim(0,40)

setwd("C:/Users/andrew.domenico/git/ExData_Plotting1")
ggsave('plot3.png', width=480, height=480, units="px")
dev.off()
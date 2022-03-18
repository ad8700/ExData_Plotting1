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
raw_df$Date <- as.Date(raw_df$Date, format = '%d/%m/%Y')
raw_df$Dt <- with(raw_df, ymd(Date) + hms(Time))
raw_df$Global_active_power <- as.numeric(raw_df$Global_active_power)
raw_df$day <- weekdays(raw_df$Date)
head(raw_df)
#sapply(raw_df, class)

#Only use data from the dates 2007-02-01 and 2007-02-02
filtered_df <- filter(raw_df, Date >= '2007-02-01', Date <= '2007-02-02')
#head(filtered_df)
#dim(filtered_df)
#table(filtered_df$Date)

#Create a line graph with Global Active Power (kilowatts) as the y axis and
#horizontal axis by day, using day names

ggplot(data=filtered_df, aes(x=Dt, y=Global_active_power)) +
  geom_line(size = 0.25) + 
  labs(y="Global Active Power (kilowats)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
setwd("C:/Users/andrew.domenico/git/ExData_Plotting1")
ggsave('plot2.png', width=480, height=480, units="px", dpi=100)
dev.off()
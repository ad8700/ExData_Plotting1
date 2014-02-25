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
raw_df <- read.delim(file.choose(), sep= ";")
#head(raw_df)
#sapply(raw_df, class)

#convert the Date and Time variables to Date/Time classes
raw_df$Date <- as.Date(raw_df$Date, format = '%d/%m/%Y')
raw_df$Time <- strptime(raw_df$Time, format= '%H:%M:%S')
raw_df$Global_active_power <- as.numeric(raw_df$Global_active_power)
#sapply(raw_df, class)

#Only use data from the dates 2007-02-01 and 2007-02-02
filtered_df <- filter(raw_df, Date >= '2007-02-01', Date <= '2007-02-02')
#head(filtered_df)
#dim(filtered_df)

#Create a PNG histogram of Global Active Power (kilowatts)
ggplot(filtered_df, aes(Global_active_power),binwidth=12, color="red") + 
  geom_histogram(bins=30, fill="red") +
  ggtitle("Global Active Power") + 
  theme(plot.title=element_text(hjust=0.5)) +
  labs(x="Global Active Power (kilowats)",
       y="Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))
setwd("C:/Users/andrew.domenico/git/ExData_Plotting1")
ggsave('plot1.png', width=480, height=480, units="px", dpi=100)
dev.off()
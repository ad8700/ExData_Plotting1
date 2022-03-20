#Import Libraries
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)


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

#Create a 4 in 1 graph of Global Active Power, Voltage, Energy sub metering
# and Global_reactive_power
p1 <- ggplot(data=filtered_df, aes(x=Dt, y=Global_active_power)) +
  geom_line(size = 0.25) + 
  labs(y="Global Active Power (kilowats)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  theme_gray(base_size=5)

p2 <-ggplot(data=filtered_df, aes(x=Dt, y=Voltage)) + 
     geom_line(size=0.25) +
     xlab("Datetime") + 
     ylim(234, 246) +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(color = "black")) +
  theme_gray(base_size=5)


p3 <- ggplot(data=filtered_df, aes(x=Dt)) +
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
  ylim(0,40) + 
  theme_gray(base_size=5)

p4 <- ggplot(data=filtered_df, aes(x=Dt, y=Global_reactive_power)) +
  geom_line(size = 0.25) + 
  labs(y="Global_reactive_power", x="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  theme_gray(base_size=5)


#Once all 4 plots are created, arrange to a grid
p5 <- grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
p5

setwd("C:/Users/andrew.domenico/git/ExData_Plotting1")
ggsave(filename = 'plot4.png', plot = p5, width=480, height=480, units="px")
dev.off()
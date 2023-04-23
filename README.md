# Exploratory-Data-Analysis-Week-1-Course-Project-1
Exploratory Data Analysis Week 1 Course Project 1

install.packages("ggpubr")

# Packages
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggpubr)

# Data
household_power_consumption_0 <- read_delim("household_power_consumption.CSV",
                                          delim = ";", escape_double = FALSE, 
                                          trim_ws = TRUE)

household_power_consumption_1 <- household_power_consumption_0

mutate(household_power_consumption_1,
       parse_date_time(household_power_consumption_1$Date,
                       "dmy"))


household_power_consumption_1 <-  mutate(household_power_consumption_1,
       dmy(household_power_consumption_1$Date))

household_power_consumption_1 <-  rename(household_power_consumption_1, 
       newdate = `dmy(household_power_consumption_1$Date)`)


household_power_consumption_2 <- subset(household_power_consumption_1,
                                        household_power_consumption_1$newdate >= "2007-02-01" & 
                                          household_power_consumption_1$newdate <= "2007-02-02")

household_power_consumption_2$Global_active_power <- as.numeric(household_power_consumption_2$Global_active_power)


# Plot 1
plot1.png <- ggplot(household_power_consumption_2,
                    aes(x=household_power_consumption_2$Global_active_power)) +
  geom_histogram(fill="red",col="black",linewidth=0.5,
                 bins = 18) +
  labs(x ="Global Active Power", 
       y = "Frequency",
       title = "Plot 1 : Global Active Power") + 
  theme(plot.title = element_text(hjust = 0.5))

plot1.png 


# Plot 2
household_power_consumption_3 <- unite(data = household_power_consumption_2,
                                      Date, Time, col = "Time",
                                      sep = " ")



plot2_1.png <- ggplot(household_power_consumption_3,
                    aes(x = dmy_hms(household_power_consumption_3$Time),
                        y = Global_active_power)) + 
  geom_line() + 
  labs(y = "Global Active Power (Kilowatts",
       x  = "datetime",
       title = "Plot 2_1")+ 
  theme(plot.title = element_text(hjust = 0.5))

plot2_1.png 


# Plot 3 
plot3_3.png <- ggplot(household_power_consumption_3) + 
  geom_line( aes(x = dmy_hms(household_power_consumption_3$Time),
                 y = as.numeric(household_power_consumption_3$Sub_metering_1),
                 col = "black")) +
  geom_line( aes(x = dmy_hms(household_power_consumption_3$Time),
                  y = as.numeric(household_power_consumption_3$Sub_metering_2),
                  col = "red")) +
  geom_line( aes(x = dmy_hms(household_power_consumption_3$Time),
                  y = as.numeric(household_power_consumption_3$Sub_metering_3),
                  col = "blue")) +
  labs(y = "Global Active Power (Kilowatts",
       x  = "datetime",
       title = "Plot 3") +
    theme(plot.title = element_text(hjust = 0.5))

plot3_3.png 

# Plot 4

par(mfrow=c(2,2))



plot4_2.png <- ggplot(household_power_consumption_4,
                    aes(x = dmy_hms(household_power_consumption_4$Time),
                       y = as.numeric(household_power_consumption_4$Voltage))) + 
  geom_line() + 
  labs(y = "Voltage",
       x  = "datetime",
       title = "Plot 4_2")+ 
  theme(plot.title = element_text(hjust = 0.5))

plot4_2.png 


plot4_4.png <- ggplot(household_power_consumption_4,
                      aes(x = dmy_hms(household_power_consumption_4$Time),
                          y = as.numeric(household_power_consumption_4$Global_reactive_power))) + 
  geom_line() + 
  labs(y = "Global_reactive_power",
       x  = "datetime",
       title = "Plot 4_4")+ 
  theme(plot.title = element_text(hjust = 0.5))

plot4_4.png 

ggarrange(plot2_1.png, plot4_2.png, plot3_3.png, plot4_4.png)







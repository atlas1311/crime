#Dependencies
library(ggplot2)
library(plyr)

# Read in data
setwd("~/crime")
arrestRaw <- read.csv("BPD_Arrests.csv")

# Clean Data
arrestRaw$District <- as.factor(toupper(arrestRaw$District))
arrestRaw$District[arrestRaw$District == ""] <- NA
arrestRaw$ArrestDate <- as.character(arrestRaw$ArrestDate)
arrestRaw$ArrestDate <- as.Date(arrestRaw$ArrestDate, format = "%m/%d/%Y")
arrestRaw$ArrestMonth <- format(arrestRaw$ArrestDate, format = "%Y/%m")

# Summary Histogram of arrests by District
ggplot(data.frame(na.omit(arrestRaw)), aes(x = District)) +
        geom_histogram(fill = "#0066FF") +
        labs(title = "Arrests in Baltimore", x = "District", y = "Total") +
        stat_bin(geom = "text", aes(label = ..count.., vjust = -1))

# Arrest Totals by Month
arrestMonthFreq <- as.data.frame(table(arrestRaw$ArrestMonth))
colnames(arrestMonthFreq) <- c("Date", "Total")
arrestMonthFreq$Date <- as.character(arrestMonthFreq$Date)

ggplot(arrestMonthFreq, aes(x = Date, y = Total)) +
        geom_line(position = "identity", aes(group = 1)) +
        labs(title = "Arrests in Baltimore (Jan 2013 - June 2015)", x = "Month", y = "Total Monthly Arrests") +
        stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))

# Arrest Totals by District by Month

# Crime Data
crimeRaw <- read.csv("BPD_Part_1_Victim_Based_Crime_Data.csv")

# Clean the Crime Data
crimeRaw$District[crimeRaw$District == ""] <- NA
crimeRaw$CrimeDate <- as.Date(as.character(crimeRaw$CrimeDate), format = "%m/%d/%Y")
crimeRaw$crimeMonth <- format(crimeRaw$CrimeDate, format = "%Y/%m")

# Summary Histogram of Crime Type
ggplot(data.frame(na.omit(crimeRaw)), aes(x = Description)) +
        geom_histogram(fill = "#0066FF") +
        labs(title = "Crime in Baltimore (Jan 2010 - June 2015)", x = "Crime", y = "Total") +
        stat_bin(geom = "text", aes(label = ..count.., vjust = -1))

# Homicides by month
homicide <- count(crimeRaw, c("CrimeDate", "Description"))



#Dependencies
library(ggplot2)

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

# Arrest Totals by year
arrestMonthFreq <- as.data.frame(table(arrestRaw$ArrestMonth))
colnames(arrestMonthFreq) <- c("Date", "Total")
arrestMonthFreq$Date <- as.character(arrestMonthFreq$Date)


ggplot(arrestMonthFreq, aes(x = Date, y = Total)) +
        geom_line(position = "identity", aes(group = 1))








#Dependencies
library(ggplot2)
library(plyr)
library(gridExtra)

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
crimeHist <- ggplot(data.frame(na.omit(arrestRaw)), aes(x = District)) +
        geom_histogram(fill = "#0066FF") +
        labs(title = "Arrests in Baltimore", x = "District", y = "Total") +
        stat_bin(geom = "text", aes(label = ..count.., vjust = -1))
crimeHist

# Arrest Totals by Month
arrestMonthFreq <- as.data.frame(table(arrestRaw$ArrestMonth))
colnames(arrestMonthFreq) <- c("Date", "Total")
arrestMonthFreq$Date <- as.character(arrestMonthFreq$Date)

crimeMonth <- ggplot(arrestMonthFreq, aes(x = Date, y = Total)) +
        geom_line(position = "identity", aes(group = 1)) +
        labs(title = "Arrests in Baltimore (Jan 2013 - June 2015)", x = "Month", y = "Total Monthly Arrests") +
        stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
crimeMonth

# District variables
western <- na.omit(arrestRaw[arrestRaw$District == "WESTERN", ])
eastern <- na.omit(arrestRaw[arrestRaw$District == "EASTERN", ])
central <- na.omit(arrestRaw[arrestRaw$District == "CENTRAL", ])
northeastern <- na.omit(arrestRaw[arrestRaw$District == "NORTHEASTERN", ])
northwestern <- na.omit(arrestRaw[arrestRaw$District == "NORTHWESTERN", ])
southeastern <- na.omit(arrestRaw[arrestRaw$District == "SOUTHEASTERN", ])
southern <- na.omit(arrestRaw[arrestRaw$District == "SOUTHERN", ])
southwestern <- na.omit(arrestRaw[arrestRaw$District == "SOUTHWESTERN", ])
northern <- na.omit(arrestRaw[arrestRaw$District == "NORTHERN", ])

# Plot by district by month - Western
westernFreq <- as.data.frame(table(western$ArrestMonth))
colnames(westernFreq) <- c("Date", "Total")
westernFreq$Date <- as.character(westernFreq$Date)

westernMonth <- ggplot(westernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Western", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
westernMonth

# Plot by district by month - Eastern
easternFreq <- as.data.frame(table(eastern$ArrestMonth))
colnames(easternFreq) <- c("Date", "Total")
easternFreq$Date <- as.character(easternFreq$Date)

easternMonth <- ggplot(easternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Eastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
easternMonth

# Plot by district by month - Central
centralFreq <- as.data.frame(table(central$ArrestMonth))
colnames(centralFreq) <- c("Date", "Total")
centralFreq$Date <- as.character(centralFreq$Date)

centralMonth <- ggplot(centralFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Central", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
centralMonth

# Plot by district by month - Northeastern
northeasternFreq <- as.data.frame(table(northeastern$ArrestMonth))
colnames(northeasternFreq) <- c("Date", "Total")
northeasternFreq$Date <- as.character(northeasternFreq$Date)

northeasternMonth <- ggplot(northeasternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Northeastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
northeasternMonth

# Plot by district by month - Northwestern
northwesternFreq <- as.data.frame(table(northwestern$ArrestMonth))
colnames(northwesternFreq) <- c("Date", "Total")
northwesternFreq$Date <- as.character(northwesternFreq$Date)

northwesternMonth <- ggplot(northwesternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Northwestern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
northwesternMonth

# Plot by district by month - Southeastern
southeasternFreq <- as.data.frame(table(southeastern$ArrestMonth))
colnames(southeasternFreq) <- c("Date", "Total")
southeasternFreq$Date <- as.character(southeasternFreq$Date)

southeasternMonth <- ggplot(southeasternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Southeastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
southeasternMonth

# Plot by district by month - Southern
southernFreq <- as.data.frame(table(southern$ArrestMonth))
colnames(southernFreq) <- c("Date", "Total")
southernFreq$Date <- as.character(southernFreq$Date)

southernMonth <- ggplot(southernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Southern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
southernMonth

# Plot by district by month - Southwesten
southwesternFreq <- as.data.frame(table(southwestern$ArrestMonth))
colnames(southwesternFreq) <- c("Date", "Total")
southwesternFreq$Date <- as.character(southwesternFreq$Date)

southwesternMonth <- ggplot(southwesternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Southwestern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
southwesternMonth

# Plot by district by month - Northern
northernFreq <- as.data.frame(table(northern$ArrestMonth))
colnames(northernFreq) <- c("Date", "Total")
northernFreq$Date <- as.character(northernFreq$Date)

northernMonth <- ggplot(northernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Northern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
northernMonth


grid.arrange(westernMonth, easternMonth, centralMonth, southernMonth, ncol = 2)
grid.arrange(northwesternMonth, northeasternMonth, southwesternMonth, southeasternMonth, ncol = 2)
grid.arrange(southernMonth, northernMonth, ncol = 1)


##### Crime Data #####
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
homicide <- count(crimeRaw, c("CrimeDate", crimeRaw$Description == "HOMICIDE"))

# Crime
crimeRaw <- read.csv("BPD_Part_1_Victim_Based_Crime_Data.csv")

# Clean Data
crimeRaw$District <- as.factor(toupper(crimeRaw$District))
crimeRaw$District[crimeRaw$District == ""] <- NA
crimeRaw$CrimeDate <- as.character(crimeRaw$CrimeDate)
crimeRaw$CrimeDate <- as.Date(crimeRaw$CrimeDate, format = "%m/%d/%Y")
crimeRaw$CrimeMonth <- as.Date(crimeRaw$CrimeDate, format = "%Y/%m")








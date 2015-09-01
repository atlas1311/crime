#Install
install.packages(c("PerformanceAnalytics", "ggthemes"))

#Dependencies
library(ggplot2)
library(plyr)
library(gridExtra)
library(ggthemes)

# Read in data
# https://www.baltimorepolice.org/bpd-open-data
setwd("~/crime")
arrestRaw <- read.csv("BPD_Arrests.csv")

# Clean Data
arrestRaw$District <- as.factor(toupper(arrestRaw$District))
arrestRaw$District[arrestRaw$District == ""] <- NA
arrestRaw$ArrestDate <- as.character(arrestRaw$ArrestDate)
arrestRaw$ArrestDate <- as.Date(arrestRaw$ArrestDate, format = "%m/%d/%Y")
arrestRaw$ArrestMonth <- format(arrestRaw$ArrestDate, "%Y-%m")

# Summary Histogram of arrests by District
crimeHist <- ggplot(data.frame(na.omit(arrestRaw)), aes(x = District)) +
        geom_histogram(fill = "#0066FF") +
        labs(title = "Arrests in Baltimore: 1 JAN 13 - 22 AUG 15", x = "District", y = "Total") +
        stat_bin(geom = "text", aes(label = ..count.., vjust = -1))
crimeHist

# Arrest Totals by Month
arrestMonthFreq <- as.data.frame(table(arrestRaw$ArrestMonth))
colnames(arrestMonthFreq) <- c("Date", "Total")
arrestMonthFreq$Date <- as.character(arrestMonthFreq$Date)

crimeMonth <- ggplot(arrestMonthFreq, aes(x = Date, y = Total)) +
        geom_line(position = "identity", aes(group = 1)) +
        labs(title = "Arrests in Baltimore (Jan 2013 - Aug 2015)", x = "Month", y = "Total Monthly Arrests") +
        stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
crimeMonth

# Plot by district by month - Northern
northernArrest <- na.omit(subset(arrestRaw, District == "NORTHERN"))
northernFreq <- as.data.frame(table(northernArrest$ArrestMonth))
colnames(northernFreq) <- c("Date", "Total")
northernFreq$Date <- as.character(northernFreq$Date)

northernMonth <- ggplot(northernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Northern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
northernMonth

# Plot by district by month - Southern
southernArrest <- na.omit(subset(arrestRaw, District == "SOUTHERN"))
southernFreq <- as.data.frame(table(southernArrest$ArrestMonth))
colnames(southernFreq) <- c("Date", "Total")
southernFreq$Date <- as.character(southernFreq$Date)

southernMonth <- ggplot(southernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  labs(title = "Arrests by Month in the Southern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
southernMonth

# Plot by district by month - Eastern
easternArrest <- na.omit(subset(arrestRaw, District == "EASTERN"))
easternFreq <- as.data.frame(table(easternArrest$ArrestMonth))
colnames(easternFreq) <- c("Date", "Total")
easternFreq$Date <- as.character(easternFreq$Date)

easternMonth <- ggplot(easternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  labs(title = "Arrests by Month in the Eastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1))
easternMonth

# Plot by district by month - Western
westernArrest <- na.omit(subset(arrestRaw, District == "WESTERN"))
westernFreq <- as.data.frame(table(westernArrest$ArrestMonth))
colnames(westernFreq) <- c("Date", "Total")
westernFreq$Date <- as.character(westernFreq$Date)

westernMonth <- ggplot(westernFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  labs(title = "Arrests by Month in the Western", x = "Month", y = "Total Arrests")
westernMonth


# Plot by district by month - Central
centralArrest <- na.omit(subset(arrestRaw, District == "CENTRAL"))
centralFreq <- as.data.frame(table(centralArrest$ArrestMonth))
colnames(centralFreq) <- c("Date", "Total")
centralFreq$Date <- as.character(centralFreq$Date)

centralMonth <- ggplot(centralFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Arrests by Month in the Central", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
centralMonth

# Plot by district by month - Northeastern
northeasternArrest <- na.omit(subset(arrestRaw, District == "NORTHEASTERN"))
northeasternFreq <- as.data.frame(table(northeasternArrest$ArrestMonth))
colnames(northeasternFreq) <- c("Date", "Total")
northeasternFreq$Date <- as.character(northeasternFreq$Date)

northeasternMonth <- ggplot(northeasternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  labs(title = "Arrests by Month in the Northeastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
northeasternMonth

# Plot by district by month - Northwestern
northwesternArrest <- na.omit(subset(arrestRaw, District == "NORTHWESTERN"))
northwesternFreq <- as.data.frame(table(northwesternArrest$ArrestMonth))
colnames(northwesternFreq) <- c("Date", "Total")
northwesternFreq$Date <- as.character(northwesternFreq$Date)

northwesternMonth <- ggplot(northwesternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350)) +
  labs(title = "Arrests by Month in the Northwestern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
northwesternMonth

# Plot by district by month - Southeastern
southeasternArrest <- na.omit(subset(arrestRaw, District == "SOUTHEASTERN"))
southeasternFreq <- as.data.frame(table(southeasternArrest$ArrestMonth))
colnames(southeasternFreq) <- c("Date", "Total")
southeasternFreq$Date <- as.character(southeasternFreq$Date)

southeasternMonth <- ggplot(southeasternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  labs(title = "Arrests by Month in the Southeastern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
southeasternMonth

# Plot by district by month - Southwesten
southwesternArrest <- na.omit(subset(arrestRaw, District == "SOUTHWESTERN"))
southwesternFreq <- as.data.frame(table(southwesternArrest$ArrestMonth))
colnames(southwesternFreq) <- c("Date", "Total")
southwesternFreq$Date <- as.character(southwesternFreq$Date)

southwesternMonth <- ggplot(southwesternFreq, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  labs(title = "Arrests by Month in the Southwestern", x = "Month", y = "Total Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) +
  ylim(0, 400)
southwesternMonth

# grid.arrange of arrest data
grid.arrange(westernMonth, easternMonth, centralMonth, southernMonth, ncol = 2)
grid.arrange(northwesternMonth, northeasternMonth, southwesternMonth, southeasternMonth, ncol = 2)
grid.arrange(southernMonth, northernMonth, ncol = 1)


##### Crime Data #####
crimeRaw <- read.csv("BPD_Part_1_Victim_Based_Crime_Data.csv")

# Clean the Crime Data
crimeRaw$District[crimeRaw$District == ""] <- NA
crimeRaw$CrimeDate <- as.Date(as.character(crimeRaw$CrimeDate), format = "%m/%d/%Y")
crimeRaw$CrimeMonth <- format(crimeRaw$CrimeDate, "%Y-%m")

# Summary Histogram of Crime Type
ggplot(data.frame(na.omit(crimeRaw)), aes(x = Description)) +
        geom_histogram(fill = "#0066FF") +
        labs(title = "Crime in Baltimore (Jan 2010 - June 2015)", x = "Crime", y = "Total") +
        stat_bin(geom = "text", aes(label = ..count.., vjust = -1))
## Might have to do something here to better detail how robberies are displayed. There are 4 different types
## of robberies...Might be smart to create a new variable that is a sum-total of all robberies committed,
## omit the individual robberies from the exploratory graph, and then display the different types of robberies
## in a seperate plot all together, including the total

# Histogram of Homicides by District
homicides <- na.omit(subset(crimeRaw, Description == "HOMICIDE"))
homicideHist <- ggplot(data.frame(homicides), aes(x = District)) +
  geom_histogram(fill = "#0066FF") +
  labs(title = "Homicides in Baltimore 1 JAN 13 - 22 AUG 15", x = "District", y = "Total") +
  stat_bin(geom = "text", aes(label = ..count.., vjust = -1))
homicideHist

# Plot by district by month - Northern
northernHomicide <- subset(homicides, District == "NORTHERN")
northernHomicideFreq <- data.frame(table(northernHomicide$CrimeMonth))
colnames(northernHomicideFreq) <- c("Date", "Total")

northernHomicidePlot <- ggplot(northernHomicideFreq, aes(x = factor(Date), y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by Month in the Northern", x = "Month", y = "Total Homicides")
northernHomicidePlot

# Plot by district by month - Southern
southernHomicide <- subset(homicides, District == "SOUTHERN")
southernHomicideFreq <- as.data.frame(table(southernHomicide$CrimeMonth))
colnames(southernHomicideFreq) <- c("Date", "Total")

southernHomicidePlot <- ggplot(southernHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by Month in the Southern", x = "Month", y = "Total Homicides")
southernHomicidePlot

# Plot by district by month - Eastern
easternHomicide <- subset(homicides, District == "EASTERN")
easternHomicideFreq <- as.data.frame(table(easternHomicide$CrimeMonth))
colnames(easternHomicideFreq) <- c("Date", "Total")

easternHomicidePlot <- ggplot(easternHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by Month in the Eastern", x = "Month", y = "Total Homicides")
easternHomicidePlot

# Plot by distrcit by month - Western
westernHomicide <- subset(homicides, District == "WESTERN")
westernHomicideFreq <- as.data.frame(table(westernHomicide$CrimeMonth))
colnames(westernHomicideFreq) <- c("Date", "Total")

westernHomicidePlot <- ggplot(westernHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(breaks = c(0:13)) +
  labs(title = "Homicides by Month in the Western", x = "Month", y = "Total Homicides")
westernHomicidePlot

# Plot by district by month - Central
centralHomicide <- subset(homicides, District == "CENTRAL")
centralHomicideFreq <- as.data.frame(table(centralHomicide$CrimeMonth))
colnames(centralHomicideFreq) <- c("Date", "Total")

centralHomicidePlot <- ggplot(centralHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by Month in the Central", x = "Month", y = "Total Homicides")
centralHomicidePlot

# Plot by district by month - Northeastern
northeasternHomicide <- subset(homicides, District == "NORTHEASTERN")
northeasternHomicideFreq <- as.data.frame(table(northeasternHomicide$CrimeMonth))
colnames(northeasternHomicideFreq) <- c("Date", "Total")

northeasternHomicidePlot <- ggplot(northeasternHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by month in the Northeastern", x = "Month", y = "Total Homicides")
northeasternHomicidePlot

# Plot by district by month - Northwestern
northwesternHomicide <- subset(homicides, District == "NORTHWESTERN")
northwesternHomicideFreq <- as.data.frame(table(northwesternHomicide$CrimeMonth))
colnames(northwesternHomicideFreq) <- c("Date", "Total")

northwesternHomicidePlot <- ggplot(northwesternHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(breaks = c(0:10)) +
  labs(title = "Homicides by month in the Northwestern", x = "Month", y = "Total Homicides")
northwesternHomicidePlot

# Plot by district by month - Southeastern
southeasternHomicide <- subset(homicides, District == "SOUTHEASTERN")
southeasternHomicideFreq <- as.data.frame(table(southeasternHomicide$CrimeMonth))
colnames(southeasternHomicideFreq) <- c("Date", "Total")

southeasternHomicidePlot <- ggplot(southeasternHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Homicides by month in the Southeastern", x = "Month", y = "Total Homicides")
southeasternHomicidePlot

# Plot by district by month - Southwestern
southwesternHomicide <- subset(homicides, District == "SOUTHWESTERN")
southwesternHomicideFreq <- as.data.frame(table(southwesternHomicide$CrimeMonth))
colnames(southwesternHomicideFreq) <- c("Date", "Total")

southwesternHomicidePlot <- ggplot(southwesternHomicideFreq, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(breaks = c(0:10)) +
  labs(title = "Homicides by month in the Southwestern", x = "Month", y = "Total Homicides")
southwesternHomicidePlot

## grid.arrange for homicides













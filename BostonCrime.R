# https://data.cityofboston.gov/Public-Safety/Crime-Incident-Reports-July-2012-August-2015-Sourc/7cdf-6fgx

# Dependencies
dependent <- c("ggmap", "data.table", "reshape2", "ggplot2", "dplyr", "forecast", "quantmod",
               "tseries", "stats", "dynlm", "vars", "scales")
lapply(dependent, library, character.only = TRUE)

# ggmap citation
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1),
# 144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

# terror trends
# immigrant trends
# demographic shifts

# Set your working directory
setwd("~/crime")

# read in the data
crimeRaw <- read.csv(file = "BostonCrime.csv", header = TRUE, sep = ",")

# split off the Lat/Long coordinates
crimeBoston <- data.frame(crimeRaw, colsplit(crimeRaw$Location, pattern = "\\,", names = c("Lat", "Long")))
crimeBoston$Lat <- as.numeric(sub(pattern = "\\(", replacement = "", x = crimeBoston$Lat))
crimeBoston$Long <- as.numeric(sub(pattern = "\\)", replacement = "", x = crimeBoston$Long))
str(crimeBoston)

# Fix the date formats
crimeBoston$FROMDATE <- as.character(crimeBoston$FROMDATE)
crimeBoston$DATE <- as.Date(crimeBoston$FROMDATE, format = "%m/%d/%Y %I:%M:%S %p")
crimeBoston$MONTH <- as.Date(cut(crimeBoston$DATE, breaks = "month"))
crimeBoston$WEEK <- as.Date(cut(crimeBoston$DATE, breaks = "week", start.on.monday = FALSE))
crimeBoston$DAY <- as.Date(cut(crimeBoston$DATE, breaks = "day"))


# Explore crime types
unique(crimeBoston$INCIDENT_TYPE_DESCRIPTION)
barplot(prop.table(table(crimeBoston$INCIDENT_TYPE_DESCRIPTION)))
table(crimeBoston$INCIDENT_TYPE_DESCRIPTION)

# Create crime buckets
violentCrime <- c("AGGRAVATED ASSAULT", "SIMPLE ASSAULT", "DEATH INVESTIGATION", "HOMICIDE",
                  "Manslaug", "Simple Assault", "Aggravated Assault", "Homicide")
sexCrimes <- c("CRIMES AGAINST CHILDREN", "SexReg", "PROSTITUTION CHARGES", "Sex Offender Registration",
               "Prostitution", "Rape and Attempted")
propertyCrimes <- c("RESIDENTIAL BURGLARY", "ROBBERY", "COMMERCIAL BURGLARY", "PropLost", "OTHER LARCENY",
                    "AUTO THEFT", "VANDALISM", "LARCENY FROM MOTOR VEHICLE", "FIRE", "ARSON", "Vandalism",
                    "Residential Burglary", "Property Related Damage", "Commercial Burglary", "Other Burglary")

# Subset the dataset on the buckets
violence <- subset(crimeBoston, INCIDENT_TYPE_DESCRIPTION %in% violentCrime)
violenceMonth <- as.data.frame(table(violence$MONTH))
colnames(violenceMonth) <- c("Date", "Total")
violenceMonth$Date <- as.character(violenceMonth$Date)
violenceMonth$Date <- as.Date(violenceMonth$Date, format = "%Y-%m-%d")
# Drop the last observation...something is wrong with it
violenceMonth[38,] <- NA

sex <- subset(crimeBoston, INCIDENT_TYPE_DESCRIPTION %in% sexCrimes)
sexMonth <- as.data.frame(table(sex$MONTH))
colnames(sexMonth) <- c("Date", "Total")
sexMonth$Date <- as.character(sexMonth$Date)
sexMonth$Date <- as.Date(sexMonth$Date, format = "%Y-%m-%d")
sexMonth[38,] <- NA


property <- subset(crimeBoston, INCIDENT_TYPE_DESCRIPTION %in% propertyCrimes)
propertyMonth <- as.data.frame(table(property$MONTH))
colnames(propertyMonth) <- c("Date", "Total")
propertyMonth$Date <- as.character(propertyMonth$Date)
propertyMonth$Date <- as.Date(propertyMonth$Date, format = "%Y-%m-%d")
propertyMonth[38,] <- NA
propertyMonth[37,] <- NA

# Exploratory plots
# Monthly violent crime
crimeMonth <- ggplot(violenceMonth, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Violent Crime in Boston (JUL 2012 - JUL 2015)", 
       x = "Month", y = "Violent Crime by Month") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) + 
  scale_x_date(date_labels = "%Y %b")
# Need to fix x-axis scale. Change trendline color. Add "," on y-axis. 
crimeMonth

# Sex crimes
SexCrimeMonth <- ggplot(sexMonth, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Sex Crimes in Boston (JUL 2012 - JUL 2015)", 
       x = "Month", y = "Sex Crimes by Month") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) + 
  scale_x_date(date_labels = "%Y %b")
# Need to fix x-axis scale. Change trendline color. Add "," on y-axis. 
SexCrimeMonth

# Property Crimes
propertyCrimeMonth <- ggplot(propertyMonth, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Property Crimes in Boston (JUL 2012 - JUN 2015)", 
       x = "Month", y = "Property Crimes by Month") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", aes(group = 1)) + 
  scale_x_date(date_labels = "%Y %b")
# Need to fix x-axis scale. Change trendline color. Add "," on y-axis. 
propertyCrimeMonth


# Start making some heat maps
# Create base map for Boston
BostonBase <- get_map(location = c(-71.057083, 42.361145), zoom = "auto", maptype = "roadmap", 
                  source = "google")
BostonMap <- ggmap(BostonBase, fullpage = TRUE)

# violent crime maps
map1 <- ggmap(BostonBase, extent = "panel") + 
  geom_density2d(data = violentCrime, aes(x = Long, y = Lat), size = 0.3) +
  stat_density2d(data = violentCrime, aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, n = 50, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.25), guide = FALSE) +
  labs(title = "Violent Crime in Boston 2012-2015", x = "Longitude", y = "Latitude") 
map1

# Apparently the new package as broken
# Reverting to prior version
library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
devtools::install_github('hadley/ggplot2', force = TRUE)
devtools::install_github("dkahle/ggmap")

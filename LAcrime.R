# https://data.lacity.org/A-Safe-City/Crimes-2012-2015/s9rj-h3s6

# Dependencies - can probably pare a few of these down.
dependent <- c("ggmap", "data.table", "reshape2", "ggplot2", "dplyr", "forecast", "quantmod",
              "tseries", "stats", "dynlm", "vars")
lapply(dependent, library, character.only = TRUE)

library(ggmap)
library(data.table)
library(reshape2)
library(ggplot2)
library(dplyr)
library(forecast)
library(quantmod)
library(tseries)
library(stats)
library(dynlm)
library(vars)
library(RColorBrewer)

# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1),
# 144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

### Plot theme for later ###
bsbTheme <- function() {
  set1 = brewer.pal("PuBu", n = 9)
  set2 = brewer.pal("RdGy", n = 11)
  set3 = brewer.pal("Greys", n = 9)
  panelColor = set3[1]
  background = set2[7]
  majorTheme = set2[8]
  axisTextColor = set2[9]
  axisTitle = set1[8]
  title = set1[9]
  
  theme_bw(base_size = 11) +
    # chart region
    theme(panel.background = element_rect(fill = panelColor, color = panelColor)) +
    theme(plot.background = element_rect(fill = background, color = background)) +
    theme(panel.border = element_rect(color = background)) +
    # grid
    theme(panel.grid.major = element_line(color = majorTheme, size = .25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    # legend
    theme(legend.position = "right") +
    theme(legend.background = element_rect(fill = background)) +
    theme(legend.text = element_text(size = 7, color = axisTextColor)) +
    # title and axis
    theme(plot.title = element_text(color = title, size = 16, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 10, color = axisTextColor)) +
    theme(axis.text.y = element_text(size = 10, color = axisTextColor)) +
    theme(axis.title.x = element_text(size = 12, color = title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 12, color = title, vjust = 1.25)) +
    # margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.5), "cm"))
  # chart fonts
  #theme(text = element_text(familiy = "Times"))
}
# need to change fonts


setwd("~/crime")

# Load and clean the data. Add lat/long variables for location data.
crimeRaw <- read.csv(file = "Crimes_2012-2015.csv", header = TRUE, sep = ",")
crimeLA <- data.frame(crimeRaw, colsplit(crimeRaw$Location.1, pattern = "\\,", names = c("Lat", "Long")))
crimeLA$Lat <- as.numeric(sub(pattern = "\\(", replacement = "", x = crimeLA$Lat))
crimeLA$Long <- as.numeric(sub(pattern = "\\)", replacement = "", x = crimeLA$Long))
str(crimeLA)

## Add month vector for reports and occurances
crimeLA$Date.Rptd <- as.character(crimeLA$Date.Rptd)
crimeLA$Date.Rptd <- as.Date(crimeLA$Date.Rptd, format = "%m/%d/%Y")
crimeLA$ReportMonth <- format(crimeLA$Date.Rptd, "%Y-%m")

crimeLA$DATE.OCC <- as.character(crimeLA$DATE.OCC)
crimeLA$DATE.OCC <- as.Date(crimeLA$DATE.OCC, format = "%m/%d/%Y")
crimeLA$CrimeMonth <- format(crimeLA$DATE.OCC, "%Y-%m")

## Add year vector for reports and occurances. Need this for making the .gif plot easier.
# Maybe there's a different way to subset?

crimeLA$ReportYear <- format(crimeLA$Date.Rptd, "%Y")
crimeLA$CrimeYear <- format(crimeLA$DATE.OCC, "%Y")

# Select on violent crime
violentChar <- c("ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT", "RAPE, FORCIBLE",
                "RAPE, ATTEMPTED", "BATTERY WITH SEXUAL CONTACT", 
                "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER", "CRIMINAL HOMICIDE",
                "LYNCHING", "LYNCHING - ATTEMPTED", "HOMICIDE (NON-UCR)")

violence <- subset(crimeLA, CrmCd.Desc %in% violentChar)
# Report that did not, as of data release, result in an arrest
violenceReport <- subset(violence, Status.Desc != "Adult Arrest" & Status.Desc != "Juv Arrest")
# Report that did, according to the data, result in an arrest
violenceArrest <- subset(violence, Status.Desc %in% c("Adult Arrest", "Juv Arrest"))
# Specialty classes
robbery <- subset(crimeLA, CrmCd.Desc %in% c("BURGLARY", "ROBBERY"))
rapeCrime <-subset(crimeLA, CrmCd.Desc %in% c("RAPE, FORCIBLE", "RAPE"))

# remove Dec 2015 outlier for reports
# Dec 2015 data only goes to 2015-12-03
ptn <- '^2015-12.*?'
ndx <- grep(ptn, crimeLA$ReportMonth, perl = TRUE, invert = TRUE)
crimeLA2 <- crimeLA[ndx, ]


#select for only pre-ferguson report data
crimeLA3 <- crimeLA2[which(crimeLA2$ReportMonth!='2014-09' 
            & crimeLA2$ReportMonth!='2014-10' 
            & crimeLA2$ReportMonth!='2014-11' 
            & crimeLA2$ReportMonth!='2014-12' 
            & crimeLA2$ReportMonth!='2015-01' 
            & crimeLA2$ReportMonth!='2015-02' 
            & crimeLA2$ReportMonth!='2015-03' 
            & crimeLA2$ReportMonth!='2015-04' 
            & crimeLA2$ReportMonth!='2015-05' 
            & crimeLA2$ReportMonth!='2015-06' 
            & crimeLA2$ReportMonth!='2015-07' 
            & crimeLA2$ReportMonth!='2015-08' 
            & crimeLA2$ReportMonth!='2015-09' 
            & crimeLA2$ReportMonth!='2015-10' 
            & crimeLA2$ReportMonth!='2015-11'),]

# Crime reports per month
violentMonth <- data.frame(table(crimeLA2$ReportMonth))
colnames(violentMonth) <- c("Date", "Total")
class(violentMonth)
class(violentMonth$Date)
df <- data.frame(Date = c("2015-10", "2015-09", "2015-08"))
as.Date(df$Date, format = "%Y-%")
violentMonth$Date <- as.character(violentMonth$Date)
violentMonth$Date <- as.Date(violentMonth$Date, format = "%Y-%m")
head(violentMonth)

testDate <- factor("2012-01")
testDate2 <- c("2012-01")
as.Date(testDate, format = "%Y-%m")
as.Date(testDate2, format = "%Y-%m")

# pre-Ferguson reports per month
violentMonth2 <- as.data.frame(table(crimeLA3$ReportMonth))
colnames(violentMonth2) <- c("Date", "Total")
violentMonth2$Date <- as.character(violentMonth2$Date)


# Summary plot for monthly totals
crimeMonth <- ggplot(violentMonth, aes(x = Date, y = Total)) +
              geom_line(position = "identity", aes(group = 1)) +
              labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
              x = "Month", y = "Total Monthly Reports") +
              stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
              aes(group = 1)) +
              scale_x_date(date_labels = "%b %d") +
              bsbTheme()
# Need to fix x-axis scale. Change trendline color. Add "," on y-axis. 
crimeMonth

# pre-Ferguson plot
crimeMonth2 <- ggplot(violentMonth2, aes(x = Date, y = Total)) +
               geom_line(position = "identity", aes(group = 1)) +
               labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
               x = "Month", y = "Total Monthly Reports") +
               stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
               aes(group = 1)) +
               bsbTheme()
# Need to fix x-axis scale. Change trendling color. Add "," on y-axis.
crimeMonth2


# Barplot of violent crime type
violentHist <- ggplot(violence, aes(x = CrmCd.Desc)) +
               geom_bar() +
               scale_y_continuous(breaks = seq(0, 40000, 5000)) +
               labs(title = "Violence Crime in LA Jan 2012 - Dec 2015", x = "Type", y = "Total") +
               bsbTheme()
# Need to fix x-axis scale. Add "," on y-axis. Need to think about changing the y-axis scale
# Reorder barplots. Maybe color-code and include plot key?
violentHist

# Load the base map
LAbase <- get_map(location = c(-118.3308, 33.9931), zoom = "auto", maptype = "roadmap", 
                  source = "google")
LAmap <- ggmap(LAbase, fullpage = TRUE)

# Map conture plots
# Total LA crime "incidents" 2012-2015
map1 <- ggmap(LAbase, extent = "panel") + 
        geom_density2d(data = crimeLA, aes(x = Long, y = Lat), size = 0.3) +
        stat_density2d(data = crimeLA, aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
        size = 0.01, n = 50, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.25), guide = FALSE) +
        labs(title = "Total LA Crime 'Incidents'", x = "Longitude", y = "Latitude") 
map1

# Violent incidents reported in LA County 2012-2015
map2 <- ggmap(LAbase, extent = "panel") + 
        geom_density2d(data = violenceReport, aes(x = Long, y = Lat), size = 0.3) +
        stat_density2d(data = violenceReport, aes(x = Long, y = Lat, fill = ..level.., 
        alpha = ..level..), size = 0.01, n = 100, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.25), guide = FALSE) +
        labs(title = "Total LA Violent Crime Reports", x = "Longitude", y = "Latitude")
map2

# Violent incidents where there was an arrest made in LA County 2012-2015
map3 <- ggmap(LAbase, extent = "panel") +
        geom_density2d(data = violenceArrest, aes(x = Long, y = Lat), size = 0.3) +
        stat_density2d(data = violenceArrest, aes(x = Long, y = Lat, fill = ..level..,
        alpha = ..level..), size = 0.01, n = 100, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
        labs(title = "Total LA Violent Crime Arrests", x = "Longitude", y = "Latitude")
map3

# Need to think about making a GIF with the change in crime density across years, or even months







# Robberies in LA
map4 <- ggmap(LAbase, extent = "panel") +
        geom_density2d(data = robbery, aes(x = Long, y = Lat), size = 0.3) +
        stat_density2d(data = robbery, aes(x = Long, y = Lat, fill = ..level..,
        alpha = ..level..), size = 0.01, n = 100, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
        labs(title = "Total LA Robberies", x = "Longitude", y = "Latitude")
map4

# Rape in LA
map5 <- ggmap(LAbase, extent = "panel") +
        geom_density2d(data = rapeCrime, aes(x = Long, y = Lat), size = 0.3) +
        stat_density2d(data = rapeCrime, aes(x = Long, y = Lat, fill = ..level..,
        alpha = ..level..), size = 0.01, n = 100, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
        labs(title = "Total LA Rape Crimes", x = "Longitude", y = "Latitude")
map5





# ARMA Models
Total = violentMonth2[,"Total"]

par(mfrow = c(2,2))
plot(Total, type="l")

acf(Total, lag.max=24)
pacf(Total, lag.max=24)

par(mfrow=c(1,1))

model1a <- arima(Total, order=c(1,0,0)) # AR(1) 
model1b <- arima(Total, order=c(0,0,1)) # MA(1) 
model1c <- arima(Total, order=c(1,0,1)) # ARMA(1,1) 
model1d <- arima(Total, order=c(2,0,0)) # AR(2)
model1e <- arima(Total, order=c(0,0,2)) # MA(2)
model1f <- arima(Total, order=c(2,0,1)) # ARMA(2,1)
model1g <- arima(Total, order=c(1,0,2)) # ARMA(1,2)
model1h <- arima(Total, order=c(2,0,2)) # ARMA(2,2)

summary(model1a)
summary(model1d)
summary(model1h)

# Forecast error evaluation with Root Mean Square Error (RMSE): 
accuracy(model1a)
accuracy(model1b)
accuracy(model1c)
accuracy(model1d)
accuracy(model1e)
accuracy(model1f)   
accuracy(model1g)
accuracy(model1h)

for1h <- forecast(model1h,15)  # forecast the next 24 periods
for1h
crimeMonth
plot(for1h)
abline(h = 0)
grid()





for1hPlot <- ggplot(violentMonth, aes(x = Date, y = Total)) +
             geom_line(position = "identity", aes(group = 1)) +
             geom_line(position = "identity", aes(group = 1)) +

for1hPlot

arrestTS <- ts(reportMonthlyTotal, frequency = 12, start = c(2012,1))

forPlot <- funggcast(arrestTS,for1h)

library(forecast)
library(zoo)
library(ggplot2)

ggplot(violentMonth, aes(x = Date, y = Total)) +
  geom_line(position = "identity", aes(group = 1)) +
  labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
       x = "Month", y = "Total Monthly Arrests") +
  stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
              aes(group = 1))

#ARIMA Model
auto.arima(Total)
model2 <- arima(Total, order = c(0,1,0))
for2 <- forecast(model2,15)

par(mfrow=c(3,3))
plot(for2)
par(mfrow=c(1,1))

# Dynamic Linear Model

reportMonth <- as.data.frame(table(violenceReport$ReportMonth))
colnames(reportMonth) <- c("Date", "Total")
reportMonth$Date <- as.character(reportMonth$Date)

arrestMonth <- as.data.frame(table(violenceArrest$ReportMonth))
colnames(arrestMonth) <- c("Date", "Total")
arrestMonth$Date <- as.character(arrestMonth$Date)


reportMonthlyTotal <- reportMonth[,"Total"]
reportTS <- ts(reportMonthlyTotal, frequency = 12, start = c(2012,1))
plot(reportTS)

rLag <- lag(reportTS, k = -1)

arrestMonthlyTotal <- arrestMonth[,"Total"]
arrestTS <- ts(reportMonthlyTotal, frequency = 12, start = c(2012,1))
plot(arrestTS)

vLag <- lag(arrestTS, k = -1)

model3 <- dynlm(reportTS ~ vLag + rLag)
summary(model3)
plot(residuals(model3))

forecast(model3)


### Forward looking forcast
model4 <- VAR(, p = 12, type = "const")
for4 <- predict(model4, n.ahead = 24, ci = 0.95)









violentACF <- as.numeric(violentMonth[, "Total"])
acf(violentACF)

model6a <- arima(violentACF, order=c(1,0,0)) # AR(1) 
model6b <- arima(violentACF, order=c(0,0,1)) # MA(1) 
model6c <- arima(violentACF, order=c(1,0,1)) # ARMA(1,1) 
model6d <- arima(violentACF, order=c(2,0,0)) # AR(2)
model6e <- arima(violentACF, order=c(0,0,2)) # MA(2)
model6f <- arima(violentACF, order=c(2,0,1)) # ARMA(2,1)
model6g <- arima(violentACF, order=c(1,0,2)) # ARMA(1,2)
model6h <- arima(violentACF, order=c(2,0,2)) # ARMA(2,2)

summary(model6a)
summary(model6d)
summary(model6h)

accuracy(model6a)
accuracy(model6b)
accuracy(model6c)
accuracy(model6d)
accuracy(model6e)
accuracy(model6f)   
accuracy(model6g)
accuracy(model6h)

auto.arima(violentACF)

model7a <- arima(violentACF, order = c(1,0,0))
model7b <- arima(violentACF, order = c(0,0,1))
model7c <- arima(violentACF, order = c(1,0,1))
model7d <- arima(violentACF, order = c(2,0,0))
model7e <- arima(violentACF, order = c(0,0,2))
model7f <- arima(violentACF, order = c(2,0,1))
model7g <- arima(violentACF, order = c(1,0,2))
model7h <- arima(violentACF, order = c(2,0,2))

forecast7a <- forecast(model7a, 48)  
forecast7b <- forecast(model7b, 48)
forecast7c <- forecast(model7c, 48)
forecast7d <- forecast(model7d, 48)
forecast7e <- forecast(model7e, 48)
forecast7f <- forecast(model7f, 48)
forecast7g <- forecast(model7g, 48)
forecast7h <- forecast(model7h, 48)

par(mfrow = c(3,3))
plot(forecast7a)
plot(forecast7b)
plot(forecast7c)
plot(forecast7d)
plot(forecast7e)
plot(forecast7f)
plot(forecast7g)
plot(forecast7h)









  




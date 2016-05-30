# https://data.lacity.org/A-Safe-City/Crimes-2012-2015/s9rj-h3s6

# Dependencies - can probably pare a few of these down.

library(ggmap)
library(data.table)
library(reshape2)
library(ggplot2)
library(dplyr)
library(forecast)
library(quantmod)
library(tseries)
library(stats)

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

# Load and clean the data
crimeRaw <- read.csv(file = "Crimes_2012-2015.csv", header = TRUE, sep = ",")
crimeLA <- data.frame(crimeRaw, colsplit(crimeRaw$Location.1, pattern = "\\,", names = c("Lat", "Long")))
crimeLA$Lat <- as.numeric(sub(pattern = "\\(", replacement = "", x = crimeLA$Lat))
crimeLA$Long <- as.numeric(sub(pattern = "\\)", replacement = "", x = crimeLA$Long))
str(crimeLA)

## Add month vector
crimeLA$Date.Rptd <- as.character(crimeLA$Date.Rptd)
crimeLA$Date.Rptd <- as.Date(crimeLA$Date.Rptd, format = "%m/%d/%Y")
crimeLA$MONTH <- format(crimeLA$Date.Rptd, "%Y-%m")

# Select on violent crime
violentChar <- c("ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT", "RAPE, FORCIBLE",
                "RAPE, ATTEMPTED", "BATTERY WITH SEXUAL CONTACT", 
                "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER", "CRIMINAL HOMICIDE",
                "LYNCHING", "LYNCHING - ATTEMPTED", "HOMICIDE (NON-UCR)")

violence <- subset(crimeLA, CrmCd.Desc %in% violentChar)
violenceArrest <- subset(crimeLA, Status.Desc %in% c("Adult Arrest", "Juv Arrest"))

# remove Dec 2015 outlier
ptn = '^2015-12.*?'
ndx = grep(ptn, crimeLA$MONTH, perl = T,invert = T)
crimeLA2 = crimeLA[ndx, ]

#select for only pre-ferguson data
crimeLA3 <- crimeLA2[which(crimeLA2$MONTH!='2014-09' 
            & crimeLA2$MONTH!='2014-10' 
            & crimeLA2$MONTH!='2014-11' 
            & crimeLA2$MONTH!='2014-12' 
            & crimeLA2$MONTH!='2015-01' 
            & crimeLA2$MONTH!='2015-02' 
            & crimeLA2$MONTH!='2015-03' 
            & crimeLA2$MONTH!='2015-04' 
            & crimeLA2$MONTH!='2015-05' 
            & crimeLA2$MONTH!='2015-06' 
            & crimeLA2$MONTH!='2015-07' 
            & crimeLA2$MONTH!='2015-08' 
            & crimeLA2$MONTH!='2015-09' 
            & crimeLA2$MONTH!='2015-10' 
            & crimeLA2$MONTH!='2015-11'),]

# Add totals for each month
violentMonth <- as.data.frame(table(crimeLA2$MONTH))
colnames(violentMonth) <- c("Date", "Total")
violentMonth$Date <- as.character(violentMonth$Date)

# pre-Ferguson
violentMonth2 <- as.data.frame(table(crimeLA3$MONTH))
colnames(violentMonth2) <- c("Date", "Total")
violentMonth2$Date <- as.character(violentMonth2$Date)

# Summary plot for monthly totals
crimeMonth <- ggplot(violentMonth, aes(x = Date, y = Total)) +
              geom_line(position = "identity", aes(group = 1)) +
              labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
              x = "Month", y = "Total Monthly Arrests") +
              stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
              aes(group = 1))
crimeMonth

crimeMonth2 <- ggplot(violentMonth2, aes(x = Date, y = Total)) +
               geom_line(position = "identity", aes(group = 1)) +
               labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
               x = "Month", y = "Total Monthly Arrests") +
               stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
               aes(group = 1))
crimeMonth2

# Barplot of violent crime type
violentHist <- ggplot(violence, aes(x = CrmCd.Desc)) +
               geom_bar() +
               scale_y_continuous(breaks = seq(0, 40000, 5000)) +
               labs(title = "Violence Crime in LA Jan 2012 - Dec 2015", x = "Type", y = "Total")
violentHist

# Load the base map
LAbase <- get_map(location = c(-118.3308, 33.9931), zoom = "auto", maptype = "roadmap", 
                  source = "google")
LAmap <- ggmap(LAbase, fullpage = TRUE)

# Map conture plots

map1 <- ggmap(LAbase, extent = "panel") + 
        geom_density2d(data = crimeLA, aes(x = Long, y = Lat), size = 0.2) +
        stat_density2d(data = crimeLA, aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
        size = 0.2, n = 20, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.3), guide = FALSE)
map1


map2 <- ggmap(LAbase, extent = "panel") + 
        geom_density2d(data = violence, aes(x = Long, y = Lat), size = 0.2) +
        stat_density2d(data = violence, aes(x = Long, y = Lat, fill = ..level.., 
        alpha = ..level..), size = 0.2, n = 20, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.3), guide = FALSE)
map2


map3 <- ggmap(LAbase, extent = "panel") +
        geom_density2d(data = violenceArrest, aes(x = Long, y = Lat), size = 0.2) +
        stat_density2d(data = violenceArrest, aes(x = Long, y = Lat, fill = ..level..,
        alpha = ..level..), size = 0.2, n = 20, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.3), guide = FALSE)
map3


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

#ARIMA Model
auto.arima(Total)
model2 <- arima(Total, order = c(0,1,0))
for2 <- forecast(model2,15)

par(mfrow=c(3,3))
plot(for2)
par(mfrow=c(1,1))








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









  




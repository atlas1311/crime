# https://data.lacity.org/A-Safe-City/Crimes-2012-2015/s9rj-h3s6

# Dependencies

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

### Plot theme fore later ###
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

# Add totals for each month
violentMonth <- as.data.frame(table(crimeLA$MONTH))
colnames(violentMonth) <- c("Date", "Total")
violentMonth$Date <- as.character(violentMonth$Date)

# Summary plot for monthly totals
crimeMonth <- ggplot(violentMonth, aes(x = Date, y = Total)) +
              geom_line(position = "identity", aes(group = 1)) +
              labs(title = "Violent Crime Reports in LA County (Jan 2012 - Dec 2015)", 
              x = "Month", y = "Total Monthly Arrests") +
              stat_smooth(method = "lm", se = TRUE, fill = "black", colour = "black", 
              aes(group = 1))
crimeMonth

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


# Models

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









  




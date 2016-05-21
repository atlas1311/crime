# https://data.lacity.org/A-Safe-City/Crimes-2012-2015/s9rj-h3s6

# Dependencies

setwd("~/crime")
crimeRaw <- read.table(file = "LA_Crimes_2012-2015.csv", header = TRUE, sep = ",")

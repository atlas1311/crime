# https://data.lacity.org/A-Safe-City/Crimes-2012-2015/s9rj-h3s6

# Dependencies

library(ggmap)
library(data.table)
library(reshape2)
library(ggplot2)
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1),
# 144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

setwd("~/crime")

# Load and clean the data
crimeRaw <- read.csv(file = "Crimes_2012-2015.csv", header = TRUE, sep = ",")
crimeLA <- data.frame(crimeRaw, colsplit(crimeRaw$Location.1, pattern = "\\,", names = c("Lat", "Long")))
crimeLA$Lat <- as.numeric(sub(pattern = "\\(", replacement = "", x = crimeLA$Lat))
crimeLA$Long <- as.numeric(sub(pattern = "\\)", replacement = "", x = crimeLA$Long))


# Load the base map
LAbase <- get_map(location = c(-118.3308, 33.9931), zoom = "auto", maptype = "roadmap", 
                  source = "google")
LAmap <- ggmap(LAbase, fullpage = TRUE)

map1 <- ggmap(LAbase, extent = "panel") + 
        geom_density2d(data = crimeLA, aes(x = Long, y = Lat), size = 0.2) +
        stat_density2d(data = crimeLA, aes(x = Long, y = Lat, fill = ..level.., alpha = ..level..),
        size = 0.2, n = 20, geom = "polygon") +
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0, 0.3), guide = FALSE)
map1







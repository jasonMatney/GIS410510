####################################################################################
################[ Ordinary Kriging with Jockey's Ridge LiDAR data  ]################
################[ NC State University GIS 410/510 - Spring 17        ]##############
################[ 03/27/2017                                       ]################
####################################################################################

# Inspiration and most of the code from: 
# https://rpubs.com/nabilabd/118172
# by Nabil A.

for (package in c('sp', 'gstat', 'dplyr', 'ggplot2', 'scales', 'magrittr', 'RCurl')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos="http://cran.us.r-project.org")
    library(package, character.only=T)
  }
}

library(sp)
library(gstat)
# packages for manipulation & visualization
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse" and "sample_n"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})

library(RCurl)
# Load the data
JR_elev_2001_30K <- getURL("https://raw.githubusercontent.com/jasonMatney/GIS410510/master/JR_elev_2001_30K.csv")
JR_elev_2001 <- read.csv(text = JR_elev_2001_30K, col.names=c("x","y","z","id"))[ ,1:3]
JR_elev_2001 <- sample_n(JR_elev_2001, 1000) # downsample

# Inspect the data
glimpse(JR_elev_2001)

# Make a spatial object
coordinates(JR_elev_2001) <- ~x+y

# Fit variogram 
lzn.vgm <- variogram(z~1, JR_elev_2001) # calculates sample variogram values 
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(psill=35, model="Exp", range=5)) # fit model

# Plot the sample values, along with the fit model
plot(lzn.vgm, lzn.fit) 

# Sample spatial domain to interpolate over
JR_elev_2001_grid = spsample(JR_elev_2001, type = "regular", cellsize = c(10,10)) 
gridded(JR_elev_2001_grid) = TRUE

# Create plot of the downscaled data
plot1 <- JR_elev_2001 %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# Create plot of gridded area over the region of interest
plot2 <- JR_elev_2001_grid %>% as.data.frame %>%
  ggplot(aes(x1, x2)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

# Display those plots side-by-side
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

# Time the kriging operation
# Start the clock!
ptm <- proc.time()

# Perform Ordinary Kriging 
lzn.kriged <- krige(z ~ 1, JR_elev_2001, JR_elev_2001_grid, model=lzn.fit)

# Stop the clock!
proc.time() - ptm

# Plot using ggplot2
p <- lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x1, x2))

p + geom_point(aes(colour = var1.pred))  + scale_colour_gradient(low = "yellow", high="red") + coord_equal() 



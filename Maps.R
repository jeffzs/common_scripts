#install.packages(c("maptools", "RColorBrewer", "classInt"))
library(maptools)
library(RColorBrewer)
library(classInt)

map.usa <- readShapePoly("/Users/zhifeisun/Desktop/cb_2014_us_state_500k/cb_2014_us_state_500k.shp")
names(map.usa)
colours <- brewer.pal(5, "Blues") 
plot(map.usa)

sport<- readShapePoly("london_sport.shp")
names(sport)<- c("ID", "name", "Partic_Per", "Pop_2001")

##############
install.packages("rworldmap")
library(rworldmap)
data(countryExData)
sPDF <- joinCountryData2Map( countryExData, joinCode = "ISO3", nameJoinColumn = "ISO3V10")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( sPDF, nameColumnToPlot="BIODIVERSITY" )
###########

install.packages("maps")
install.packages("ggplot2")
library(maps)
library(ggplot2)
all_states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
p
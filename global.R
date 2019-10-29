library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(sp)
library(maptools)
library(rgdal)
library(shinyjs)
library(shinyBS)

tabledata_MS<-read.csv("data/nonspatial/ProjectCatalog_MS.csv")
tabledata_AL<-read.csv("data/nonspatial/ProjectCatalog_AL.csv")
tabledata_LA<-read.csv("data/nonspatial/ProjectCatalog_LA.csv")
tabledata_TX<-read.csv("data/nonspatial/ProjectCatalog_TX.csv")
tabledata_FL<-read.csv("data/nonspatial/ProjectCatalog_FL.csv")
tabledata_Regional<-read.csv("data/nonspatial/ProjectCatalog_Regional.csv")
tabledata_newly<-read.csv("data/nonspatial/Newly_added.csv")

Points_city_ms <- readOGR(dsn = "data/spatial/point/city_ms.shp", layer = "city_ms")
Points_city_tx <- readOGR(dsn = "data/spatial/point/city_tx.shp", layer = "city_tx")
Points_county_al <- readOGR(dsn = "data/spatial/point/county_al.shp", layer = "county_al")
Points_county_ms <- readOGR(dsn = "data/spatial/point/county_ms.shp", layer = "county_ms")
Points_county_tx <- readOGR(dsn = "data/spatial/point/county_tx.shp", layer = "county_tx")
Points_county_fl <- readOGR(dsn = "data/spatial/point/county_fl.shp", layer = "county_fl")
Points_perish_la <- readOGR(dsn = "data/spatial/point/perish_la.shp", layer = "perish_la")
Points_state <- readOGR(dsn = "data/spatial/point/state.shp", layer = "state")

City_ms <- readOGR(dsn = "data/spatial/city/City_MS.shp", layer = "City_MS")
City_tx <- readOGR(dsn = "data/spatial/city/City_TX.shp", layer = "City_TX")

Costalzone <- readOGR(dsn = "data/spatial/coastalzone/coastalzone.shp", layer = "coastalzone")

County_All <- readOGR(dsn = "data/spatial/county/County_all.shp", layer = "County_all")

Regional <- readOGR(dsn = "data/spatial/regional/SECoastStates.shp", layer = "SECoastStates")
State <- readOGR(dsn = "data/spatial/state/SECoastStates.shp", layer = "SECoastStates")
Watershed <- readOGR(dsn = "data/spatial/watershed/Watershed_all.shp", layer = "Watershed_all")
Watershed1 <- readOGR(dsn = "data/spatial/watershed/watershed_1.shp", layer = "watershed_1")

Points_city_ms <- spTransform(Points_city_ms, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_city_tx <- spTransform(Points_city_tx, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_county_al <- spTransform(Points_county_al, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_county_ms <- spTransform(Points_county_ms, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_county_tx <- spTransform(Points_county_tx, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_county_fl<- spTransform(Points_county_fl, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_perish_la <- spTransform(Points_perish_la, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_state <- spTransform(Points_state, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Points_city_ms@data[is.na(Points_city_ms@data)]<-0

City_ms <- spTransform(City_ms, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
City_tx  <- spTransform(City_tx , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Costalzone <- spTransform(Costalzone, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
County_All <- spTransform(County_All, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Regional<- spTransform(Regional, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
State  <- spTransform(State , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Watershed  <- spTransform(Watershed , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Watershed1  <- spTransform(Watershed1 , CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

shortdata_MS<-subset(tabledata_MS, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdata_FL<-subset(tabledata_FL, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdata_TX<-subset(tabledata_TX, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdata_AL<-subset(tabledata_AL, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdata_LA<-subset(tabledata_LA, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdata_Regional<-subset(tabledata_Regional, select=c(Plan.Name,Plan.Timeframe,Agency.Lead,Geo.Extent))
shortdatage<-rbind(shortdata_AL,shortdata_FL,shortdata_MS,shortdata_Regional,shortdata_TX,shortdata_LA)
data<-rbind(tabledata_AL,tabledata_FL,tabledata_MS,tabledata_Regional,tabledata_TX,tabledata_LA)







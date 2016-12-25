#Must have dplyr, and geosphere installed, tibble is optional. Must have the PrecipData.RData loaded into the home director's download directory, or change the line to point to a different location
library(dplyr)
library(tibble)
library(geosphere)
load("~/Downloads/PrecipData.RData")

#The three sites to be examined
PatonaBayMarina <- c(-85.78179219999998, 41.3356788 )
CulverMarina <- c(-86.38940309999998,41.1898399)
MorseMarina <- c(-86.03378399999997,40.09323699999999)

#Implementation of K Nearest Neighbors
KNN <- function(target, k){
  #arrange data by euclidian distance away from target, take the top k of the list, and then average the precipitation at those sites
  precip <- Precip.df %>% 
    arrange(apply(Precip.df[,c("Longitude","Latitude")],1,distVincentyEllipsoid,p2=target)) %>%
    filter(Total.Precip>=0) %>%
    head(k) %>% 
    summarise(Avg.Precip = round(mean(Total.Precip,na.rm=TRUE),3))
  return(precip)
}

WithinN <- function(target, n){
  #Filter by all sites that are within n miles of target, using the Haversine formula for geospatial distance, and then mean the precipitation at those points  
  precip <- Precip.df %>%
    filter(apply(Precip.df[,c("Longitude","Latitude")],1,distVincentyEllipsoid,p2=target)*0.000621371<=n) %>%
    summarise(Avg.Precip=round(mean(Total.Precip,na.rm=TRUE),3))
  return(precip)
}

print("Nearest 5 Neighbors vs Within 25 miles")
print("Patona Bay Marina")
print(paste(c(KNN(PatonaBayMarina,5)$Avg.Precip,"vs",WithinN(PatonaBayMarina,25)$Avg.Precip),collapse = " "))

print("Culver Marina")
print(paste(c(KNN(CulverMarina,5)$Avg.Precip,"vs",WithinN(CulverMarina,25)$Avg.Precip),collapse = " "))

print("Morse Marina")
print(paste(c(KNN(MorseMarina,5)$Avg.Precip,"vs",WithinN(MorseMarina,25)$Avg.Precip),collapse = " "))

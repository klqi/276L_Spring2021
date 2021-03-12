setwd("C:/Users/ceiba/Documents/School/SIO 276L")
habd<- read.csv("hab_filtered_2020.csv")
C15north<- read.csv("c15_b2f8_9567_652c.csv")
C15north<- na.omit(C15north)
C15east<- read.csv("c15_fe79_009e_172d.csv")
C15east<- na.omit(C15east)
C15Temp<- read.csv("c15_f596_07e2_f42c.csv")
C15Temp<- na.omit(C15Temp)

View(habd)
length(unique(habd$description))
Stations<- table((habd$description))


#Subsetting by description
# ##########----------Next steps:
Get datasets to line up wll emough for EDM

#Grouping data by latitude/longitude:
library(sf)
LocHabd <- st_as_sf(habd, coords = c("longitude", "latitude"), crs = 4326)

####### Aggregating chlorophyll data by GPS coords
Habdistances = as.matrix(dist(habd)) 

#focused on Sarasota bay (Station w highest freq obs)
C15<- tibble(
  lat=rep(27.2990, 36686),
  long = rep(-82.6400, 36686))
C10<- tibble(
  lat=rep(27.1250, 36686),
  long = rep(-82.87500, 36686))
#C12<- tibble(
  lat=rep(27.2990, 36686),
  long = rep(-82.6400, 36686))

###################______________ Disntances between Buoys and HABSOS Observations
library(geosphere)
#1609.35 is the conversion from miles to meters
dist<-distGeo(habd[, c("longitude", "latitude")], C15[, c("long", "lat")])
habdiff<-cbind(habd, difference_metersC15=dist)
length(habdiff$difference_metersC15[which(habdiff$difference_metersC15<10000)])
#483 observations @ 6km
#5526 observations @ 10km radius

#lets try another Buoy C10
dist<-distGeo(habd[, c("longitude", "latitude")], C10[, c("long", "lat")])
habdiff<-cbind(habd, difference_metersC10=dist)
length(habdiff$difference_metersC10[which(habdiff$difference_metersC10<10000)])
#154 obs @ 10km
#That sucks, lets check diff b/n SECOORA Buoy and HABSOS



# Difference b/n C10 and C15:
dist<-distGeo(C10[, c("long", "lat")], C15[, c("long", "lat")])
#30km



#######Surprise we have have salinity data.... but how much?
length(na.omit(habd$salinity))
##17114 - Thats good right

Habsal<- habd[!is.na(habd$salinity),]

### Now lets see how much data we have thats close (ish) to a buoy
C15<- tibble(
  lat=rep(27.2990, 17114),
  long = rep(-82.6400, 17114))

dist<-distGeo(Habsal[, c("longitude", "latitude")], C15[, c("long", "lat")])
Habdiff<-cbind(Habsal, difference_metersC15=dist)
length(Habdiff$difference_metersC15[which(Habdiff$difference_metersC15<10000)])
##1447 obs @ 10km radius





###### Filtering data by frequency:
library(dplyr)
LocHabd %>% 
  group_by(description) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  filter(freq > 50000) %>%
  select(-freq)


#Problem: new geometry column hard to understand

#For aggregating:
kmeans
fviz_nbclust() (for determining optimal number of clusters from factoextra package)


######----- Lets use xts to combine buoy data with algae data by date/time to create an aligned dataset for simplex proj ###########
#Diff method
library(tidyverse)
habd <- rename(habd,
               time = sample_date
)

#remove last 7 digits from time column in buoy data so we dont have to deal with time:
C15easta = data.frame("data"=C15east)
C15east$time = substr(C15easta$time,1,nchar(C15east$time)-3)

habDates <- format(as.POSIXct(strptime(habd$time,"%m/%d/%Y %H:%M",tz="")) ,format = "%d-%m-%Y")
habd$time<- habDates
tempdates<-  format(as.POSIXct(strptime(C15Temp$time,"%y/%m/%d %H:%M",tz="")) ,format = "%d-%m-%Y")
  C15Temp$time<- tempdates
eastdates<-  format(as.POSIXct(strptime(C15east$time,"%y/%m/%d %H:%M",tz="")) ,format = "%d/%m/%Y")
  C15east$time<-eastdates
Northdates<-  format(as.POSIXct(strptime(C15north$time,"%y/%m/%d %H:%M",tz="")) ,format = "%d/%m/%Y")
  C15north$time<- Northdates
  
xts(habd, order.by = habd$time, "%m/%d/%Y")
xts(C15east, order.by = C15east$time, "%m/%d/%Y")

habdzoo<- read.zoo(habd, index.column = 7, sep = ",", format = "%m/%d/%Y")


habsec<- merge(habd, C15east, C15north, C15Temp, by= time , fill=NA)

############################### EDM ##########################################
C15north<- read.csv("c15_b2f8_9567_652c.csv")
C15north<- na.omit(C15north)
C15east<- read.csv("c15_fe79_009e_172d.csv")
C15east<- na.omit(C15east)
C15Temp<- read.csv("c15_f596_07e2_f42c.csv")
C15Temp<- na.omit(C15Temp)

length(C15east$eastward_sea_water_velocity)
[1] 948901
> length(C15Temp$sea_water_temperature)
[1] 149635
> length(C15north$northward_sea_water_velocity)
[1] 948901



################1: Find optimal embedding dimension
rho_E1 <- EmbedDimension(dataFrame = HabdF, lib = "1 3000", pred = "3500 6000",
                         columns = "", target = "cellcount")
title(main = "SECOORA C15")



##############2: Test for nonlinearity #################
######## Prediction skill varying theta(S-map Localisation) ####################

rho_theta <- PredictNonlinear(dataFrame = HabdF, lib = "1 3000", pred = "3500 6000",
                              target = "cellcount", columns = "", E = 3)
title(main = "SECOORA C15")


##############3: Simplex

#Load packages
library(data.table)
library(ggplot2)

#Force fixed notation of numbers instead of scientific
options(scipen = 0)  

#Define a 'not in' function
'%ni%' <- function(x,y)!('%in%'(x,y))

#Define a function 
milbil <- function(x){
  if (x >= 1e9){ paste0("$", round(x/1e9,0), "B")}
  else if (x >= 1e6){ paste0("$", round(x/1e6,0), "M")}
}

#Set working directory
setwd(paste0(getwd(),"/Johns Hopkins University/Reproducible Research/Course Project 1"))

#Download .csv.bz2
if (!file.exists("StormData.csv.bz2")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
    destfile = file.path(getwd(), "StormData.csv.bz2"))
}
StormData <- fread("StormData.csv.bz2")
setDT(StormData)
StormData <- StormData[, c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG", "CROPDMGEXP")]


#Property Damage
StormData[(PROPDMGEXP == "K" | PROPDMGEXP == "k"), "PROPDMGEXP"] <- 3
StormData[(PROPDMGEXP == "M" | PROPDMGEXP == "m"), "PROPDMGEXP"] <- 6
StormData[(PROPDMGEXP == "B" | PROPDMGEXP == "b"), "PROPDMGEXP"] <- 9
StormData[(PROPDMGEXP == "H" | PROPDMGEXP == "h"), "PROPDMGEXP"] <- 2
StormData[PROPDMGEXP %ni% 1:9, "PROPDMGEXP"] <- 0
StormData[,Property_Damage := as.numeric(PROPDMG) * 10^(as.numeric(PROPDMGEXP))]

#Storm Damage Exponent
StormData[(CROPDMGEXP == "K" | CROPDMGEXP == "k"), "CROPDMGEXP"] <- 3
StormData[(CROPDMGEXP == "M" | CROPDMGEXP == "m"), "CROPDMGEXP"] <- 6
StormData[(CROPDMGEXP == "B" | CROPDMGEXP == "b"), "CROPDMGEXP"] <- 9
StormData[(CROPDMGEXP == "H" | CROPDMGEXP == "h"), "CROPDMGEXP"] <- 2
StormData[CROPDMGEXP %ni% 1:9, "CROPDMGEXP"] <- 0
StormData[,Crop_Damage := as.numeric(CROPDMG) * 10^(as.numeric(CROPDMGEXP))]

#Economic_Damage
StormData[,Economic_Damage := Property_Damage + Crop_Damage]

#Remove Feilds
StormData[,c("CROPDMG", "CROPDMGEXP", "PROPDMG", "PROPDMGEXP") := NULL]

#Summarize by Event Type
FATALITIES <- StormData[,sum(FATALITIES), EVTYPE][,FATALITIES := V1][,V1 := NULL]
INJURIES <- StormData[,sum(INJURIES), EVTYPE][,INJURIES := V1][,V1 := NULL]
DAMAGE <- StormData[,sum(Economic_Damage), EVTYPE][,Economic_Damage := V1][,V1 := NULL]

#Hail
StormData[grepl(pattern = "*hail*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Hail"

#Tornado
StormData[grepl(pattern = "*tor*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Tornado"

#Tsunami
StormData[grepl(pattern = "*tsu*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Tsunami"

#Typhoon/Hurricane
StormData[grepl(pattern = "*typhoon*|*hurricane*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Typhoon/Hurricane"

#Landslide/Avalanche
StormData[grepl(pattern = "*slide*|*avalanche*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Landslide/Avalanche"

#Flooding
StormData[grepl(pattern = "*flood*|*surge*|*fld*|*surf*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Flooding"

#Thunderstorm
StormData[grepl(pattern = "*rain*|*thunder*|*lightning*|*TSTM*|*wind*|*wet*|*percip*|*precip*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Thunderstorm"

#Drought/Fire
StormData[grepl(pattern = "*fire*|*drought*|*heat*|*high*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Drought/Fire"
#Winter Weather
StormData[grepl(pattern = "*snow*|*freeze*|*frost*|*winter*|*blizzard*|*ice*|*cold*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Winter Weather"
#Fog/Dust
StormData[grepl(pattern = "*fog*|*dust*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Fog/Dust"
#Other
StormData[!grepl(pattern = "*fog*|*winter*|*fire*|*thunder*|*flood*|*slide*|*typh*|*tsun*|*tor*|*hail*",
                       x = StormData$EVTYPE, ignore.case = TRUE), "EVTYPE"] <- "Other"

#Summerize Economic Damage
Damage <- StormData[,sum(Economic_Damage, na.rm = TRUE), EVTYPE][,Event := EVTYPE][,Economic_Damage := V1][,c("EVTYPE","V1") := NULL]

#Summerize Fatalities
Fatalities <- StormData[,sum(FATALITIES, na.rm = TRUE), EVTYPE][,Event := EVTYPE][,Fatalities := V1][,c("EVTYPE","V1") := NULL]

#Summerize Injuries
Injuries <- StormData[,sum(INJURIES, na.rm = TRUE), EVTYPE][,Event := EVTYPE][,Injuries := V1][,c("EVTYPE","V1") := NULL]

#Merge Economic Damage, Fatalities, and Injuries
StormData <- merge(Damage, Fatalities, by = "Event")
StormData <- merge(StormData, Injuries, by = "Event")

#All a label with millions, billions denoted
StormData[ ,Label := sapply(Economic_Damage, milbil)]

#Clean Enviroment
rm(list=setdiff(ls(), "StormData"))


# Plot Economic Damage
ggplot(StormData, aes( x = reorder(Event, - Economic_Damage), y= Economic_Damage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Label),  vjust=-0.25)+
  scale_y_log10(breaks=c(10^0,10^1,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9, 10^10, 10^11))+
  labs( title = "Total Economic Damage by Event", x = "Event", y = "Economic Damage")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
  
# Plot Injuries
ggplot(StormData, aes( x = reorder(Event, - Injuries), y= Injuries)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Injuries),  vjust=-0.25)+
  scale_y_log10(breaks=c(10^0,10^1,10^2,10^3,10^4,10^5,10^6))+
  labs( title = "Total Injuries by Event", x = "Event", y = "# of Injuries")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# Plot Fatalities
ggplot(StormData, aes( x = reorder(Event, - Fatalities), y= Fatalities)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Fatalities),  vjust=-0.25)+
  scale_y_log10(breaks=c(10^0,10^1,10^2,10^3,10^4))+
  labs( title = "Total Fatalities by Event", x = "Event", y = "# of Fatalities")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

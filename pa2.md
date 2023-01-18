---
title: "The Environments Event with the Biggest Impact on Economy and Public Health"
author: "FJ Haran"
date: "2023-01-17"
output: 
  html_document:
     keep_md: yes
---

## Synopsis

This project analyzed data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks the characteristics of storms and weather events in the United States, including when and where they occur and estimates of any fatalities, injuries, and property damage. Analyses performed were the same for both questions (Question 1 (Q1) and Question 2 (Q2) and are detailed below.

* New data frames, q1_data and q2_data, were created for each question from the storm data provided.
* Creation of the Q2 data frame was more involved as calculations were involved to properly represent the total value in dollars for both property and crop damages. This involved processing codes in the storm data representing different dollar amounts (i.e., hundreds and billions).
* Data was summarized by event type.
* Top 5 events were found and reported for each environmental event group.
* Two plots were created for the top 5 environmental events, for the combined public health concerns (Q1) and total economic damages (Q2).

 
### Ready environment and load the data


* Load libraries

```r
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)
library(crayon)
library(stringr)
```

* Download & load data set

```r
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2',
              destfile = './pa2.bz2', method = 'curl', quiet = T)
storm_data <- read.csv('pa2.bz2')
```

## Question 1: Across the United States, which types of events are most harmful with respect to population health?

### **Data Processing**

* Select data and create new data frame

```r
evtype <- storm_data %>% select('EVTYPE')
colnames(evtype)[1] <- "EventType"
evtype %>% mutate_if(is.character, str_to_upper) -> evtype

deaths <- storm_data %>% select('FATALITIES')
colnames(deaths)[1] <- "Deaths"

injuries <- storm_data %>% select('INJURIES')
colnames(injuries)[1] <- "Injuries"

deaths_and_injuries <- deaths + injuries
colnames(deaths_and_injuries)[1] <- "DeathsAndInjuries"

q1_data <- data.frame(evtype, deaths, injuries, deaths_and_injuries)
```

* Group data by event type

```r
GrpQ1data <- q1_data %>% 
  group_by(EventType) %>%
  summarise_all(sum)
```

* Find top 5 events

```r
Deaths <- GrpQ1data[order(GrpQ1data$Deaths, decreasing = T), ]
Deaths <- Deaths[-c(3:4)]
TopDeaths <- head(Deaths, n=5)

Injuries <- GrpQ1data[order(GrpQ1data$Injuries, decreasing = T), ]
Injuries <- Injuries[-c(2,4)]
TopInjuries <- head(Injuries, n=5)

DeathsAndInjuries <- GrpQ1data[order(GrpQ1data$DeathsAndInjuries, decreasing = T), ]
DeathsAndInjuries <- DeathsAndInjuries[-c(2:3)]
topDeathsAndInjuries <- head(DeathsAndInjuries, n=5)
```

## Results 

* Report environmental events with the greates impacts.

```r
cat('The environmental event that results in the most deaths is:', TopDeaths$EventType[which.max(TopDeaths$Deaths)])
```

```
## The environmental event that results in the most deaths is: TORNADO
```


```r
cat('The environmental event that results in the most injuries is:', TopInjuries$EventType[which.max(TopInjuries$Injuries)])
```

```
## The environmental event that results in the most injuries is: TORNADO
```


```r
cat('The environmental event that is the most harmful to overall population health is:', topDeathsAndInjuries$EventType[which.max(topDeathsAndInjuries$DeathsAndInjuries)])
```

```
## The environmental event that is the most harmful to overall population health is: TORNADO
```

* Plot data

```r
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + .01, las = 3, cex = 0.7, cex.axis = .7, cex.main = 1.4, cex.lab = 1.2)

barplot(topDeathsAndInjuries$DeathsAndInjuries, names.arg = topDeathsAndInjuries$EventType, col = 'orange', ylab = 'Number of Deaths & Injuries', horiz = TRUE)

mtext("Total Public Health Concerns by Environmental Event", line = -2, side = 3, outer=TRUE, cex=1.6, las = 0, font = 2)           
```

<img src="pa2_files/figure-html/fig1-1.png" angle=90 style="display: block; margin: auto;" />


## Question 2: Across the United States, which types of events have the greatest economic consequences?

* Create new data frame

```r
evtype <- storm_data %>% select('EVTYPE')
colnames(evtype)[1] <- "EventType"
evtype %>% mutate_if(is.character, str_to_upper) -> evtype
evtype$EventType <- trimws(evtype$EventType, which = c("left"))
evtype$EventType <- gsub("[.]$","", evtype$EventType)
evtype$EventType <- as.data.frame(evtype$EventType)

#property damage
prop_exp <- storm_data %>% select('PROPDMGEXP')
colnames(prop_exp)[1] <- "prop_exp"
prop_exp <- prop_exp %>%
  mutate(prop_exp = case_when(
          prop_exp=="" ~ 0,
          prop_exp=="-" ~ 0,
          prop_exp=="?" ~ 0,
          prop_exp=="0" ~ 0,
          prop_exp=="1" ~ 1,
          prop_exp=="2" ~ 2,
          prop_exp=="3" ~ 3,
          prop_exp=="4" ~ 4,
          prop_exp=="5" ~ 5,
          prop_exp=="6" ~ 6,
          prop_exp=="7" ~ 7,
          prop_exp=="8" ~ 8,
          prop_exp=="9" ~ 9,
          prop_exp=="h" ~ 2,
          prop_exp=="H" ~ 2,
          prop_exp=="k" ~ 3,
          prop_exp=="K" ~ 3,
          prop_exp=="m" ~ 6,
          prop_exp=="M" ~ 6,
          prop_exp=="b" ~ 9,
          prop_exp=="B" ~ 9,
          TRUE ~ 0)) 

prop_dmg <- storm_data %>% select('PROPDMG')
colnames(prop_dmg)[1] <- "PropDmg"
prop_dmg <- prop_dmg %>%
    mutate(TotPropDmg = PropDmg*10^(prop_exp))
prop_dmg <- prop_dmg[,-1]
colnames(prop_dmg)[1] <- "PropDmg"

#crop damage
crop_exp <- storm_data %>% select('CROPDMGEXP')
colnames(crop_exp)[1] <- "crop_exp"
crop_exp <- crop_exp %>%
  mutate(crop_exp = case_when(
          crop_exp=="" ~ 0,
          crop_exp=="-" ~ 0,
          crop_exp=="?" ~ 0,
          crop_exp=="0" ~ 0,
          crop_exp=="1" ~ 1,
          crop_exp=="2" ~ 2,
          crop_exp=="3" ~ 3,
          crop_exp=="4" ~ 4,
          crop_exp=="5" ~ 5,
          crop_exp=="6" ~ 6,
          crop_exp=="7" ~ 7,
          crop_exp=="8" ~ 8,
          crop_exp=="9" ~ 9,
          crop_exp=="h" ~ 2,
          crop_exp=="H" ~ 2,
          crop_exp=="k" ~ 3,
          crop_exp=="K" ~ 3,
          crop_exp=="m" ~ 6,
          crop_exp=="M" ~ 6,
          crop_exp=="b" ~ 9,
          crop_exp=="B" ~ 9,
          TRUE ~ 0)) 

crop_dmg <- storm_data %>% select('CROPDMG')
colnames(crop_dmg)[1] <- "CropDmg"
crop_dmg <- crop_dmg %>%
    mutate(TotcropDmg = CropDmg*10^(crop_exp))
crop_dmg <- crop_dmg[,-1]
colnames(crop_dmg)[1] <- "CropDmg"

eco_dmg <- prop_dmg + crop_dmg
colnames(eco_dmg)[1] <- "EcoDmg"

q2_data <- data.frame(evtype, prop_dmg, crop_dmg, eco_dmg)

q2_data <- q2_data[-246124,]
q2_data <- as.data.frame(q2_data)
```

* Summarize group data by event type

```r
GrpQ2data <- q2_data %>% 
  group_by(EventType) %>%
  summarise_all(sum)

colnames(GrpQ2data)[1] <- "EventType"
```


* Find Top 5 events

```r
PropDmg <- GrpQ2data[order(GrpQ2data$PropDmg, decreasing = T), ]
PropDmg <- PropDmg[-c(3:4)]
TopPropDmg <- head(PropDmg, n=5)

CropDmg <- GrpQ2data[order(GrpQ2data$CropDmg, decreasing = T), ]
CropDmg <- CropDmg[-c(2,4)]
TopCropDmg <- head(CropDmg, n=5)

EcoDmg <- GrpQ2data[order(GrpQ2data$EcoDmg, decreasing = T), ]
EcoDmg <- EcoDmg[-c(2:3)]
topEcoDmg <- head(EcoDmg, n=5)
```

## Results 

* Report evironmental events with the largest impact.

```r
cat('The environmental event that results in the most property damage is:', TopPropDmg$EventType[1,1])
```

```
## The environmental event that results in the most property damage is: FLOOD
```


```r
cat('The environmental event that results in the most crop damage is:', TopCropDmg$EventType[1,1])
```

```
## The environmental event that results in the most crop damage is: DROUGHT
```


```r
cat('The environmental event that is the most economy damages is:', topEcoDmg$EventType[1,1])
```

```
## The environmental event that is the most economy damages is: FLOOD
```

* Plot the data 

```r
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + .01, las = 3, cex = 0.7, cex.axis = .7, cex.lab = 1.3)

barplot(topEcoDmg$EcoDmg, names.arg = unlist(topEcoDmg$EventType), col = 'orange', ylab = 'Economic Damages', horiz = TRUE)

mtext("Total Economic Damages by Environmental Event", line = -2, side = 3, outer=TRUE, cex=1.6, las = 0, font = 2)           
```

<img src="pa2_files/figure-html/fig2-1.png" angle=90 style="display: block; margin: auto;" />

library(ggplot2)
library(stringr)
library(RColorBrewer)
library(reshape2)
library(dplyr)

# Get Data
getdata <- function() {
    # Download & Read Data, note very large file
    #setwd('C:/Users/Darren/Dropbox/R_DataScience/StormOutcomes')
    if(!exists('StormData')) {
        url  <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
        destfile <- 'StormData.bz2'
        #add time downloaded 
        if(!file.exists(destfile)){
            print('Downloading Large File')
            download.file(url,destfile)
            unlink(url)}
        print('Reading Large Compressed Dataset into Memory')
        StormData <<- read.csv(destfile, stringsAsFactors = F) 
    }
    # Subset Data
    My_StormData <- StormData %>% 
        select(REFNUM,EVTYPE,FATALITIES:PROPDMG,CROPDMG,PROPDMGEXP,CROPDMGEXP,REMARKS) %>% 
        filter(PROPDMG != 0|CROPDMG != 0|INJURIES != 0|FATALITIES!= 0) %>% 
        mutate(EVTYPE=toupper(EVTYPE))  
    row.names(My_StormData) <- My_StormData$REFNUM
    My_StormData
}
My_StormData <- getdata()

# Clean Property & Crop Values 
CleanExponents <- function() {
    # Clean Property Damage Exponents
    table(My_StormData$PROPDMGEXP)
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Kk]','3')
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Hh]','2')
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Mm]','6')
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Bb]','9')
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'\\+|\\-','3')
    
    My_StormData %>% filter(PROPDMGEXP == '', PROPDMG!= 0)  %>% nrow()
    My_StormData %>% filter(PROPDMGEXP == '', PROPDMG!= 0) %>% select(REMARKS) %>% head()
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'^$','3')
    
    My_StormData %>% filter(PROPDMGEXP == '0', PROPDMG!= 0) %>% nrow()
    My_StormData %>% filter(PROPDMGEXP == '0', PROPDMG!= 0) %>%  select(REMARKS) %>% head(2)
    My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'0','3')
    table(My_StormData$PROPDMGEXP)
    
    My_StormData$PROPDMGEXP <- as.numeric(My_StormData$PROPDMGEXP)
    
    # Again for Crop Damage Exponents
    table(My_StormData$CROPDMGEXP)
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Kk]','3')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Hh]','2')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Mm]','6')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Bb]','9')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'\\?','3')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'^$','3')
    My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'0','3')
    My_StormData$CROPDMGEXP <- as.numeric(My_StormData$CROPDMGEXP)
    
    #Where PROPDMG > 999 make exponent 0
    My_StormData %>% filter(PROPDMG>999) %>% nrow()
    My_StormData %>% filter(PROPDMG>999) %>% select(PROPDMG,PROPDMGEXP)
    My_StormData[My_StormData['PROPDMG'] > 999,'PROPDMGEXP'] = 0
    
    My_StormData
}
My_StormData <- CleanExponents()

## select statement reorders
My_StormData <-select(My_StormData,REFNUM:PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,
                      REMARKS)
My_StormData$PropertyDamage <- apply(My_StormData[,5:6],1,function(p) p[1]*10^p[2])
My_StormData$CropDamage <- apply(My_StormData[,7:8],1,function(p) p[1]*10^p[2])
My_StormData$Cost <- My_StormData$PropertyDamage + My_StormData$CropDamage
My_StormData <- select(My_StormData,REFNUM,Fatalities=FATALITIES,Injuries=INJURIES,REMARKS,
                       PropertyDamage,CropDamage,Cost,EVTYPE)                                

# Read EventTypes from text file
EventTypes <- readLines('EventTypes.txt')
EventLabels <- EventTypes %>% str_trim() %>% str_replace_all('[\\W+]','')

# Create Function to Solve for Category Names given a set of regular expressions
# Returns column of category labels that have corresponding regular expressions 
# in EVTYPE
SearchEventsCategory <- function(myLabels,myRegEx) {
    starti <- ncol(My_StormData)+1
    for(i in 1:length(myLabels)){
        colname=myLabels[i]
        searchstring=myRegEx[i]
        My_StormData[colname] = str_detect(My_StormData$EVTYPE,searchstring)
    }
    endi=ncol(My_StormData)
    My_StormData$Counted <- My_StormData[,starti:endi] %>% rowSums()
    My_StormData$Category <- apply(My_StormData[,starti:endi],1,function(u) 
        paste(names(which(u)),collapse=','))
    My_StormData$Category <- gsub('^$','Not Categorized',  My_StormData$Category)
    My_StormData$Category
}


###################################
# Examine Data with exact labels
# EVTYPE must be keyed in exactly 
SearchExactRegEx <- paste('^',str_trim(EventTypes),'$',sep='')

# Apply FUnction to Return Category Names 
My_StormData$Exact <- SearchEventsCategory(EventLabels,SearchExactRegEx)

# Report number of cases in each category
My_StormData %>% group_by(Exact) %>% summarise(n=n()) %>% arrange(desc(n))

#Portion of Uncategorized Data: 32.1%
nrow(filter(My_StormData,Exact=='Not Categorized')) / nrow(My_StormData)          



##################
# Define Groups of Event Types in seporate file, export to csv
# Search for sets of strings for a number of event types
getGroupedEvents <- function() {
    # Grouped Events
    url='https://docs.google.com/spreadsheets/d/e/2PACX-1vTSsmEtqEoFPGMpsH9_DaP2_vcZwe5jsynu0KXxv2rLJr14We6CTQTLmCztCjTn9i-snTdKggVuSl5Q/pub?gid=695320465&single=true&output=csv'
    newurl='https://docs.google.com/spreadsheets/d/e/2PACX-1vTSsmEtqEoFPGMpsH9_DaP2_vcZwe5jsynu0KXxv2rLJr14We6CTQTLmCztCjTn9i-snTdKggVuSl5Q/pub?gid=0&single=true&output=csv'
    file='StormEventsGrouped.csv'
    GroupedEvents <- read.csv(newurl,stringsAsFactors = F)
}
GroupedEvents <- getGroupedEvents()


#Create Regular Expression Strings from GroupedEvent info
GroupedRegEx <- apply( GroupedEvents[1:nrow(GroupedEvents),], 2, 
                       paste, collapse= "|" )
GroupedRegEx <- str_replace_all(GroupedRegEx,'[\\|]+$','') #Remove | at the end
#Create Column Names
GroupedLabels <- GroupedEvents %>% colnames()

# Write new function to search for the locations of the regular expressions
# Adds Counted, Category, and whichfirst to dataframe
#Now locate strings to find order  
SearchEventsLocation <- function(myLabels,myRegEx) {
    # loop through groups to search for regex strings and track whether they are found
    print('searching for strings')
    for(i in 1:length(myLabels)){
        colname=myLabels[i]
        searchstring=myRegEx[i]
        searchstringstart=str_locate(My_StormData$EVTYPE,searchstring)[,1]
        My_StormData[colname] = searchstringstart
    }
    starti <<-  match('Cold',names(My_StormData))
    endi <<- match('Winter.Storm',names(My_StormData))
    print('categorizing and counting')
    #track which string match was found first
    My_StormData$whichfirst <- apply(My_StormData[,starti:endi],1, 
                                     function(x) paste(names(which.min(x)),
                                                       collapse=',' ))
    
    # convert to labels to TRUE FALSE
    My_StormData[,starti:endi] <- !is.na(My_StormData[,starti:endi])
    # count number of categories found TRUE
    My_StormData$Counted <- apply(My_StormData[starti:endi],1, 
                                  function(x) length(which(x)))
    # assign category labels (note, can result in multiple labels for one event)
    My_StormData$Category <- apply(My_StormData[,starti:endi],1,
                                   function(u) paste(names(which(u)),
                                                     collapse=','))
    # group events with no category assigned
    My_StormData$Category <- gsub('^$','Not Categorized',  My_StormData$Category)
    My_StormData
}
My_StormData <- SearchEventsLocation(GroupedLabels,GroupedRegEx)
table(My_StormData$Counted)
My_StormData %>% filter(Counted==3) %>% select(EVTYPE,Category) %>% unique()


# Regular Expressions allow for OR opperator but don't use NOT opperator,
# Apply logical nots to make some categories mutually exclusive
My_StormData <- My_StormData %>% mutate(
    Thunderstorm = Thunderstorm & !Tornado,  # 1. If Tornado then not Wind
    Wind = Wind & !Tornado,  # 1. If Tornado then not Wind
    Wind = Wind & !Thunderstorm, # If Thunderstorm Wind then not Wind
    Thunderstorm = Thunderstorm & !Winter.Storm, #frezzing rain
    Thunderstorm = Thunderstorm & !Flood,
    Cold = Cold & !Winter.Storm, # If winter storm then not cold
    Wind = Wind & !Cold, # If cold then not wind (Cold\wind chill)
    Thunderstorm = Thunderstorm & !Cold,
    Cold = Cold & !Flood,
    Tide = Tide & !Flood,
    Heat = Heat & !Drought,
    Tide = Tide & !Tropical,
    Wind = Wind & !Tropical,
    Cold = Cold & !Tornado,
    Tide = Tide & !Heat,   
    Wind = Wind & !Winter.Storm
)

# Count Again 
My_StormData$Counted <- apply(My_StormData[starti:endi],1, 
                              function(x) length(which(x)))
table(My_StormData$Counted)

My_StormData %>% filter(Counted>1) %>% 
    group_by(EVTYPE,Category) %>% 
    summarise(n=n()) %>% 
    arrange(desc(n)) 
#
#My_StormData <- My_StormData %>% mutate(
#    Wind = Wind & !Cold,  # 1. If Cold then not Wind (ACCOUNTS FOR Cold/Wind CHILL)
#    Cold = Cold & !Thunderstorm, # 2. IF PERCIPITATION THEN NOT Cold (FREEZING RAIN)
#    Thunderstorm = Thunderstorm & !Flood, # iF FloodING THEN NOT Thunderstorm
#    Cold = Cold & !Flood, # If Flood NOT Cold
#    Tide = Tide & !Flood, # If Flood NOT Tide
#    Tide = Tide & !Tropical, #IF Tropical NOT TIDAL
#    Wind = Wind & !Tropical, #IF Tropical NOT WIND
#    Cold = Cold & !Tornado #IF Tornado NOT Cold
#    )

#Update Category
My_StormData$Category <- apply(My_StormData[,starti:endi],1,
                               function(u) paste(names(which(u)),
                                                 collapse=','))
My_StormData$Category <- gsub('^$','Not Categorized',  My_StormData$Category)


# For remaining, pick first listed
#My_StormData$Category <- My_StormData$Category
My_StormData[My_StormData$Counted>1,"Category"] <- 
    My_StormData[My_StormData$Counted>1,"whichfirst"]
My_StormData$Category <- factor(My_StormData$Category, c(
    'Tornado','Flood',
    'Tide','Thunderstorm',
    'Winter.Storm','Tropical','Wind',
    'Fire','Cold','Heat','Drought',
    'Debris.Flow','Not Categorized'))


My_StormData$TypeHolder = as.character(My_StormData$Category)

#My_StormData$strmatch <-  grepl('HEAT',My_StormData$EVTYPE)
#My_StormData$Type <- ifelse(My_StormData$strmatch & My_StormData$Category=='Heat',
#                      'Extreme Heat', 
#                       My_StormData$TypeHolder)
#My_StormData$TypeHolder <- My_StormData$Type

run_addsubcat <- function() {
    addsubcat <- function(InCategory,subcat_string,subcat_label) {
        My_StormData$strmatch <-  grepl(subcat_string,My_StormData$EVTYPE)
        My_StormData$Type <- ifelse(My_StormData$strmatch & 
                                        My_StormData$Category==InCategory,
                                    subcat_label, 
                                    My_StormData$TypeHolder)
        My_StormData$TypeHolder <- My_StormData$Type
        My_StormData 
    }
    
    
    My_StormData <- addsubcat('Heat','EXCESSIVE|EXTREME','Excessive Heat')
    
    # My_StormData <- addsubcat('FIRE','WILD','Wildfire')
    
    # My_StormData <- addsubcat('DEBRISFLOW','AVALANCHE','Avalanche')
    # My_StormData <- addsubcat('DEBRISFLOW','LAND','Landslide')
    # My_StormData <- addsubcat('DEBRISFLOW','MUD','Mudslide')
    
    # cold
    My_StormData <- addsubcat('Cold','EXTREME','Extreme Cold')
    #My_StormData <- addsubcat('Cold','FROST|FREEZ','Frost/Freeze')
    
    # Wind
    # My_StormData <- addsubcat('Wind','HIGH','High Wind')
    #My_StormData <- addsubcat('Wind','STRONG','Strong Wind')
    #My_StormData <- addsubcat('Wind','DUST STORM','Dust Storm')
    
    #Thunderstorm
    #My_StormData <- addsubcat('Thunderstorm','HEAVY RAIN','Heavy Rain')
    My_StormData <- addsubcat('Thunderstorm','HAIL','Hail')
    My_StormData <- addsubcat('Thunderstorm','LIGHTNING','Lightning')
    My_StormData <- addsubcat('Thunderstorm','W','TSTM Wind')
    
    # Change these to winter storm, 
    # add Ice Storm, Freezing Rain
    #My_StormData <- addsubcat('Thunderstorm','WINTER STORM','Winter Storm')
    #My_StormData <- addsubcat('Thunderstorm','WINTER WEATHER','Winter Weather')
    #My_StormData <- addsubcat('Thunderstorm','BLIZZARD','Blizzard')
    #My_StormData <- addsubcat('Thunderstorm','HEAVY SNOW','Heavy Snow')
    #My_StormData <- addsubcat('Cold','ICE STORM','Ice Storm')
    #My_StormData <- addsubcat('Cold','FREEZING RAIN','Ice Storm')
    
    #My_StormData <- addsubcat('Tide','SURF','Surf')
    My_StormData <- addsubcat('Tide','RIP CURRENT','Rip Current')
    My_StormData <- addsubcat('Tide','STORM SURGE','Storm Surge')
    #My_StormData <- addsubcat('Tide','TID','Tidal')
    
    My_StormData <- addsubcat('Tropical','HURRICANE|TYPHOON','Hurricane')
    #My_StormData <- addsubcat('Tropical','DEPRESSION','Tropical Depression')
    #My_StormData <- addsubcat('Tropical','Tropical STORM|COASTAL STORM','Tropical Storm')
    #My_StormData <- addsubcat('Tropical','TSUNAME','Tsunami')
    
    #My_StormData <- addsubcat('Flood','Flood','Flood')
    My_StormData <- addsubcat('Flood','FLASH','Flash Flood')
    #My_StormData <- addsubcat('Flood','RIVER','River Flood')
    #My_StormData <- addsubcat('Flood','URBAN','Urban Flood')
    #My_StormData <- addsubcat('Flood','COAST','Coastal Flood')
    
    #My_StormData <- addsubcat('Tornado','Tornado','Tornado')
    #My_StormData <- addsubcat('Tornado','DUST DEVIL','Dust Devil')
    
    My_StormData
}
My_StormData <- run_addsubcat()
table(My_StormData$Category,My_StormData$Type)


My_StormData <- select(My_StormData, REFNUM,EVTYPE,Category,Type,Fatalities,
                       Injuries,PropertyDamage,CropDamage,Cost,REMARKS) 


#Summary Data
MySummary <- My_StormData %>% group_by(Category, Type) %>%
    summarise(Freq = n(),
              Total.Fatalities.Thousands = sum(Fatalities) / 10^3,
              Total.Injuries.Thousands = sum(Injuries) /10^3,
              Total.Cost.Billions = sum(Cost)/10^9
    )




# Set Order for legend
typeorder = c('Tornado','Flash Flood','Flood','Storm Surge','Rip Current','Tide',
              'Hail','Lightning','TSTM Wind','Thunderstorm','Winter.Storm',
              'Hurricane','Tropical','Wind','Fire','Extreme Cold','Cold',
              'Excessive Heat','Heat','Drought','Debris.Flow','Not Categorized')
MySummary$Type <- factor(MySummary$Type, typeorder)




# Choose Colors
set3pal <- brewer.pal(12,'Set3')
set1pal <- brewer.pal(9, 'Set1')
mypal <- c(set3pal,set3pal)[1:length(typeorder)]

mypal <- c('#808E85',set3pal,set1pal)


colorcodes <- data.frame(mypal %>% t)
colnames(colorcodes) <- typeorder

#make function to swap colors
swapcolors <- function(a,b) {
    temp = colorcodes[a]
    colorcodes[a] = colorcodes[b]
    colorcodes[b] = temp
    colorcodes
}
colorcodes = swapcolors('Wind','Fire')
colorcodes = swapcolors('Flood','Flash Flood')
colorcodes = swapcolors('Hurricane','Cold')
colorcodes = swapcolors('Winter.Storm','Extreme Cold')
colorcodes = swapcolors('Wind','Flood')
colorcodes = swapcolors('Wind','Winter.Storm')


#colorcodes = swapcolors('Wind','Cold')
#colorcodes = swapcolors('Wind','Debris.Flow')


mypal <- as.vector(colorcodes %>% t)

MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Injuries.Thousands,
    fill = Type)) + 
    geom_bar(stat='identity',color='darkgray',alpha=.9)  +
    theme(axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.title=element_blank(),
          legend.key.width = unit(0.6, 'cm'),
          legend.key.height= unit(0.6, 'cm'),
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          legend.direction="horizontal",
          legend.background = element_rect(fill=alpha('#ebebeb',0.7))) +
    scale_fill_manual(values=mypal) +
    labs(x='',y='Total Injuries in Thousands')

ggsave('injuries.png')

MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Fatalities.Thousands,
    fill = Type)) + 
    geom_bar(stat='identity',color='darkgray',alpha=.9)  +
    theme(legend.position="none",
          axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values=mypal) +
    labs(x='',y='Total Fatalities in Thousands')

ggsave('fatalities.png')

MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Cost.Billions,
    fill = Type)) + 
    geom_bar(stat='identity',color='darkgray',alpha=.9)  +
    theme(legend.position="none",
          axis.text = element_text(size = 11),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values=mypal) +
    labs(x='',y='Total Cost in Billions')

ggsave('cost.png')

# The Human and Financial Costs of Severe Weather in the USA
Lisa Marie Pritchett  
November 23, 2017  




## Synopsis

#### -_What types of severe weather cause the most damage to human life?_

#### -_What types cost the most financially?_

This document examines data from The National Oceanic and Atmospheric Administration (NOAA) to answer these questions. The NOAA maintains a database of severe weather events from across the USA dating back to 1950 including nearly a million entries and many different types of severe weather. 

This document contains all code needed to download the data, clean it, process it, and produce the figures at the end. The real challenge in this project was transforming the Event Type data from a string variable with hundreds of different values into a factor variable with a limited number of levels. My goal was to represent all of the severe weather types within a readable and attractive figure without throwing out any relevant data. I accomplish this by creating groups of severe weather events, efficiently serach for keywords using regular expressions, and apply conditional logic to arrive at a cleaned factor variable. In the future, this labeled dataset could be used to train a classification algorithm to automatically clean and classify the event types.

I find that Tornados cause by far the most damage to human life, with Heat and Thunderstorm events being the next most dangerous. Floods, Hurricanes, and Tornados cause the most financial damage. 

### Get & Explore Data

The code below checks to see if the file already exists in the working directory before downloading it and reading it into memory.


```r
# import libraries
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(dplyr)


# download data if not in working directory
url  <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
destfile <- 'StormData.bz2'
if(!file.exists(destfile)){
    download.file(url,destfile)
    unlink(url)
}

# read data
StormData <- read.csv(destfile, stringsAsFactors = F) 
```


First, I take a quick look at the kinds of values in the dataset. Each row describes a storm event somewhere in the United States. For this analysis, I keep only rows that are associated with some damage, injury, or death. In addition, I only need the columns regarding Event Type (EVTYPE), Fatalities, Injuries, and Costs. In addition I keep the REFNUM and the REMARKS columns. I discard all other information including the date, time, location, size and magnitude of the storm.



```r
# subset data
My_StormData <- StormData %>%
    filter(PROPDMG != 0 |CROPDMG != 0 | INJURIES != 0 | FATALITIES != 0) %>%
    select(REFNUM,EVTYPE,FATALITIES:PROPDMG,CROPDMG,
    PROPDMGEXP,CROPDMGEXP,REMARKS) %>%
    mutate(EVTYPE = toupper(EVTYPE))
    row.names(My_StormData) <- My_StormData$REFNUM
```

Subsetting the data reduced it from about 
902 thousand  to about 
255 thousand rows, and from
37 to
9 columns
without discarding any information needed.


#### Examine the Types of Events

The NOAA defines 48 unique storm event types and describes them in their publication [here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

Many of the Types seem to be closely related to others. For example we see both "Heat" and "Excessive Heat" so it may be useful to combine them into a larger group. 

Next I examined the values in the column for event type (EVTYPE). Far from the 48 event types, there are 447 unique values in the data. This data needs to be cleaned.

How many of the events in the data set are not labeled with one of the exact Event Types? To help answer this question, I typed the  list of event types from the pdf into EventTypes.txt and read it into R in the code below.


```r
typesfile <- 'NOAAWeatherTypes.txt'
if(!file.exists(typesfile)){
    typesurl = 'https://github.com/lisamariepritchett/StormOutcomes/blob/master/NOAAWeatherTypes.txt'
    download.file(typesurl,typesfile)
    unlink(typesurl)
}
EventTypes <- readLines(typesfile)
print(EventTypes)
```

```
##  [1] "ASTRONOMICAL LOW TIDE "   "AVALANCHE "              
##  [3] "BLIZZARD "                "COASTAL FLOOD "          
##  [5] "COLD/WIND CHILL "         "DEBRIS FLOW "            
##  [7] "DENSE FOG "               "DENSE SMOKE "            
##  [9] "DROUGHT "                 "DUST DEVIL "             
## [11] "DUST STORM "              "EXCESSIVE HEAT "         
## [13] "EXTREME COLD/WIND CHILL"  "FLASH FLOOD"             
## [15] "FLOOD"                    "FREEZING FOG"            
## [17] "FROST/FREEZE"             "FUNNEL CLOUD"            
## [19] "HAIL"                     "HEAT"                    
## [21] "HEAVY RAIN"               "HEAVY SNOW"              
## [23] "HIGH SURF"                "HIGH WIND"               
## [25] "HURRICANE/TYPHOON"        "ICE STORM"               
## [27] "LAKESHORE FLOOD"          "LAKE-EFFECT SNOW"        
## [29] "LIGHTNING"                "MARINE HAIL"             
## [31] "MARINE HIGH WIND"         "MARINE STRONG WIND"      
## [33] "MARINE THUNDERSTORM WIND" "RIP CURRENT"             
## [35] "SEICHE"                   "SLEET"                   
## [37] "STORM TIDE"               "STRONG WIND"             
## [39] "THUNDERSTORM WIND"        "TORNADO"                 
## [41] "TROPICAL DEPRESSION"      "TROPICAL STORM"          
## [43] "TSUNAMI"                  "VOLCANIC ASH"            
## [45] "WATERSPOUT"               "WILDFIRE"                
## [47] "WINTER STORM"             "WINTER WEATHER"
```

Now I can search through the EVTYPE column for cases matching each of the event types. I define a function below that creates a factor variable "Category" with the 48 levels above plus "Not Categorized" for events labeled with any other values. I created a function with the idea that I could try it with many different sets of search strings to best match all the weather events in the database.


```r
# mylabels and myRegEx have an element for every type/category. Searches through
# EVTYPE for regular expressions and creates dummy coded columns and a factor 
# variable and with levels given by myLabels. Note can result in more than 1 
# label to each row depending on regular expressions. Also tracks which regular
# expression is found first and the number of categories assigned to each item

SearchEvents <- function(myLabels,myRegEx) {
    # loop through groups to search for regex strings and track where they are found
    for(i in 1:length(myLabels)){
        colname=myLabels[i]
        searchstring=myRegEx[i]
        searchstringstart=str_locate(My_StormData$EVTYPE,searchstring)[,1]
        My_StormData[colname] = searchstringstart
    }
    # get column numbers for dummy coded columns
    starti <<-  match(myLabels[1],names(My_StormData))
    endi <<- match(myLabels[length(myLabels)],names(My_StormData))
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
```

Now I can use this function with a list of Regular expressions to  match only the exact Event Type strings. All of the events without a label exactly matching one of the Event Types will be placed into a 'Not Categorized' group.


```r
# Create Lables and Regular Expressions from  EventTypes
EventLabels <- EventTypes %>% str_trim() %>% str_replace_all('[\\W+]','')
ExactRegEx <- paste('^',str_trim(EventTypes),'$',sep='')

# Apply SearchEvents function to return Factor variable for Event Type if keyed in exactly
My_StormData <- SearchEvents(EventLabels,ExactRegEx)
```

#### How messy is the Event Type data?

I found that 
32%
of the entries do not have event labels that match the storm events exactly. In addition, the data are collected from many different sources so there is no reason to expect that the messiness is evenly distributed across Event Types. This means that if we just ignore all the entries without exact labels we may be greatly underestimating the impact of some of the weather events.


## Data Processing

#### Define Reasonable Groups

My goal is to convert the messy Event Type column into a true Factor variable with a limited number of levels. 

For my visualization, I'd like all of the data rows to be represented. (I don't want to limit my visualization to just the top 10 weather events, for example). At the same time I want the visualization to be readable and attractive (so I don't want to include all 48 types as separate colors or labels.

There are many different ways the 48 events could be grouped (by season, location, etc.). Over the course of my analysis I experimented with different categorization schemes but ended up with the categorization given below:

- **Tornado**: Tornado, Dust Devil, Funnel Cloud, Water Spout, Gustnado

- **Flood**: Flood, Flash Flood, Coastal Flood, Lakeshore Flood, Seiche

- **Tropical Storm**: Hurricane/Typhoon, Tropical Depression, Tropical Storm, Tsunami

- **Ocean/Tidal**: Astronomical Low Tide, High Surf, Marine Hail, Marine High Wind, Marine Strong Wind, Marine Thunderstorm Wind, Rip Current, Storm Tide

- **Thunderstorm**: Hail, Heavy Rain, Lightning, Thunderstorm Wind, Dense Fog

- **Winter Storm**: Blizzard, Freezing Fog, Freezing Rain, Heavy Snow, Ice Storm, Lake-Effect Snow, Sleet, Winter Storm, Winter Weather

- **Wind**: High Wind, Strong Wind, Dust Storm

- **Fire**: Wildfire, Dense Smoke

- **Heat**: Heat, Excessive Heat

- **Cold:** Cold/Wind Chill, Extreme Cold/Wind Chill, Frost/Freeze

- **Drought**: Drought

- **Debris Flow**: Debris Flow, Volcanic Ash, Avalanche


#### Search string data using regular expressions composed of keywords

Next I experimented with different sets of regular expressions to efficiently  search the database for these groups. Essentially, I came up with a list of keywords for each of the groups that seem to do a good job of classifying the data. I started with keywords from the list of event types and then added other keywords that showed up most frequently, for example, I added "LANDSLIDE" and "MUDSLIDE" (which are not keywords in the published list, but do show up in the data) to the Debris Flow category.

The keywords for each category are available online at my github (https://github.com/lisamariepritchett/StormOutcomes/blob/master/StormEventKeywords.csv) and are printed below:

```r
# First get the groups and keywords from the csv file
keywordsfile = 'StormEventKeywords.csv'
if(!file.exists(keywordsfile)){
    keywordsurl = 'https://github.com/lisamariepritchett/StormOutcomes/blob/master/StormEventKeywords.csv'
    download.file(keywordsurl,keywordsfile)
    unlink(keywordsurl)
}
GroupKeywords <- read.csv(keywordsfile, stringsAsFactors = F)
print(GroupKeywords)
```

```
##               Cold  Debris.Flow Drought  Fire        Flood Heat
## 1             COLD  DEBRIS FLOW DROUGHT  FIRE        FLOOD HEAT
## 2       WIND CHILL VOLCANIC ASH         SMOKE       SEICHE     
## 3            FROST    AVALANCHE                        FLD     
## 4            FREEZ        SLIDE                 HIGH WATER     
## 5  LOW TEMPERATURE        SLUMP                  DAM BREAK     
## 6      HYPOTHERMIA     AVALANCE               RISING WATER     
## 7              ICE                                             
## 8              ICY                                             
## 9            GLAZE                                             
## 10                                                             
## 11                                                             
## 12                                                             
##     Thunderstorm     Ocean_Tidal    Tornado  Tropical       Wind
## 1           HAIL            TIDE DUST DEVIL  TROPICAL       WIND
## 2   THUNDERSTORM            SURF     FUNNEL   TSUNAMI DUST STORM
## 3           RAIN          MARINE WATERSPOUT HURRICANE MICROBURST
## 4      LIGHTNING         CURRENT    TORNADO   TYPHOON  DOWNBURST
## 5            FOG     STORM SURGE   GUSTNADO                     
## 6       LIGHTING         SEAS\\b    TORNDAO                     
## 7      LIGNTNING      ROGUE WAVE    TORNADO                     
## 8  PRECIPITATION           TIDAL  LANDSPOUT                     
## 9         PRECIP          SWELLS                                
## 10  HEAVY SHOWER COASTAL EROSION                                
## 11          TSTM           WAVES                                
## 12                 COASTAL STORM                                
##     Winter.Storm
## 1       BLIZZARD
## 2           SNOW
## 3  FREEZING RAIN
## 4   FREEZING FOG
## 5          SLEET
## 6         WINTER
## 7      ICE STORM
## 8     WINTRY MIX
## 9      HEAVY MIX
## 10              
## 11              
## 12
```


The code below reads the keywords, processes them into regular expressions, and searches for them using my SearchEvents function.


```r
# Combine Keywords into Regular Expressions
GroupedRegEx <- apply( GroupKeywords[1:nrow(GroupKeywords),], 2, 
                       paste, collapse= "|" )
GroupedRegEx <- str_replace_all(GroupedRegEx,'[\\|]+$','') #Remove | at the end

#Create Column Names
GroupedLabels <- GroupKeywords %>% colnames()

# Search for Keywords
My_StormData <- SearchEvents(GroupedLabels,GroupedRegEx)
```


This code results in a classification for all but
58
cases,
34 of which are labeled "other". 

#### Apply conditional logic to make categories mutually exclusive

One problem remains: 
48% 
of the cases are placed into more than one category. In correcting for this, I used the following query to see what EVTYPE values were being classified under multiple categories:


```r
filter(My_StormData,Counted>1) %>% group_by(Category,EVTYPE) %>% summarise(n=n()) %>% arrange(desc(n))
```

```
## # A tibble: 145 x 3
## # Groups:   Category [27]
##                         Category                  EVTYPE     n
##                            <chr>                   <chr> <int>
##  1             Thunderstorm,Wind               TSTM WIND 63235
##  2             Thunderstorm,Wind       THUNDERSTORM WIND 43655
##  3             Thunderstorm,Wind      THUNDERSTORM WINDS 12086
##  4             Cold,Winter.Storm               ICE STORM   708
##  5             Thunderstorm,Wind          TSTM WIND/HAIL   441
##  6                     Cold,Wind EXTREME COLD/WIND CHILL   111
##  7 Thunderstorm,Ocean_Tidal,Wind        MARINE TSTM WIND   109
##  8                     Cold,Wind         COLD/WIND CHILL    90
##  9              Ocean_Tidal,Wind      MARINE STRONG WIND    46
## 10             Thunderstorm,Wind THUNDERSTORM WINDS HAIL    40
## # ... with 135 more rows
```
We see that Thunderstorm Wind events account for many of the cases which have been placed into more than one category. I can very quickly correct these mistakes using conditional logic. This works so easily because I have boolean ("dummy coded") columns for each group.


```r
My_StormData <- My_StormData %>% mutate(Wind = Wind & !Thunderstorm)
```

The above statement makes the Wind and Thunderstorm categories mutually exclusive. Any events that were categorized as both Wind and Thunderstorm before will now be only called Thunderstorm. Changes like this were made carefully by hand by considering what events should be placed in what group.

I can check the list of EVTYPE labels assigned to a given category using the following:


```r
My_StormData %>% filter(Wind) %>% group_by(EVTYPE) %>% summarise(n=n()) %>% arrange(desc(n))
```

```
## # A tibble: 59 x 2
##                     EVTYPE     n
##                      <chr> <int>
##  1               HIGH WIND  5522
##  2             STRONG WIND  3372
##  3              HIGH WINDS   657
##  4 EXTREME COLD/WIND CHILL   111
##  5              DUST STORM   103
##  6         COLD/WIND CHILL    90
##  7                    WIND    84
##  8          DRY MICROBURST    78
##  9            STRONG WINDS    52
## 10      MARINE STRONG WIND    46
## # ... with 49 more rows
```

This helps to confirm that each group is composed of the intended set of EVTYPE labels. Below I run a batch of conditional statements to account for most of the events categorized more than once.


```r
My_StormData <- My_StormData %>% mutate(
    Wind = Wind & !Cold, # Cold/Wind Chill is Cold not Wind
    Cold = Cold & !Winter.Storm, #  ICE STORM, SNOW/ICE are WinterStorm 
    Wind = Wind & !Ocean_Tidal, # MARINE WINDS are Ocean/Tidal
    Wind = Wind & !Tornado,
    Thunderstorm = Thunderstorm & !Winter.Storm,
    Thunderstorm = Thunderstorm & !Ocean_Tidal,
    Thunderstorm = Thunderstorm & !Flood,
    Cold = Cold & !Flood,
    Ocean_Tidal = Ocean_Tidal & !Flood,
    Ocean_Tidal = Ocean_Tidal & !Tropical,
    Wind = Wind & !Winter.Storm,
    Cold = Cold & !Tornado,
    Ocean_Tidal=Ocean_Tidal & !Heat,
    Thunderstorm = Thunderstorm & !Tornado
    )

# Count Again
My_StormData$Counted <- apply(My_StormData[starti:endi],1, 
                              function(x) length(which(x)))

#Update Category 
My_StormData$Category <- apply(My_StormData[,starti:endi],1,
                               function(u) paste(names(which(u)),
                                                 collapse=','))
# Add Not Categorized
My_StormData$Category <- gsub('^$','Not Categorized',  My_StormData$Category)
```


This results in 
99.97% 
of the data being classified with exactly 1 label. The last 
24 
cases that have more than one type listed can be treated as whichever type was listed first:


```r
My_StormData[My_StormData$Counted>1,"Category"] <- 
    My_StormData[My_StormData$Counted>1,"whichfirst"]
```


#### Labeling Specific Event Types

For my plot I want to choose a few specific event types to include within the larger groups. For example, I want to show Flash Floods within the Flood group and Hurricanes within the Tropical Storm group. Here I make a function to add Types by searching within Categories for keywords. I then call it for each subtype I wish to add. 


```r
# create a function to easily add levels to a new factor v
addsubcat <- function(InCategory,subcat_string,subcat_label) {
    My_StormData$strmatch <-  grepl(subcat_string,My_StormData$EVTYPE)
    My_StormData$Type <- ifelse(My_StormData$strmatch & 
                                    My_StormData$Category==InCategory,
                                subcat_label, 
                                My_StormData$TypeHolder)
    My_StormData$TypeHolder <- My_StormData$Type
    My_StormData 
}

My_StormData$TypeHolder <- My_StormData$Category
My_StormData <- addsubcat('Heat','EXCESSIVE|EXTREME','Excessive Heat')
My_StormData <- addsubcat('Cold','EXTREME','Extreme Cold')
My_StormData <- addsubcat('Thunderstorm','HAIL','Hail')
My_StormData <- addsubcat('Thunderstorm','LIGHTNING','Lightning')
My_StormData <- addsubcat('Thunderstorm','WIND', 'TSTM Wind') #?
My_StormData <- addsubcat('Ocean_Tidal','RIP CURRENT','Rip Current')
My_StormData <- addsubcat('Ocean_Tidal','STORM SURGE','Storm Surge')
My_StormData <- addsubcat('Tropical','HURRICANE|TYPHOON','Hurricane')
My_StormData <- addsubcat('Flood','FLASH','Flash Flood')
```

Now I can make Type and Category Factor variables and set the order they will appear in the figure


```r
# Make Type a factor and set order
typeorder <- c('Tornado', 'Flash Flood', 'Flood', 'Storm Surge', 'Rip Current', 
    'Ocean_Tidal', 'Hail', 'Lightning', 'TSTM Wind', 'Thunderstorm', 
    'Winter.Storm','Hurricane', 'Tropical', 'Wind','Fire', 
    'Extreme Cold', 'Cold','Excessive Heat','Heat', 'Drought', 
    'Debris.Flow', 'Not Categorized')

My_StormData$Type <-  factor(My_StormData$Type, typeorder)

# Rename some levels
levels(My_StormData$Type) <- c(
    'Tornado', 'Flash Flood', 'Flood', 'Storm Surge', 'Rip Current', 
    'Ocean/Tidal', 'Hail', 'Lightning', 'TSTM Wind', 'Thunderstorm', 
    'Winter Storm','Hurricane', 'Tropical', 'Wind','Fire', 
    'Extreme Cold', 'Cold','Excessive Heat','Heat', 'Drought', 
    'Debris Flow', 'Not Categorized')

# make category a factor
My_StormData$Category <- factor(My_StormData$Category, c(
    'Tornado','Flood','Ocean_Tidal','Thunderstorm',
    'Winter.Storm', 'Tropical', 'Wind', 'Fire', 'Cold',
    'Heat', 'Drought','Debris.Flow','Not Categorized'))

# rename some levels
levels(My_StormData$Category) <- c( 
    'Tornado', 'Flood' , 'Ocean/Tidal','Thunder- storm', 'Winter Storm',
    'Tropical Storm', 'Wind', 'Fire', 'Cold', 'Heat', 'Drought',
    'Debris Flow', 'Not Categorized')
```

### Clean Value Columns

Next I examined the value columns (fatalities, injuries, and financial cost). 

#### Fatalities and Injuries 

I find that the fatalities and injuries don't need cleaning. Both hold only positive integer values and zeros. 

The fatalities data do appear to have one outlier. A Heat event in Chicago resulted in 583 deaths (where the next highest is 158). The REMARKS column authenticates this value with a news story on the event.

Both the fatalities and injuries columns are kept unchanged.

#### Financial Costs

The financial costs include Property Damage and Crop Damage. For this analysis, they will be combined into one value for the Financial Cost. 

Property damage and Crop Damage are each coded using 2 columns, one gives a value between 0 and 999 and the second gives the "exponent" to apply to that number. For example, if the property value with a given storm event were $500,000 it would be recorded as (500, 'K') indicating 500 thousand dollars. The most common exponent values were 'K' and 1. Examining the REMARKS column led to the conclusion that both of those represented thousands of dollars. 

The following code replaces the exponent values with the proper number to be used in exponential notation (e.g. 3 to represent thousands). 

Finally, in the code below Property and Crop damage are calculated and combined to represent the total cost of each storm event.


```r
# Convert strings into exponents. Values determined by careful inspection of REMARKS
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Kk]','3')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Hh]','2')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Mm]','6')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'[Bb]','9')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'\\+|\\-','3')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'^$','3')
My_StormData$PROPDMGEXP <- str_replace_all(My_StormData$PROPDMGEXP,'0','3')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Kk]','3')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Hh]','2')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Mm]','6')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'[Bb]','9')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'\\?','3')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'^$','3')
My_StormData$CROPDMGEXP <- str_replace_all(My_StormData$CROPDMGEXP,'0','3')

# A small number of values of PROPDMG are > 999.
My_StormData[My_StormData['PROPDMG'] > 999,'PROPDMGEXP'] = 0

#Exponents are now all numeric so convert type
My_StormData$PROPDMGEXP <- as.numeric(My_StormData$PROPDMGEXP)
My_StormData$CROPDMGEXP <- as.numeric(My_StormData$CROPDMGEXP)

# Calculate Dollar Amounts
My_StormData$PropertyDamage <- apply(My_StormData[,5:8],1,
                                     function(p) p[1]*10^p[3])
My_StormData$CropDamage <- apply(My_StormData[,5:8],1,
                                 function(p) p[2]*10^p[4])
# Combine into Financial Cost
My_StormData$Cost <- My_StormData$PropertyDamage + My_StormData$CropDamage
```


## Results

### Make Summary

With all of the data cleaned I am ready to make a summary data table. The code below calculates the total fatalities, injuries, and costs for each level of my factors for Category and Type.


```r
My_StormData <- select(My_StormData, REFNUM,EVTYPE,Category,Type,Fatalities=FATALITIES,
                       Injuries=INJURIES,PropertyDamage,CropDamage,Cost,REMARKS) 

MySummary <- My_StormData %>% group_by(Category, Type) %>%
    summarise(
        Freq = n(),
        Total.Fatalities = sum(Fatalities),
        Total.Injuries = sum(Injuries),
        Total.Cost.Billions = sum(Cost) / 10 ^ 9
    )
```



### Choose Colors for Plot

I want to use colors to code Event Type, it has 22 levels. The Rcolorbrewer package makes quick attractive color palettes but for only up to 12 levels. Here I combined two palettes and add colors by hand to make the needed number number of colors. Then, I make a swap colors function to quickly fine tune the color scheme (such as making Fire red instead of blue).


```r
# Choose Colors
set3pal <- brewer.pal(12,'Set3')
set1pal <- brewer.pal(9, 'Set1')
mypal <- c('#808E85',set3pal,set1pal)

# Make a dataframe to match colors with type labels
colorcodes <- data.frame(mypal %>% t)
colnames(colorcodes) <- typeorder

# Swap some colors
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

# My Pallete
mypal <- as.vector(t(colorcodes))
```


### Make Plots

Finally, I make and save the plots


```r
MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Injuries,
    fill = Type)) + 
    geom_bar(stat='identity',color='darkgray',alpha=.9)  +
    scale_x_discrete(labels = function(Category) str_wrap(Category, width = 5)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=6.5),
          axis.title.y  = element_text(angle=0, vjust = 0.5, size = 9),
          legend.text = element_text(size=7),
          legend.title=element_blank(),
          legend.position = c(1,1),
          legend.justification=c(1,1),
          legend.direction="horizontal",
          legend.background = element_rect(fill=alpha('#ebebeb',0.7))) +
    scale_fill_manual(values=mypal) +
    labs(x='',y='Total\nInjuries', 
         subtitle = 'Total Injuries related to each type of severe weather in the NOAA database')

#ggsave('injuries.png', width=7, height=3.5, units=c('in'))


MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Fatalities,
    fill = Type)) + 
    geom_bar(stat='identity',color='darkgray',alpha=.9)  +
    scale_x_discrete(labels = function(Category) str_wrap(Category, width = 5)) +
    theme(legend.position="none",
          axis.title.y  = element_text(angle=0, hjust = 0.5, vjust = 0.5, size = 9),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0.5, size=6.5)) +
    scale_fill_manual(values=mypal) +
    labs(x='',y='Total\nFatalities',
         subtitle = 'Total Fatalities related to each type of severe weather in the NOAA database')

#ggsave('fatalities.png', width=7, height=3.5, units=c('in'))

MySummary %>%  ggplot(aes(
    x = Category,
    y = Total.Cost.Billions,
    fill = Type)) + 
    geom_bar(stat='identity',color='lightgray',alpha=.9,size=.3)  +
    scale_x_discrete(labels = function(Category) str_wrap(Category, width = 5)) +
    theme(legend.position="none",
          axis.title.y  = element_text(angle=0, hjust = 0, vjust = 0.5, size = 9),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0.5,size = 6.5))+
    scale_fill_manual(values=mypal) +
    labs(x='',y='Billions\n of USD',
         subtitle = 'Total Cost in Billions of Dollars related to each type of severe weather in the NOAA database')



#ggsave('cost.png', width=7, height=3.5, units=c('in'))
```

![](StormOutcomes_files/figure-html/makePlots-1.png)![](StormOutcomes_files/figure-html/makePlots-2.png)![](StormOutcomes_files/figure-html/makePlots-3.png)

We can see that Tornados cause by far the most injuries and deaths in the USA. Heat causes the second most deaths, and Thunderstorms cause the second most injuries. Every kind of severe weather category examined here is associated with some injuries or fatalities. 

Floods cause the most Financial damage, followed by Tropical Storms (especially Hurricanes), Tornados, other Ocean/Tidal events, Thunderstorms, and Winter Storms. The weather types causing almost nothing financially are Cold, Heat, and Debris Flow.


Thank you for reading! Please let me know if you have any comments or questions. 
Connect with me on [LinkedIn](https://www.linkedin.com/in/lisamariepritchett/)

--

Lisa M. Pritchett is the sole author and analyst of this report. This report was completed on 2017-11-24 for Reproducible Research, Johns Hopkins University, Coursera.



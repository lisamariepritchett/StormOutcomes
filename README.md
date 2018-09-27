# StormOutcomes

### Data Cleaning and Visualization Project

This is a data cleaning and visualization project using data from the National Oceanic and Atmospheric Administration (NOAA). They maintain a database of all of the severe weather events in the USA dating back to 1950.

__What types of events cause the most financial and monetary damage?__

To answer this question we need to have a clear idea of what types of weather there are. We also need to have data clearly labeled into those types. The NOAA defines 48 types of events but there are more than 400 unique values for type in the database. I define 12 types that include the 48 types as subtypes. I clean the human entered data using Regular Expressions and conditional logic. I visualize the human and finanicial costs of sever weather using _ggplot_

My report is hosted at: http://rpubs.com/lisamariepritchett/333446. 


![Injuries](https://github.com/lisamariepritchett/StormOutcomes/blob/master/StormOutcomes_files/figure-html/makePlots-1.png)
![Fatalities](https://github.com/lisamariepritchett/StormOutcomes/blob/master/StormOutcomes_files/figure-html/makePlots-2.png)
![Financial](https://github.com/lisamariepritchett/StormOutcomes/blob/master/StormOutcomes_files/figure-html/makePlots-3.png)
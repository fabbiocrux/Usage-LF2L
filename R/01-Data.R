library(tidyverse)
library(reshape2)
library(ical)
library(calendar)


# Reading LF2L ICS
char_vec <- readLines("Data/2023-05-09-Cal.ics")
LF2L.Calendar <- ical_parse_df(text = char_vec)


# Selecting the columns
LF2L.Calendar <- 
   LF2L.Calendar %>% select(start,end,summary,description)

# Naming the columns
colnames(LF2L.Calendar)=c("Start", "End", "Summary","Description" )



# Formatting the Calendar
LF2L.Calendar$Year <- format( as.Date(LF2L.Calendar$Start) , "%Y")
LF2L.Calendar$Month <- format( as.Date(LF2L.Calendar$Start) , "%B")
LF2L.Calendar$Month <- 
   factor(LF2L.Calendar$Month, levels=month.name, labels=month.abb)

levels(LF2L.Calendar$Month)

rm(char_vec) # removing raw lecture

# Making calculations of the dates

## Start
LF2L.Calendar$Start <- 
   as.POSIXct(LF2L.Calendar$Start, format = "%d-%m-%Y %H:%M:%S")

## End
LF2L.Calendar$End <- 
   as.POSIXct(LF2L.Calendar$End, format = "%d-%m-%Y %H:%M:%S")

## Identifying the 
LF2L.Calendar <-  LF2L.Calendar %>% mutate(Hours= (End-Start)/3600 )

LF2L.Calendar <- LF2L.Calendar[ order(LF2L.Calendar$Start , decreasing = TRUE ),]

## Summary and Description
LF2L.Calendar$Summary <- tolower(LF2L.Calendar$Summary)
LF2L.Calendar$Description <- tolower(LF2L.Calendar$Description)



## Identifying the Scholar year
LF2L.Calendar <- 
   LF2L.Calendar %>% 
   mutate(Scholar.year = case_when(
      (Year == 2024 & as.integer(Month) < 9)    ~ '2023 - 2024',        
      (Year == 2023 & as.integer(Month) >= 9)   ~ '2023 - 2024',
      (Year == 2023 & as.integer(Month) < 9)    ~ '2022 - 2023',       
      (Year == 2022 & as.integer(Month) >= 9)   ~ '2022 - 2023',
      (Year == 2022 & as.integer(Month) < 9)    ~ '2021 - 2022',
      (Year == 2021 & as.integer(Month) >= 9)   ~ '2021 - 2022',
      (Year == 2021 & as.integer(Month) < 9)    ~ '2020 - 2021',
      (Year == 2020 & as.integer(Month) >= 9)   ~ '2020 - 2021',
      (Year == 2020 & as.integer(Month) < 9)    ~ '2019 - 2020',
      (Year == 2019 & as.integer(Month) >= 9)   ~ '2019 - 2020',      
      (Year == 2019 & as.integer(Month) < 9)    ~ '2018 - 2019',
      (Year == 2018 & as.integer(Month) >= 9)   ~ '2018 - 2019',            
      (Year == 2018 & as.integer(Month) < 9)    ~ '2017 - 2018',
      (Year == 2017 & as.integer(Month) >= 9)   ~ '2017 - 2018',                  
      (Year == 2017 & as.integer(Month) < 9)    ~ '2016 - 2017',
      (Year == 2016 & as.integer(Month) >= 9)   ~ '2016 - 2017',                        
      (Year == 2016 & as.integer(Month) < 9)    ~ '2015 - 2016',
      (Year == 2015 & as.integer(Month) >= 9)   ~ '2015 - 2016',                              
      (Year == 2015 & as.integer(Month) < 9)    ~ '2014 - 2015',
      (Year == 2014 & as.integer(Month) >= 9)   ~ '2014 - 2015',                              
      TRUE ~ " ")
   ) 

LF2L.Calendar <- tibble(LF2L.Calendar) %>% filter(Year >=2014)

# Chaging the order
LF2L.Calendar <- 
   LF2L.Calendar %>% 
   mutate(Month = factor(Month, 
                         levels=c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
   ))


# Filtering the years 2020-2023
LF2L.Calendar <- 
   LF2L.Calendar %>% 
   filter(Scholar.year %in% c("2023 - 2024", "2022 - 2023", "2021 - 2022", "2020 - 2021", "2019 - 2020"))

# As numeric the hoours
LF2L.Calendar <- 
   LF2L.Calendar %>% mutate(Hours = as.numeric(Hours)) 
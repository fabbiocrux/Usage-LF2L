
#Magie For Bejamin. by Fabio



#Loading Packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(ical)
library(calendar)
library(tidyverse)

# Reading data
Data=read.csv2("Data-2015/Data.csv", header=TRUE, stringsAsFactors = FALSE, skip=4)           
names(Data)

# Reading data
Zenkit=read.csv("Zenkit.csv", header=TRUE, stringsAsFactors = FALSE)           


Test=Data%>% select(Date.de.début, Date.de.Fin,Horaire.début,Horaire.de.fin) 

Test=Test%>%mutate(Date= )

Test$Date.de.début=gsub("-janv.-", "/01/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-janv.-", "/01/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-févr.-", "/02/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-févr.-", "/02/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-mars-", "/03/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-mars-", "/03/", Test$Date.de.Fin)

Test$Date.de.début=gsub("-avr.-", "/04/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-avr.-", "/04/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-mai-", "/05/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-mai-", "/05/", Test$Date.de.Fin)

Test$Date.de.début=gsub("-juin-", "/06/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-juin-", "/06/", Test$Date.de.Fin)

Test$Date.de.début=gsub("-juil.-", "/07/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-juil.-", "/07/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-août-", "/08/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-août-", "/08/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-sept.-", "/09/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-sept.-", "/09/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-oct.-", "/10/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-oct.-", "/10/", Test$Date.de.Fin)


Test$Date.de.début=gsub("-nov.-", "/11/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-nov.-", "/11/", Test$Date.de.Fin)



Test$Date.de.début=gsub("-déc.-", "/12/", Test$Date.de.début)
Test$Date.de.Fin=gsub("-déc.-", "/12/", Test$Date.de.Fin)

Test=Test[-c(130:506), ]



Test2=Test%>% mutate(Fecha=as.Date(Test$Date.de.début, format = "%d.%m.%Y"))



Test=Test%>%mutate(Date= paste(Date.de.début,Horaire.début,"-", Date.de.Fin, Horaire.de.fin ))

write.csv(Test, file = "Test-Zenkit.csv")


help(as.Date)


dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")


   


   




write.csv(LF2L.Calendar, file = "LF2L.Calendar.csv")


# FROM ENSGSI
# Reading ICS
ENSGSI <- readLines("ENSGSI.ics")
X=ical_parse(text = ENSGSI)
names(X)

x = readLines("ENSGSI.ics")
ENSGSI.Calendar = ic_dataframe(x)
names(ENSGSI.Calendar)

ENSGSI.Calendar$LOCATION=as.factor(ENSGSI.Calendar$LOCATION)

levels(ENSGSI.Calendar$LOCATION)

ENSGSI.Calendar=ENSGSI.Calendar%>%filter( str_detect(LOCATION, 'LF2L'))

L=as.data.frame(ENSGSI.Calendar$LOCATION)


#Dataframe
Summary <- as.data.frame(X[['summary']])
Description <- as.data.frame(X[['description']])
Start <- as.data.frame(X[['start']])
End <- as.data.frame(X[['end']])


ENSGSI.Calendar = cbind(Start, End , Summary, Description )
colnames(ENSGSI.Calendar)=c("Start", "End", "Summary","Description" )





# download.file("long_url", "inst/extdata/example.ics")
ical_example = readLines(system.file("extdata", "example.ics", package = "ical"))
# usethis::use_data(ical_example)


ic_dataframe(ical_example)
ic_dataframe(ical_outlook)

ics_file <- system.file("extdata", "england-and-wales.ics", package = "calendar")

x = data.frame(x_df)
x_df2 = ic_dataframe(x)
identical(x, x_df2)


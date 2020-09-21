#Load dataset

#Check each column for missing values and/or NAs and drop the observations that have them.

#Change the object class of the column containing the dates to object of “class” = Date.
UN_CS112_Data$Last_Reporting_Date <- as.Date(UN_CS112_Data$Last_Reporting_Date, "%d/%m/%Y")
class(UN_CS112_Data$Last_Reporting_Date)
##Here I called the the Last_Reporting_Data Column from the UN_CS112 data-set, then, assigning it as an object, used as.Date to convert it to date given the formula in the instructions. To check, I did class and the object which returned [Date].

#Was that goal achieved for the latest month reported (July 2020)?
July2020 <- UN_CS112_Data[UN_CS112_Data$Last_Reporting_Date >= "2020-07-31" & UN_CS112_Data$Last_Reporting_Date <= "2020-07-31",]
##I created a subset for the month of July from the data-set, assigning it July2020. I wasn't sure how to use the subset function so I improvised with this approach which took an inclusive sample from the Date column in the dataset. Parameters for the sample were the final day of report for July; 31|07|2020. The same approach was used to create a sample for 2017, 2018, and 2019.
FormedPolice_July2020 <- subset(July2020, Personnel_Type=="Formed Police Units")
##Next, from the July2020 sample, I isolated those whose Personnel Type fit the criteria "Formed Police Units". The same approach was used for 2017, 2018, and 2019.
PercentageFormedPolice_July2020 <- 100*sum(FormedPolice_July2020$Female_Personnel)/sum(FormedPolice_July2020$Female_Personnel + FormedPolice_July2020$Male_Personnel)
##After this, I manually calculated the percentage that were female using the formula 100*x[x being total females from sample]/y[being the total males and females from sample]. The same approach was used for 2017, 2018, and 2019.
AllYear2017 <- UN_CS112_Data[UN_CS112_Data$Last_Reporting_Date >= "2017-01-31" & UN_CS112_Data$Last_Reporting_Date <= "2017-12-31",]
FormedPolice_2017 <- subset(AllYear2017, Personnel_Type=="Formed Police Units")
PercentageFormedPolice_2017 <- 100*sum(FormedPolice_2017$Female_Personnel)/sum(FormedPolice_2017$Female_Personnel + FormedPolice_2017$Male_Personnel)
AllYear2018 <- UN_CS112_Data[UN_CS112_Data$Last_Reporting_Date >= "2018-01-31" & UN_CS112_Data$Last_Reporting_Date <= "2018-12-31",]

PercentageFormedPolice_2018 <- 100*sum(FormedPolice_2018$Female_Personnel)/sum(FormedPolice_2018$Female_Personnel + FormedPolice_2018$Male_Personnel)
AllYear2019 <- UN_CS112_Data[UN_CS112_Data$Last_Reporting_Date >= "2019-01-31" & UN_CS112_Data$Last_Reporting_Date <= "2019-12-31",]
FormedPolice_2019 <- subset(AllYear2019, Personnel_Type=="Formed Police Units")
PercentageFormedPolice_2019 <- 100*sum(FormedPolice_2019$Female_Personnel)/sum(FormedPolice_2019$Female_Personnel + FormedPolice_2019$Male_Personnel)
PercentageFormedPolice_2017 <- round(PercentageFormedPolice_2017,digits=1)
##After calculating percentages for 2017, 2018, 2019, and July2020, I rounded the values off to 1 decimal place.
PercentageFormedPolice_2018 <- round(PercentageFormedPolice_2018,digits=1)
PercentageFormedPolice_2019 <- round(PercentageFormedPolice_2019,digits=1)
PercentageFormedPolice_July2020 <- round(PercentageFormedPolice_July2020,digits=1)
FP_PercentFemale_df <- data.frame(years = c("2017", "2018", "2019", "2028", "2020July"), percent_female = c(PercentageFormedPolice_2017, PercentageFormedPolice_2018, PercentageFormedPolice_2019, "20", PercentageFormedPolice_July2020))
head (FP_PercentFemale_df)
##I then created a data.frame to reflect that July2020 exceeded 2019, but did not meet the doubling projecting of 20%

#To drive the point home, replicate the bar plot from the previous step (the one entitled 'WOMEN SERVING IN FORMED POLICE UNITS'), but this time include the July 2020 percentage as a new bar on the chart.
FP_PercentFemale_df$percent_female <- as.numeric(as.vector(FP_PercentFemale_df$percent_female))
##I ran into trouble when I attempted to create the bar plot so I had to conver the above column from my dataframe to a numeric class before proceeding.
##next I installed ggplot2, install.packages("ggplot2")
library(ggplot2)
UN_Bar_Plot <- ggplot (data = FP_PercentFemale_df, aes(x=years, y=percent_female)) + geom_bar(stat="identity") + geom_bar(stat="identity", fill="steelblue") + geom_text(aes(label=percent_female), vjust=1.6, color="white", size=3.5)
UN_Bar_Plot
##Using the formula provided, I then created the bar plot.

#Write a function that takes as input a country’s ISO code and returns (in any nice-looking format):
#(a) a list of all the unique missions that the country has sent personnel to for the whole timeline of the dataset 
#(b) the number of unique missions that the country has sent personnel to for the whole timeline of the dataset
Minerva_Countries <- subset(UN_CS112_Data, ISOCode3=="USA" | ISOCode3=="KOR" | ISOCode3=="IND" | ISOCode3=="DEU" | ISOCode3=="ARG" | ISOCode3=="GBR")
##First I create a composite subset of the initial data set with the relevant information.
trimws(Minerva_Countries$Mission_Acronym, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
##As per instructions, removed excess spaces from Mission_Acronym column
Minera_Countries_df <- as.data.frame(Minerva_Countries)
##Converts dataset to dataframe


#Obtain a dataframe that contains the total number of personnel, both male and female, stationed at MINUSMA during its whole duration, from all countries across all personnel types.
trimws(UN_CS112_Data$Mission_Acronym, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
PrelimUNStat <- subset(UN_CS112_Data, Mission_Acronym == "MINUSMA", select = c("Male_Personnel", "Female_Personnel", "Last_Reporting_Date"))
PrelimUNStat$TotalP <- rowSums( PrelimUNStat[,1:2] )
UNStatFinal <- subset(PrelimUNStat, select = c("Last_Reporting_Date", "TotalP"))
#(a) Across time, how much total personnel was stationed there on average?
#(a) What was the median? 
UNStat_Mean <- mean(UNStatFinal$TotalP, trim=0, na.rm = FALSE)
#(a) Across time, 114.0 Perosonnel were stationed there on average.
UNStat_Median <- median(UNStatFinal$TotalP, na.rm = FALSE)
#(a) The median value is 8 personnel
UNStat_Sorted <- sort(UNStatFinal$TotalP)
quantile(UNStat_Sorted, 0.25)
#This returns 25% | 2 
quantile(UNStat_Sorted, 0.75)
#This returns 75% | 52
min(UNStat_Sorted)
#This gives the value 1, which is a heavily recurring value.
MinRange <- subset(UNStatFinal, TotalP=="1", Select=c("Last_Reporting_Date", "TotalP") )
#This ranged from 31 of July 2013 to 31 of July 2020.
max(UNStat_Sorted)
#The value here is 1726.
MaxRange <- subset(UNStatFinal, TotalP=="1726", Select=c("Last_Reporting_Date", "TotalP"))
#This happened on the 31 of January 2017.

# ANA 515 Practicum Assignment
# Due date : 3/5/2023
# Name : Jie Hui Ho 1099872

# install package(s) to be used
install.packages("readxl")

# retrieve libraries to be used
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

# read in datafile 1 from github repo
url1718 <- 'https://github.com/dr3am05/ANA-515-Practicum/blob/main/G&T%20Results%202017-18%20(Responses)%20-%20Form%20Responses%201.xlsx?raw=true'
destfile1718 <- tempfile ()
download.file (url1718, destfile1718, mode = 'wb')
results1718raw <- read_excel(destfile1718, sheet = 'in')
results1718 <- data.frame(results1718raw)

# read in datafile 2 from github repo
url1819 <- 'https://github.com/dr3am05/ANA-515-Practicum/blob/main/G&T%20Results%202018-19%20Responses%20-%20Sheet1.xlsx?raw=true'
destfile1819 <- tempfile ()
download.file (url1819, destfile1819, mode = 'wb')
results1819raw <- read_excel(destfile1819, sheet = 'in')
results1819 <- data.frame(results1819raw)

# remove the last two columns in datafile 2 that didn't have a header in original file
results1819 <- results1819 %>% select(-c(...13,...14))

# convert datafile 1 Timestamp to date format only
results1718 <- results1718 %>% mutate(Timestamp=as.Date(as.POSIXct(Timestamp, format = "%Y-%m-%d %I:%M:%S")))

# combine two datafiles
results <- rbind(results1718,results1819)

summary(results)


###
### Remove all periods from column names
names(results) <- gsub("\\.", "", names(results))


###
### Cleaning Timestamp
# quick summary of the column
summary(results$Timestamp)

# remove the rows that look like outliers and create a new column for year
results <- results %>% filter(!(format(results$Timestamp, format="%Y")) %in% c(1899,2718)) %>%
                       mutate(TimestampYear = format(results$Timestamp, format="%Y"))

# look at the cleaned summary
summary(results$Timestamp)


###
### Cleaning EnteringGradeLevel
# group by EnteringGradeLevel column to see the different variables setup
Grade <- results %>% group_by(EnteringGradeLevel) %>% summarise(count=n())
# view table
Grade

# cleaned up all the different combinations
results <- results %>% mutate(EnteringGradeLevel = ifelse(EnteringGradeLevel == '1',"first",ifelse(EnteringGradeLevel == '2',"second",
                                                   ifelse(EnteringGradeLevel == '3',"third",ifelse(EnteringGradeLevel == 'k'|EnteringGradeLevel == 'K'|EnteringGradeLevel == 'kinder'|EnteringGradeLevel == 'Kinder'|
                                                   EnteringGradeLevel == 'Kindergarten',"kindergarten",EnteringGradeLevel)))))

# group by EnteringGradeLevel column to check if the ifelse statement works
Grade <- results %>% group_by(EnteringGradeLevel) %>% summarise(count=n())
# view table
Grade


###
### Cleaning BirthMonth
# group by BirthMonth column to see the different variables setup
BirthMonth <- results %>% group_by(BirthMonth) %>% summarise(count=n())
# view table
BirthMonth

# cleaned up all the different combinations
results <- results %>% mutate(BirthMonth = ifelse(BirthMonth == '2'|BirthMonth == "Febrauary"|BirthMonth == "Feb","February",
                                           ifelse(BirthMonth == '8',"August",ifelse(BirthMonth =='september',"September",
                                           ifelse(BirthMonth == '11',"November",ifelse(BirthMonth == '12',"December",BirthMonth))))))

# group by BirthMonth column to check if the ifelse statement works
BirthMonth <- results %>% group_by(BirthMonth) %>% summarise(count=n())
# view table
BirthMonth


###
### Cleaning OLSATVerbalScore
results <- results %>% mutate(OLSATVerbalScore = ifelse(grepl('/30',OLSATVerbalScore) == "TRUE", substring(OLSATVerbalScore,1,nchar(OLSATVerbalScore)-3),
                                                 ifelse(grepl('Fill out later',OLSATVerbalScore) == "TRUE", 0,
                                                 ifelse(grepl("[0-9]",OLSATVerbalScore) == "TRUE", OLSATVerbalScore,
                                                 0))))

# convert dataframe column to numeric
results$OLSATVerbalScore = as.numeric(as.character(results$OLSATVerbalScore))

# convert data with decimal points and data not in the same marking system to be consistent
results <- results %>% mutate(OLSATVerbalScore = ifelse(OLSATVerbalScore>30|OLSATVerbalScore<1, round(OLSATVerbalScore*0.3), OLSATVerbalScore))                                                       

# check if cleaning steps work as expected
summary(results$OLSATVerbalScore)


###
### Cleaning OLSATVerbalPercentile
results <- results %>% mutate(OLSATVerbalPercentile = ifelse(grepl('~',OLSATVerbalPercentile) == "TRUE", substring(OLSATVerbalPercentile,2,3),
                                                      ifelse(grepl("[0-9]",OLSATVerbalPercentile) == "TRUE", OLSATVerbalPercentile,0)))

# convert dataframe column to numeric
results$OLSATVerbalPercentile = as.numeric(as.character(results$OLSATVerbalPercentile))

# convert data with decimal points to be consistent
results <- results %>% mutate(OLSATVerbalPercentile = ifelse(OLSATVerbalPercentile<1, round(OLSATVerbalPercentile*100), OLSATVerbalPercentile))

# check if cleaning steps work as expected
summary(results$OLSATVerbalPercentile)


###
### Cleaning NNATNonVerbalRawScore
results <- results %>% mutate(NNATNonVerbalRawScore = ifelse(grepl('/48',NNATNonVerbalRawScore) == "TRUE", substring(NNATNonVerbalRawScore,1,nchar(NNATNonVerbalRawScore)-3),
                                                      ifelse(grepl('Fill out later',NNATNonVerbalRawScore) == "TRUE",0,
                                                      ifelse(grepl("[0-9]",NNATNonVerbalRawScore) == "TRUE", NNATNonVerbalRawScore,
                                                      0))))

# convert dataframe column to numeric, the two NA's are score out of 50, ruled out these 2
results$NNATNonVerbalRawScore = as.numeric(as.character(results$NNATNonVerbalRawScore))

# convert data with decimal points to be consistent with the same making system
results <- results %>% mutate(NNATNonVerbalRawScore = ifelse(NNATNonVerbalRawScore>48|NNATNonVerbalRawScore<1, round(NNATNonVerbalRawScore*0.48), 
                                                      NNATNonVerbalRawScore))                                          

# check if cleaning steps work as expected
summary(results$NNATNonVerbalRawScore)


###
### Cleaning NNATNonVerbalPercentile
results <- results %>% mutate(NNATNonVerbalPercentile = ifelse(grepl("[0-9]",NNATNonVerbalPercentile) == "TRUE", NNATNonVerbalPercentile,0))

# convert dataframe column to numeric
results$NNATNonVerbalPercentile = as.numeric(as.character(results$NNATNonVerbalPercentile))

# convert data with decimal points to be consistent
results <- results %>% mutate(NNATNonVerbalPercentile = ifelse(NNATNonVerbalPercentile<1, round(NNATNonVerbalPercentile*100), NNATNonVerbalPercentile))

# check if cleaning steps work as expected
summary(results$NNATNonVerbalPercentile)


###
### Save cleaned dataset
write_csv(results, file = 'C:/Users/alici/OneDrive/Documents/McDaniel College/Spring 2023/ANA 515/G&T_Results_Cleaned.csv')


###
### Did not clean up nor do anything to these columns: SchoolPreferences, SchoolAssigned and Willyouenrollthere
### There were many missing values for these variables and too many varieties of answers/data


###
### Graphs and charts

results$EnteringGradeLevel <- factor(results$EnteringGradeLevel, levels = c("kindergarten", "first", "second", "third"))
# histogram for TimestampYear and EnteringGradeLevel
hist_year <- results %>% ggplot(aes(x=TimestampYear, color=EnteringGradeLevel, fill=EnteringGradeLevel)) + geom_bar(width=0.8, position="dodge")
hist_year

# histogram for EnteringGradeLevel
hist <- results %>% ggplot(aes(x=EnteringGradeLevel, color=EnteringGradeLevel, fill=EnteringGradeLevel)) + geom_bar(width=0.8, position="dodge")
hist

# scatterplot for OLSATVerbalScore and NNATNonVerbalRawScore
scatter_OLSAT_NNAT <- results %>% ggplot(aes(x=OLSATVerbalScore, y=NNATNonVerbalRawScore,color=EnteringGradeLevel)) + geom_point()
scatter_OLSAT_NNAT
# OLSAT Verbal Score and NNAT Non Verbal Raw Score are not correlated with one another

# scatterplot for OverallScore and OLSATVerbalScore
scatter_Overall_OLSAT <- results %>% ggplot(aes(x=OverallScore, y=OLSATVerbalScore, color=EnteringGradeLevel)) + geom_point()
scatter_Overall_OLSAT
# OLSAT Verbal Score will impact Overall Score

# boxplots for OLSATVerbalPercentile
OLSAT_box <- results %>% ggplot(aes(x=EnteringGradeLevel, y=OLSATVerbalPercentile, fill=EnteringGradeLevel)) + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim=c(75,100)) + geom_jitter()
OLSAT_box
# data points are generally close to 100

# boxplots for NNATNonVerbalPercentile
NNAT_box <- results %>% ggplot(aes(x=EnteringGradeLevel, y=NNATNonVerbalPercentile, fill=EnteringGradeLevel)) + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim=c(85,100)) + geom_jitter()
NNAT_box
# there were only 2 data points for EnteringGradeLevel = third

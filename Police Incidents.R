library(stringr)
library(zoo)
crime <- read.csv("Police_Incident_Reports.csv")
colnames(crime)
length(table(crime$Offense.Description))
latlong<-gsub("[\\(\\)]", "", regmatches(crime$Location, gregexpr("\\(.*?\\)", crime$Location)))


strsplit(latlong[1], split = ',')
latlong_splt <- str_split_fixed(latlong, ",", 2)
latlong_splt <- as.data.frame(latlong_splt)
colnames(latlong_splt) <- c("Lat", "Long")

#lat_long investigation
nrow(latlong_splt)
crime<- cbind(crime, latlong_splt)

#Removing unncessary columns
colnames(crime)
#Police.Case.Number
#Date.Reported
#Date.Found 
#Offense.Code
#Subdivision
#Zone.ID
#Case.Status 
#Location
small_crime <- crime
small_crime$Police.Case.Number <- NULL
small_crime$Date.Reported <- NULL
small_crime$Date.Found <- NULL
small_crime$Offense.Code <- NULL
small_crime$Subdivision<-NULL
small_crime$Zone.ID <- NULL
small_crime$Case.Status <- NULL
small_crime$Location<- NULL

#Filter out only the crimes that happened in 2016
crime_2016 <- small_crime[grep("2016", small_crime$Date.Occured),]
months <- substr(crime_2016$Date.Occured, 1,2)
months <- as.numeric(months)
months<- month.name[months]

#adding months
crime_2016$Month <- months

#splitting out time 
time <- strsplit(as.character(crime_2016$Date.Occured), " ")
length(time)

var <- NULL
for (i in 1:length(time)){
  var <- c(var, time[[i]][2])
}
length(var)

crime_2016$Time <- var

AmPm <- NULL
for (i in 1:length(time)){
  AmPm <- c(AmPm, time[[i]][3])
}
length(AmPm)

crime_2016$AmPm <- AmPm

typeof(crime_2016$Time)
#as.date(crime_2016$Time)

addtime <- AmPm == "PM"
addtime <- as.numeric(addtime)
addtime <- addtime*12

c(var,AmPm)

crime_2016$AddTime <- addtime
write.csv(crime_2016, "crime_2016.csv")

df2= structure(c("10:43 AM", "10:54 AM", "11:54 AM", "12:07 PM", "12:15 PM", 
                 "12:54 PM", "1:54 PM", "2:54 PM"), .Dim = c(8L, 1L))
strptime(df2, "%I:%M %p" )


typeof(df2)
var2 <- substr(crime_2016$Time, 1,5)
crime_2016$Time <- var2
TimeAmPm <- paste(crime_2016$Time, crime_2016$AmPm)
newtime <- strptime(TimeAmPm, "%I:%M %p")
newtime2 <- substr(newtime,11,16)
crime_2016$NewTime <-newtime2

here<-as.integer(substr(newtime2, 1,3))
period=cut(here, c(-Inf, 6, 12, 17, 20, 25),
           labels=c("night", "morning", "afternoon", "evening", "night"))
crime_2016$Period <- period

length(period)
nrow(crime_2016)
#get string before first comma
crime <- gsub(",.*$", "", crime_2016$Offense.Description)
sort(table(crime))
crime_type <- c("FRAUD", "HIT & RUN", "DESTRUCTION OF PROPERTY", "ASSAULT", "LARCENY")
keep_rows <- crime %in% crime_type

new_crime_2016 <- crime_2016[keep_rows, ]
crime <- gsub(",.*$", "", new_crime_2016$Offense.Description)
new_crime_2016$Offense <- crime

#Assigning months to seasons
yq <- as.yearqtr(as.yearmon(new_crime_2016$Date.Occured, "%m/%d/%Y") + 1/12)
new_crime_2016$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

#Making final dataset
final_crime <- new_crime_2016
final_crime$Date.Occured<-NULL
final_crime$Offense.Description<-NULL
final_crime$Time<-NULL
final_crime$AmPm <- NULL
final_crime$AddTime <- NULL
final_crime$NewTime <- NULL

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

allCaps <- function(s){
  paste(toupper(s))
}

final_crime$Period <- allCaps(final_crime$Period)
final_crime$Season <- allCaps(final_crime$Season)
final_crime$Month <- allCaps(final_crime$Month)

#remove duplicated
final_crime2 <- unique(final_crime)

#Final crimes3
anyDuplicated(final_crime2)
final_crime2$Long[1055:1057]
which(row.names(final_crime2)=="1056")
empty = final_crime2$Long[193]
sum(final_crime2$Long == empty)
emptyLong <- which(final_crime2$Long == empty)

final_crime3 <- final_crime2[-emptyLong, ]

write.csv(final_crime3, "crime_2016.csv", row.names = F)


table(final_crime3$Offense)

assault_index <- which(final_crime3$Offense=="ASSAULT")

 assaults<- final_crime3[assault_index, ]
 
 write.csv(assaults, "assaults.csv", row.names=F)

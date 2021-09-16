#The weekly Google symptom search data for the three years prior to the COVID pandemic were downloaded in API format, and prepared in the same way as the pandemic data.
data2.1 <- read_csv("http://storage.googleapis.com/gcs-public-data---symptom-search/2017/country/weekly/2017_GB_weekly_symptoms_dataset.csv")
data2.2 <- read_csv("http://storage.googleapis.com/gcs-public-data---symptom-search/2018/country/weekly/2018_GB_weekly_symptoms_dataset.csv")
data2.3 <- read_csv("http://storage.googleapis.com/gcs-public-data---symptom-search/2019/country/weekly/2019_GB_weekly_symptoms_dataset.csv")
data2.4 <- rbind(data2.1,data2.2,data2.3)
data2.4 <- data2.4 %>% rename_all(make.names)
data2.4 <- data2.4[!is.na(data2.4$sub_region_1),]
data2.4 <- data2.4[,-c(5)]
data2.4 <- data2.4[order(data2.4$sub_region_1,data2.4$date),]
#As we did not intend to merge this data with the Google mobility or COVID data, we did not create a separate week variable, only a week of the year variable.
data2.4$week_year <- floor_date(data2.4$date,"week")
data2.4$week_year <- strftime(data2.4$week_year,format = "%V")
#The symptom variables were interpolated.
data2.5 <- do.call(rbind, by(data2.4,data2.4$sub_region_1,na_interpolation_LTLA1))
#We then added the suffix "_baselineG" to the symptom column names to specify that this was the data set for before the pandemic (the baseline).
names(data2.5)[8:429] <- c(paste(names(data2.5)[8:429],"_baselineG",sep=""))
#We then aggregated the data to get the mean symptom search for each sub region for each week of the year. This prevented data duplication, among other things.
data2.6 <- aggregate(data2.5[,8:429], by = list(data2.5$week_year,data2.5$sub_region_1), mean, na.rm = TRUE)
#The variable names of the aggregated data set were then restored.
data2.6 <- rename(data2.6, week_year = Group.1)
data2.6 <- rename(data2.6, sub_region_1 = Group.2)
save(data2.6, file = "inputs/symptom_baseline.RData")

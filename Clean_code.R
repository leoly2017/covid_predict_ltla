##Downloading and preparing master data set.
#Here is the list of packages needed to prepare the data set
rm(list=ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(imputeTS)
library(lubridate)
library(readr)
library(usethis)
source("functions.R")
weekStart <- 39
# 1. Data preparation----
message("Data preparation")
# 1.1 Google symptom----
#First we download the Google symptom search data
#The weekly Google symptom search data during the COVID pandemic was downloaded in API format.
data1 <- read_csv("http://storage.googleapis.com/gcs-public-data---symptom-search/2021/country/weekly/2021_GB_weekly_symptoms_dataset.csv")
data1.2 <- read_csv("http://storage.googleapis.com/gcs-public-data---symptom-search/2020/country/weekly/2020_GB_weekly_symptoms_dataset.csv")
# A new data set was prepared for the two years. 
# This involved changing the column names to make it easier to use the $ operator, 
# removing an empty sub region, removing an empty variable (sub region 2), and reordering the data.
data1.3 <- rbind(data1,data1.2)
data1.3 <- data1.3 %>% rename_all(make.names)
data1.3 <- data1.3[!is.na(data1.3$sub_region_1),]
data1.3 <- data1.3[,-c(5)]
data1.3 <- data1.3[order(data1.3$sub_region_1,data1.3$date),]
#The missing values were then interpolated by sub region.
data1.4 <- do.call(rbind, by(data1.3,data1.3$sub_region_1,na_interpolation_LTLA1))
#Although the observations were/are already weekly, we created a week variable to help merge the results with other data sets. We also created a numbered week of the year variable to merge with the baseline data.
data1.4$week_begin <- floor_date(data1.4$date+7,"week")
data1.4$week_year <- data1.4$week_begin
data1.4$week_year <- strftime(data1.4$week_year,format = "%V")
#We then added the suffix "_data" to the symptom column names to specify that this was the data set for symptom searches during the COVID pandemic.
names(data1.4)[8:429] <- c(paste(names(data1.4)[8:429],"_data",sep=""))
load(file = "inputs/symptom_baseline.RData")
#With the Google symptom data and baseline data sets prepared, we could now calculate the change in symptom searches from baseline during the pandemic.
#The Google symptom data and baseline data sets were merged.
data3 <- left_join(data1.4,data2.6, by = c("sub_region_1","week_year"))
#Surplus data sets were removed to make for work space (RStudio has a limited amount of data for creating objects)
rm(data1)
rm(data1.2)
rm(data1.3)
rm(data1.4)
rm(data2.6)
#We then used pivot_longer to align the data and baseline observations for each symptom search, from that calculate the percentage change in the symptom searches from baseline, then restore the data set using pivot_wider.
data3.2 <- data3 %>% pivot_longer(cols = ends_with("_baselineG") | ends_with("_data"), names_to = c("symptom",".value"), names_pattern = "(.*)_(.*)")
data3.2$change <- ((data3.2$data/data3.2$baselineG)-1)*100
data3.3 <- data3.2 %>% pivot_wider(names_from = symptom,names_sep = "_",values_from = c(baselineG,data,change))
#Having now got the change in symptom_search variable, we removed the data and baseline variables, and removed other variables not relevant to the master data set.
data3.3 <- data3.3[,-c(10:853)]
data3.3 <- data3.3[,-c(1:2,4:7)]
# 1.2 Google mobility----
#Second, we download the Google mobility data.
#The daily Google mobility data was downloaded as a zip file, and the data sets relevant to the UK and the pandemic were extracted from said file
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip", 
              destfile = "inputs/Region_Mobility_Report_CSVs.zip")
unzip("inputs/Region_Mobility_Report_CSVs.zip", 
      files = c("2021_GB_Region_Mobility_Report.csv","2020_GB_Region_Mobility_Report.csv"),
      exdir = "inputs/Google_Mobilty_Reports")
data4.2 <- read.csv("inputs/Google_Mobilty_Reports/2021_GB_Region_Mobility_Report.csv")
data4.3 <- read.csv("inputs/Google_Mobilty_Reports/2020_GB_Region_Mobility_Report.csv")
#With the data sets for the pandemic, we prepared a new data set, reordering, removing an empty sub region and creating a week variable.
data4.4 <- rbind(data4.2,data4.3)
data4.4 <- data4.4[order(data4.4$sub_region_1,data4.4$sub_region_2,data4.4$date),]
data4.4 <- data4.4[data4.4$sub_region_1 != "",]
data4.4$date <- as.Date(data4.4$date)
data4.4$week_begin <- floor_date(data4.4$date, "week")
#The Google mobility data was then interpolated.
data4.5 <- do.call(rbind, by(data4.4,list(data4.4$sub_region_1,data4.4$sub_region_2),na_interpolation_LTLA2))
#We then aggregated the daily data into weekly data, by sub region. After aggregation, the variable names were restored.
data4.61 <- aggregate(data4.5[,10:15], by = list(data4.5$week_begin,data4.5$sub_region_1,data4.5$sub_region_2), mean, na.rm = TRUE)
data4.61 <- rename(data4.61, week_begin = Group.1)
data4.61 <- rename(data4.61, sub_region_1 = Group.2)
data4.61 <- rename(data4.61, sub_region_2 = Group.3)
#The Google mobility data set had different observations for Hackney and the City of London,
# whilst for the COVID data, Hackney and the City of London come under one LTLA. 
# We therefore decided to merge the two sets of observations into a new "Hackney and City of London" sub region.
Hackney_and_COL2 <- data4.61[data4.61$sub_region_2 == "City of London" | data4.61$sub_region_2 == "London Borough of Hackney",]
Hackney_and_COL3 <- aggregate(Hackney_and_COL2[,4:9], by = list(Hackney_and_COL2$week_begin),mean,na.rm = TRUE)
Hackney_and_COL3 <- rename(Hackney_and_COL3, week_begin = Group.1)
Hackney_and_COL3$sub_region_1 <- c("Greater London")
Hackney_and_COL3$sub_region_2 <- c("Hackney and City of London")
data4.71 <- data4.61[data4.61$sub_region_2 != "City of London" & data4.61$sub_region_2 != "London Borough of Hackney",]
data4.71 <- rbind(data4.71,Hackney_and_COL3)
#The Google mobility data set was then merged with the Google symptom search data set.
data5 <- left_join(data4.71,data3.3)
# 1.3 COVID-19 data----
#Third, we download the COVID data
#A single Government coronavirus data set of daily data was downloaded in API format, and prepared mildy (reordering and adding a week variable)
data6 <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&metric=newCasesByPublishDate&metric=newCasesBySpecimenDate&format=csv")
data6 <- data6[order(data6$areaName,data6$date),]
data6$week_begin <- floor_date(data6$date,"week")
#Before interpolating the data, all vaccination observations were set to zero for dates before the vaccine roll-outs (7th of December and 28th of December 2020 for first and second dose respectively).
data6[data6$date <= "2020-12-7", "cumVaccinationFirstDoseUptakeByVaccinationDatePercentage"] <- 0
data6[data6$date <= "2020-12-28", "cumVaccinationSecondDoseUptakeByVaccinationDatePercentage"] <- 0

#This is to correct for no vaccination data in Northern Ireland and Wales
data6.0 <- readxl::read_xlsx("inputs/LTLA-ed.xlsx")
names(data6.0)[3] <- c("areaName")
data6.0 <- left_join(data6.0, data6)
data6.1 <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&format=csv")
data6.0$CD_1 <- substr(data6.0$areaCode,1,1)
data6.1$CD_1 <- substr(data6.1$areaCode,1,1)
names(data6.1)[5:6] <- c("vac1_national", "vac2_national")
data6.11 <- left_join(data6.0, data6.1[4:7])
data6.11$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage[
  is.na(data6.11$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage)
] <- data6.11$vac1_national[
  is.na(data6.11$cumVaccinationFirstDoseUptakeByVaccinationDatePercentage)
]
data6.11$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage[
  is.na(data6.11$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage)
  ] <- data6.11$vac2_national[
    is.na(data6.11$cumVaccinationSecondDoseUptakeByVaccinationDatePercentage)
    ]
data6.12 <- data6.11[names(data6)]
#The data was then interpolated.
data6.2 <- do.call(rbind, by(data6.12,data6$areaName,na_interpolation_LTLA3))
rm(data6.0, data6.1, data6.11)
#The variables were then aggregated by LTLA and week. For the vaccination variables, we chose the maximum percentage for each week, for the COVID case outcomes, we chose the sum of cases for each week. The new data sets were then joined and the column names restored.
data6.3 <- aggregate(data6.2[,7:8], by = list(data6.2$areaName,data6.2$week_begin),sum, na.rm = TRUE)
data6.4 <- aggregate(data6.2[,5:6], by = list(data6.2$areaName,data6.2$week_begin),max)
data6.5 <- left_join(data6.3,data6.4)
#The areaName variable was renamed Map, in order for it to merge with the LTLA name variable in the linkage file (also named Map).
data6.5 <- rename(data6.5, Map = Group.1)
data6.5 <- rename(data6.5, week_begin = Group.2)
#Some excess data sets to be removed
rm(data3)
rm(data3.2)
rm(data3.3)
rm(data4.2)
rm(data4.3)
rm(data4.4)
rm(data4.5)
rm(data4.61)
rm(data4.71)
rm(data6)
rm(data6.2)
rm(data6.3)
rm(data6.4)
#In order for the log_change_LTLA function to work, all zero values for the aggregated COVID variables were changed to 0.5
data6.5[data6.5$newCasesByPublishDate == 0,"newCasesByPublishDate"] <- 0.5
data6.5[data6.5$newCasesBySpecimenDate == 0,"newCasesBySpecimenDate"] <- 0.5
#This allowed for the outcomes to be generated.
data6.6 <- do.call(rbind, by(data6.5,data6.5$Map,log_change_LTLA))
# 1.4 master dataset----
#Now to prepare the master data set
#The COVID and Google data sets were merged together using a linkage file, which I have edited.
data7 <- readxl::read_xlsx("inputs/LTLA-ed.xlsx")
#In order to merge the Google data set to the linkage file, a Google variable was created combining the two sub region variables.
data5$Google <- paste(data5$sub_region_1,"_",data5$sub_region_2,sep = "")
#The two data sets were then merged.
data8 <- left_join(data7,data6.6)
data8 <- left_join(data8,data5)
# 2. Data cleaning----
message("Data cleaning")
#Now to clean the master data set
#In the protocol, we agreed only to use dates after the 1st of June 2020. To do this we created a week index variable, that numbered the difference in weeks for each observation from that date. With this information we then removed observations from before that date. Note when I did the analyses, I estimated the week index individuall for both the COVID and Google data, removed the earlier weeks in each data set, then created a week index variable again after merging. The results should not be too different.
data8.01 <- data8
data8.01$week_index <- data8.01$week_begin - as.Date("2020-06-01")
data8.01$week_index <- as.numeric(data8.01$week_index)
data8.01$week_index <- data8.01$week_index/7
data8.01$week_index <- round(data8.01$week_index)
data8.01 <- data8.01[data8.01$week_index > 0,]
#There were LTLAs with inadequate data for parks and transit stations mobility, and they were removed.
data8.02 <- remove_empty_Map(data8.01, "parks_percent_change_from_baseline")
data8.02 <- remove_empty_Map(data8.02, "transit_stations_percent_change_from_baseline")
#We coerced the master data set to data frame to remove the tibble wrapping.
data8.03 <- as.data.frame(data8.02)
#In order to include the maximum number of observations in the model, we removed Google symptom search variables with more than the smallest observed number of missing values.
all_S <- data8.03[,27:448]
S_NAs <- apply(all_S,2,function(x) sum(is.na(x)))
NA_val <- min(S_NAs)
S_valid <- all_S[,S_NAs == NA_val]
#When restording the master data set, we moved the week index variable from the end of the data set to the middle.
data8.03 <- cbind(data8.03[,c(1:26,449)],S_valid)
#Removing excess data sets
rm(data5)
rm(data6.5)
rm(data6.6)
rm(data7)
rm(data8)
rm(data8.01)
rm(data8.02)
# 2.1 Save data----
save(data8.03, file = paste("outputs/Maindata", Sys.Date(),".RData", sep = ""))
# 2.2 Load data----
# Can load data here for a paricular day----
# Note to Xin Wang: if you want to assess model for a fixed time point then you should save your data8.03 object using
# the code above and load it from below so that you can always reproduce the results for publication purposes.
# An example for loading data up to a particular day is given below
# load(file = "outputs/Maindata2021-08-02.RData")
weekMax <- max(data8.03$week_index)
##Discovering optimal models to predict log change in COVID cases in the master data set.
#3. Data analysis----
# 3.1 Naive model----
message("Naive model")
#This function was used to estimate the naive model MSEs for each outcome by each LTLA (with the mean MSE for all LTLAs being estimated), with the results being copied and pasted into a word document.
MSE_naive_by_LTLA <- rbind(
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_specimen_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_specimen_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_specimen_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_specimen_rate_1")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_specimen_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_specimen_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_specimen_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_specimen_rate_2")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 1, y = "log_specimen_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 2, y = "log_specimen_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 3, y = "log_specimen_rate_3")),
  do.call(rbind,by(data8.03,data8.03$Map,my_naive_model_MSE, h = 4, y = "log_specimen_rate_3"))
)
MSE_naive_by_LTLA %>% group_by(h, y) %>%
  dplyr::summarise(MEAN = mean(SE, na.rm = TRUE))
MSE_naive_by_LTLA[MSE_naive_by_LTLA$week >= weekMax - 3,]%>% group_by(h, y) %>%
  dplyr::summarise(MEAN = mean(SE, na.rm = TRUE))
#3.2 Finding optimal lag combinations----
message("Finding best lag combinations")
#The MSEs for each lag combination were estimated for each outcome individually, and the results were saved as csv files.
message("0/6")
log_rate_1_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03, outcomeG = "log_rate_1", 
                                                             weekStart = weekMax - 4, weekEnd = weekMax - 1)
message("1/6")
log_rate_2_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03, outcomeG = "log_rate_2", 
                                                             weekStart = weekMax - 5, weekEnd = weekMax - 2)
message("2/6")
log_rate_3_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03, outcomeG = "log_rate_3", 
                                                             weekStart = weekMax - 6, weekEnd = weekMax - 3)
message("3/6")
log_specimen_rate_1_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03, outcomeG = "log_specimen_rate_1", 
                                                                      weekStart = weekMax - 4, weekEnd = weekMax - 1)
message("4/6")
log_specimen_rate_2_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03,  outcomeG = "log_specimen_rate_2", 
                                                                      weekStart = weekMax - 5, weekEnd = weekMax - 2)
message("5/6")
log_specimen_rate_3_lag_combos3 <- finding_best_lag_combo_with_my_lag(data8.03, outcomeG = "log_specimen_rate_3", 
                                                                      weekStart = weekMax - 6, weekEnd = weekMax - 3)
message("6/6")
# 3.3 Finding optimal symptom search list----
message("Finding optimal symptom search list")
#The symptom search function was run for each outcome, and the results were copied and pasted into a word document. 
S_log_rate_1 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_rate_1", 
  optimalCombo = log_rate_1_lag_combos3[order(log_rate_1_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 4, weekEnd = weekMax - 1
  )
S_log_rate_2 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_rate_2", 
  optimalCombo = log_rate_2_lag_combos3[order(log_rate_2_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 5, weekEnd = weekMax - 2
)
S_log_rate_3 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_rate_3", 
  optimalCombo = log_rate_3_lag_combos3[order(log_rate_3_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 6, weekEnd = weekMax - 3
)
S_log_specimen_rate_1 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_specimen_rate_1", 
  optimalCombo = log_specimen_rate_1_lag_combos3[order(log_specimen_rate_1_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 4, weekEnd = weekMax - 1
)
S_log_specimen_rate_2 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_specimen_rate_2", 
  optimalCombo = log_specimen_rate_2_lag_combos3[order(log_specimen_rate_2_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 5, weekEnd = weekMax - 2
)
S_log_specimen_rate_3 <- MSE_calculator_to_find_best_symptom_list(
  data8.03, outcome = "log_specimen_rate_3", 
  optimalCombo = log_specimen_rate_3_lag_combos3[order(log_specimen_rate_3_lag_combos3$MSE, decreasing = FALSE)[1],],
  weekStart = weekMax- 6, weekEnd = weekMax - 3
)
# 3.4 Compare different models----
#The results for each outcome were estimated individually, and saved as csv files. 
#The optimal model from this analyses was used for latter sensitivity analyses.
message("Assess and compare all models")
# Note for Xin Wang: the six objects below are the most useful ones, which not only will let you know
# which model is best but also gain insights into the change over time in MSE and MSE by LTLA
Log_rate_1_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_rate_1", 
  optimalCombo = log_rate_1_lag_combos3[order(log_rate_1_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_rate_1,  weekStart = 39, weekEnd = weekMax - 1
  )
Log_rate_2_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_rate_2", 
  optimalCombo = log_rate_2_lag_combos3[order(log_rate_2_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_rate_2,  weekStart = 39, weekEnd = weekMax - 2
)
Log_rate_3_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_rate_3", 
  optimalCombo = log_rate_3_lag_combos3[order(log_rate_3_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_rate_3,  weekStart = 39, weekEnd = weekMax - 3
)
Log_specimen_rate_1_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_specimen_rate_1", 
  optimalCombo = log_specimen_rate_1_lag_combos3[order(log_specimen_rate_1_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_specimen_rate_1,  weekStart = 39, weekEnd = weekMax - 1
)
Log_specimen_rate_2_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_specimen_rate_2", 
  optimalCombo = log_specimen_rate_2_lag_combos3[order(log_specimen_rate_2_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_specimen_rate_2,  weekStart = 39, weekEnd = weekMax - 2
)
Log_specimen_rate_3_data_combos2 <- MSE_calculator_with_alternate_data_sets(
  data_LTLA = data8.03, outcome = "log_specimen_rate_3", 
  optimalCombo = log_specimen_rate_3_lag_combos3[order(log_specimen_rate_3_lag_combos3$MSE, decreasing = FALSE)[1],], 
  my_S1 = S_log_specimen_rate_3,  weekStart = 39, weekEnd = weekMax - 3
)
# Note: if loading from "key_results" then it should start from here
Model_summary <- rbind(
  Log_rate_1_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_rate_1",MSE = mean(MSE, na.rm = TRUE), h = NA),
  Log_rate_2_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_rate_2",MSE = mean(MSE, na.rm = TRUE), h = NA),
  Log_rate_3_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_rate_3",MSE = mean(MSE, na.rm = TRUE), h = NA),
  Log_specimen_rate_1_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_specimen_rate_1",MSE = mean(MSE, na.rm = TRUE), h = NA),
  Log_specimen_rate_2_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_specimen_rate_2",MSE = mean(MSE, na.rm = TRUE), h = NA),
  Log_specimen_rate_3_data_combos2 %>% group_by(model.label, model, i,j,k) %>%
    dplyr::summarise(outcome = "log_specimen_rate_3",MSE = mean(MSE, na.rm = TRUE), h = NA)
)
Model_summary_naive <- MSE_naive_by_LTLA %>% group_by(y, h) %>%
  dplyr::summarise(model.label = "naive", model = "naive", i = NA, j= NA, k =NA, outcome = y[1],
                   MSE = mean(SE, na.rm = TRUE))
Model_summary_naive <- Model_summary_naive[c("model.label", "model", "i", "j", "k", "outcome", "MSE", "h")]
#Model_summary_naive <- do.call(rbind,
#                               by(Model_summary_naive,
#                                  Model_summary_naive$outcome, FUN = function(df) df[order(df$MSE, decreasing = FALSE)[1],]))
Model_summary_combined <- rbind(Model_summary_naive, Model_summary)
Model_summary_combined$model.index <- 1:nrow(Model_summary_combined)
Best_model <-   do.call(rbind,
                        by(Model_summary_combined, 
                           Model_summary_combined$outcome, FUN = function(df) df[order(df$MSE, decreasing = FALSE)[1],]))
# 3.5 For prediction----
message("This is the last step: prediction")
Prediction_all <- do.call(rbind,
                               by(
                                 Model_summary_combined, 
                                 Model_summary_combined$model.index,
                                 Gen_predict_each
                               ))
Prediction_selected <- Prediction_all[Prediction_all$week_reference_y+14 >= as.Date(Sys.Date()) &
                                        Prediction_all$week_reference_y +7<= as.Date(Sys.Date()) &
                                          !is.na(Prediction_all$est),]
Prediction_selected <- Prediction_selected[Prediction_selected$week_reference_y==max(Prediction_selected$week_reference_y),]
Prediction_selected <- do.call(rbind, by(
  Prediction_selected, Prediction_selected[c("outcome", "week_begin_y", "week_reference_y")], 
  FUN = function(df) return(df[df$MSE==min(df$MSE),])
))
Prediction_selected$est.percentage <- (exp(Prediction_selected$est)-1)*100
Prediction_selected$lci.percentage <- (exp(Prediction_selected$est - 1.96*Prediction_selected$se)-1)*100
Prediction_selected$uci.percentage <- (exp(Prediction_selected$est + 1.96*Prediction_selected$se)-1)*100
write.csv(Prediction_selected, paste("forShiny/", "Prediction", Sys.Date(), ".csv", sep = ""), row.names = FALSE)

# 3.6 Save workplace for quick overview of historical results----
save(data8.03,MSE_naive_by_LTLA, 
     log_rate_1_lag_combos3, log_rate_2_lag_combos3, log_rate_3_lag_combos3,
     log_specimen_rate_1_lag_combos3, log_specimen_rate_2_lag_combos3, log_specimen_rate_3_lag_combos3,
     S_log_rate_1, S_log_rate_2, S_log_rate_3, S_log_specimen_rate_1, S_log_specimen_rate_2, S_log_specimen_rate_3,
     Log_rate_1_data_combos2, Log_rate_2_data_combos2, Log_rate_3_data_combos2,
     Log_specimen_rate_1_data_combos2, Log_specimen_rate_2_data_combos2, Log_specimen_rate_3_data_combos2,
     weekMax,
     file = paste("key_results/", Sys.Date(), ".RData", sep = ""))
message("All done!!")

#preparation for R shiny
#===============================================================================

library(ggplot2)
library(plotly)


load(file = "cleaned/master_2021-02-19.RData")

filename <- paste0("forShiny/", "Prediction", Sys.Date(), ".csv", sep = "")

Prediction_selected<- read.csv(file = paste(filename))

#Prediction_selected<- read.csv(file = paste("forShiny/Prediction2021-09-15.csv"))

todays_date <- as.Date(Sys.Date())
week1<-paste(floor_date(todays_date, 'week'))
week2<-paste(floor_date(todays_date, 'week')+7)
week3<-paste(floor_date(todays_date, 'week')+14)

week <- c(week1, week2, week3)

#save(list = c("todays_date", "week1", "week2", "week3", "week"), file = "shinydata.Rdata")

log_rate_name <- as.character("lograte")
log_specimen_rate_name <-as.character("logspecimenrate")

names(Prediction_selected)[12] <- "Change_in_cases"

prediction_1<- Prediction_selected[Prediction_selected$outcome=="log_rate",]
prediction_2<- Prediction_selected[Prediction_selected$outcome=="log_specimen_rate",]



map.data$id <- as.numeric(map.data$id)


for(i in 1:3){
  prediction_1_join<-prediction_1[prediction_1$week_begin_y==week[i],]
  prediction_2_join<-prediction_2[prediction_2$week_begin_y==week[i],]
  
  main.res_map_1 <- left_join(map.data,prediction_1_join)
  relavent_cols_1<- prediction_1_join[c(6,12)]
  
  main.res_map_2 <- left_join(map.data,prediction_2_join)
  relavent_cols_2<- prediction_2_join[c(6,12)]

  #create the log_rate map of week i
  ggsave(
  
    a<-ggplot(data = main.res_map_1, 
              aes(x = long, y = lat, group = group, fill = Change_in_cases,
                text = paste("LTLA:",Map))) +
    geom_polygon(colour = "grey50", size = 0.05)+
    scale_fill_gradient2(name = "% Change\nfrom baseline", midpoint=0,
                         high = "red", low = "blue", na.value = "grey")+
    ggthemes::theme_map()+
    theme(text = element_text(size = 10)),
  
  filename = paste("www/images/",log_rate_name, i, ".png", sep = ""),  width = 5, height = 8
    
  )
  
  ggsave(
    
    b<-ggplot(data = main.res_map_2, 
              aes(x = long, y = lat, group = group, fill = Change_in_cases,
                  text = paste("LTLA:",Map))) +
      geom_polygon(colour = "grey50", size = 0.05)+
      scale_fill_gradient2(name = "% Change\nfrom baseline", midpoint=0,
                           high = "red", low = "blue", na.value = "grey")+
      ggthemes::theme_map()+
      theme(text = element_text(size = 10)),
    
    filename = paste("www/images/", log_specimen_rate_name, i, ".png", sep = ""),  width = 5, height = 8
    
  )
  
}
#=====================================
todays_date <- as.Date(Sys.Date())
week1<-paste(floor_date(todays_date, 'week'))
week2<-paste(floor_date(todays_date, 'week')+7)
week3<-paste(floor_date(todays_date, 'week')+14)

referenceweek<- paste(floor_date(todays_date, 'week')-7)

save(list = c("todays_date", "week1", "week2", "week3", "referenceweek", "filename"), file = "dates.Rdata")
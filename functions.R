#This is the custom function to interpolate the Google symptom search data (as developed by Leo)
na_interpolation_LTLA1 <- function(data_LTLA) {
  res_data <- data_LTLA
  res_data[,8:429] <- na_interpolation(data_LTLA[,8:429])
  return(res_data)
}
#We also used a different interpolation function, due to different column positions
na_interpolation_LTLA2 <- function(data_LTLA) {
  res_data <- data_LTLA
  res_data[,10:15] <- na_interpolation(data_LTLA[,10:15])
  return(res_data)
}
#This required a new interpolation function.
na_interpolation_LTLA3 <- function(data_LTLA) {
  res_data <- data_LTLA
  res_data[,5:8] <- na_interpolation(data_LTLA[,5:8])
  return(res_data)
}
#This custom function, developed by Leo, was used to generate the outcomes measured in this project. It generates the log change in COVID cases a set time period after the observation, allowing the observation to predict future COVID cases.
log_change_LTLA <- function(data_LTLA) {
  len <- nrow(data_LTLA)
  data_LTLA$log_rate_1 <- c(log(data_LTLA$newCasesByPublishDate[2:len]/data_LTLA$newCasesByPublishDate[1:(len-1)]),NA)
  data_LTLA$log_rate_2 <- c(log(data_LTLA$newCasesByPublishDate[3:len]/data_LTLA$newCasesByPublishDate[1:(len-2)]),NA,NA)
  data_LTLA$log_rate_3 <- c(log(data_LTLA$newCasesByPublishDate[4:len]/data_LTLA$newCasesByPublishDate[1:(len-3)]),NA,NA,NA)
  data_LTLA$log_specimen_rate_1 <- c(log(data_LTLA$newCasesBySpecimenDate[2:len]/data_LTLA$newCasesBySpecimenDate[1:(len-1)]),NA)
  data_LTLA$log_specimen_rate_2 <- c(log(data_LTLA$newCasesBySpecimenDate[3:len]/data_LTLA$newCasesBySpecimenDate[1:(len-2)]),NA,NA)
  data_LTLA$log_specimen_rate_3 <- c(log(data_LTLA$newCasesBySpecimenDate[4:len]/data_LTLA$newCasesBySpecimenDate[1:(len-3)]),NA,NA,NA)
  return(data_LTLA)
}
#As part of the master data set cleaning, we removed LTLAs the lacked sufficient observations for a given variable
# (as they would cause the predict function to fail). To do this we used the following custom function.
remove_empty_Map <- function(data_set, variable){
  empty_Map <- by(data_set, data_set$Map, function(x) sum(is.na(x[,variable])))
  empty_val <- max(empty_Map)-1
  empty_Map2 <- empty_Map >= empty_val
  full_Map <- empty_Map2[empty_Map2 == FALSE]
  full_Map2 <- names(full_Map)
  full_Map3 <- paste(full_Map2,collapse = "|")
  data_set <- data_set[grep(full_Map3,data_set$Map),]
  data_set
}
#Estimating naive models
#To check that the optimal models provided additional predictive power, 
#we estimated naive models, where the predicted value is the same as the value for the previous observation. 
#We did this with the following custom function.
my_naive_model_MSE <- function(data_LTLA, h = 1, y= "log_rate_1", weekStart = 39){ # Some edits made by Leo for better efficiency
  res <- data.frame(week = weekStart:(nrow(data_LTLA)),
                    Map = data_LTLA$Map[1],
                    true_value = NA,
                    pred = NA,
                    h = h,
                    y = y
                    )
  for(i in weekStart:(nrow(data_LTLA))){
    res$true_value[i-weekStart+1] <- as.numeric(data_LTLA[i,y])
    res$pred[i-weekStart+1] <- as.numeric(data_LTLA[i-h,y])
  }
  res$SE <- (res$pred-res$true_value)^2
  return(res)
}
#We wanted to determine whether predictor observations from earlier time points could improve the predictive power. Normally, this would involve coercing the predictor variables to a time series format, however there was no effective way to apply a separate time series to each LTLA individually. Thus, we lagged the master data set manually before training and testing the model on the data set, using this custom function.
my_lag <- function(data_LTLA, V_lag = 0, M_lag = 0, S_lag = 0){
  len <- nrow(data_LTLA)
  data_LTLA[,10:11] <- data_LTLA[c(rep(NA,V_lag),1:(len-V_lag)),10:11]
  data_LTLA[,20:25] <- data_LTLA[c(rep(NA,M_lag),1:(len-M_lag)),20:25]
  data_LTLA[,28:ncol(data_LTLA)] <- data_LTLA[c(rep(NA,S_lag),1:(len-S_lag)),28:ncol(data_LTLA)]
  return(data_LTLA)
}
#To train and test each model generated, we used the following custom function. For every week from the 39th week from the 1st of June 2020 up to the most recent week in the master data set, it would continually retrain the model on the weeks prior to the target week, then predict a value for the target week, from which an MSE could be estimated. After estimating the MSE for each week in the data set, it would then return the mean MSE across the weeks tested, and that would be taken as the MSE for the model.
MSE_calculator_with_lm_and_outcome <- function(data_LTLA, outcome = "log_rate_1", weekStart = 39, weekEnd = 61){
  V1 <- paste("cumVaccinationFirstDoseUptakeByVaccinationDatePercentage","+","cumVaccinationSecondDoseUptakeByVaccinationDatePercentage",sep = "")
  M1 <- paste("retail_and_recreation_percent_change_from_baseline","+","grocery_and_pharmacy_percent_change_from_baseline","+","parks_percent_change_from_baseline","+","transit_stations_percent_change_from_baseline","+","residential_percent_change_from_baseline","+","workplaces_percent_change_from_baseline",sep = "")
  S1 <- paste("change_symptom.Cough","+",
              "change_symptom.Fever","+",
              "change_symptom.Fatigue","+",
              "change_symptom.Diarrhea","+",
              "change_symptom.Vomiting","+",
#              "change_symptom.Muscle.weakness","+",
#              "change_symptom.Sputum","+",
              "change_symptom.Shortness.of.breath","+",
              "change_symptom.Confusion","+",
              "change_symptom.Chest.pain",
              sep ="")
  formula <- paste(outcome,"~Map+",V1,"+",M1,"+",S1,sep = "")
  formula1 <- as.formula(formula)
  res <- data.frame(week = weekStart:weekEnd)
  eachFunction <- function(input){
    i <- input$week[1]
    training_set <- data_LTLA[data_LTLA$week_index <= i-1,]
    testing_set <- data_LTLA[data_LTLA$week_index == i,]
    model <- lm(formula1,training_set)
    pred <- predict(model,testing_set)
    true_value <- data_LTLA[data_LTLA$week_index == i, outcome]
    input$MSE <- mean((pred-true_value)^2,na.rm = TRUE)
    return(input)
  }
  res <- do.call(rbind, by(res, res$week, eachFunction))
  The_MSE <- mean(res$MSE,na.rm = TRUE)
  return(The_MSE)
}
#With these two custom functions, we used both of them in another custom function, 
#that would apply the different combinations of lags to the data set, train and test the model on each of the different lagged data sets,
# then return a data set with the MSE for each lag combination. This would take somewhere between 10 and 35 minutes per outcome
finding_best_lag_combo_with_my_lag <-function(data_LTLA, outcomeG = "log_rate_1", weekStart = 39, weekEnd = 61){
  res <- expand.grid(i = 0:3, j = 0:3, k = 0:3)
  res$group <- 1:64
  res$MSE <- NA
  eachF <- function(input){
    prep_data <- do.call(rbind, by(data_LTLA,data_LTLA$Map,my_lag,V_lag = input$i[1],M_lag = input$j[1],S_lag = input$k[1]))
    MSE <- MSE_calculator_with_lm_and_outcome(prep_data,outcome = outcomeG, weekStart = weekStart, weekEnd = weekEnd)
    input$MSE <- MSE
    return(input)
  }
  res <- do.call(rbind, by(res, res$group, eachF)) # Some edits have been made by Leo to improve efficiency
  return(res)
}
#To identify the optimal list of Google symptom search variables, we used the following custom function. 
#First it would generate a list of symptom searches not already included in the baseline model.
# Then, in a for-loop, it would individually add a symptom search variable to the model, 
# and then train and test the new model with an MSE calculator loop. 
#Then, with an if statement, if the new model had a lower MSE than the previous model (starting with the baseline model), 
#the baseline model would be reset to include the new symptom search variable and MSE. 
#This function required that you add the optimal lag combinations, 
#and the MSE for the optimal lag combinations with the baseline model. 
#Note that this function took an extremely long time to run, about six to eight hours per outcome, 
#thus it probably needs parrallel processing to be used effectively.
# Note from Leo: this function has been substantially edited and the amount of time needed per outcome has been reduced
# to ~2hrs per outcome
MSE_calculator_to_find_best_symptom_list <- function(data_LTLA, outcome = "log_rate_1", optimalCombo, weekStart = 39, weekEnd = 60){
  best_MSE <- optimalCombo$MSE[1]
  prep_data <- do.call(rbind, by(data_LTLA,data_LTLA$Map,my_lag,V_lag = optimalCombo$i[1], M_lag = optimalCombo$j[1],S_lag = optimalCombo$k[1]))
  names1 <- colnames(data_LTLA)[28:ncol(data_LTLA)]
  names2 <- c("change_symptom.Cough","change_symptom.Fever","change_symptom.Fatigue","change_symptom.Diarrhea","change_symptom.Vomiting",
#              "change_symptom.Muscle.weakness","change_symptom.Sputum",
              "change_symptom.Shortness.of.breath","change_symptom.Confusion","change_symptom.Chest.pain")
  names <- names1[-match(names2,names1)]
  V1 <- paste("cumVaccinationFirstDoseUptakeByVaccinationDatePercentage","+","cumVaccinationSecondDoseUptakeByVaccinationDatePercentage",sep = "")
  M1 <- paste("retail_and_recreation_percent_change_from_baseline","+","grocery_and_pharmacy_percent_change_from_baseline","+","parks_percent_change_from_baseline","+","transit_stations_percent_change_from_baseline","+","residential_percent_change_from_baseline","+","workplaces_percent_change_from_baseline",sep = "")
  S1 <- paste(names2, collapse = "+")
  addSymptomFunction <- function(SN) {
    res <- data.frame(week = weekStart:weekEnd)
    formulat <- paste(outcome,"~Map+",V1,"+",M1,"+",S1,"+",SN$S2[1],sep = "")
    eachFunction <- function(input){
      i <- input$week[1]
      training_set <- prep_data[prep_data$week_index <= i-1,]
      testing_set <- prep_data[prep_data$week_index == i,]
      model <- lm(formulat,training_set)
      pred <- predict(model,testing_set)
      true_value <- prep_data[prep_data$week_index == i, outcome]
      input$MSE <- mean((pred-true_value)^2,na.rm = TRUE)
      return(input)
    }
    res <- do.call(rbind, by(res, res$week, eachFunction))
    SN$MSE <- mean(res$MSE,na.rm = TRUE)
    return(SN)
  }
  for(n in 1:100) {
    
    SN <- data.frame(S2 = names)
    message(paste("Symptom", outcome, "Step",n, ":", length(SN$S2), " symptoms"))
    SN_res <- do.call(rbind,by(SN, SN$S2, addSymptomFunction))
    SN_res_optimal <- SN_res[order(SN_res$MSE, decreasing = FALSE)[1],]
    names <- names[names %in% SN_res$S2[SN_res$MSE < best_MSE]]
    if(SN_res_optimal$MSE < best_MSE){
      S1 <- paste(S1, "+", SN_res_optimal$S2, sep = "")
      names <- names[names!=SN_res_optimal$S2]
      best_MSE <- SN_res_optimal$MSE
      if(length(names)==0) {
        return(S1);break()
      }
    }else{
      return(S1);break()
    }
  }
}
#Testing for optimal predictor combinations
#This function individually generated models lacking one or two of the predictor groups, 
#and then trained and tested each model (using a for-loop) in an MSE calculator. 
#This function required the optimal lag combinations and the optimal symptom list for the outcome. 
#This was relatively quick, at around maybe 10 minutes per outcome.
# This was further revised by Leo to improve efficiency.
MSE_calculator_with_alternate_data_sets <- function(data_LTLA, outcome = "log_rate_1", 
                                                    optimalCombo, 
                                                    my_S1, weekStart = 39, weekEnd = 61){
  prep_data <- do.call(rbind, by(
    data_LTLA,data_LTLA$Map,my_lag,V_lag = optimalCombo$i[1], M_lag = optimalCombo$j[1],S_lag = optimalCombo$k[1])
    )
  S1 <- my_S1
  V1 <- paste("cumVaccinationFirstDoseUptakeByVaccinationDatePercentage","+","cumVaccinationSecondDoseUptakeByVaccinationDatePercentage",sep = "")
  M1 <- paste("retail_and_recreation_percent_change_from_baseline","+","grocery_and_pharmacy_percent_change_from_baseline","+","parks_percent_change_from_baseline","+","transit_stations_percent_change_from_baseline","+","residential_percent_change_from_baseline","+","workplaces_percent_change_from_baseline",sep = "")
  formula1 <- paste(outcome,"~Map+",V1,"+",M1,"+",S1,sep = "")
  formula2 <- paste(outcome,"~Map+",M1,"+",S1,sep = "")
  formula3 <- paste(outcome,"~Map+",V1,"+",S1,sep = "")
  formula4 <- paste(outcome,"~Map+",V1,"+",M1,sep = "")
  formula5 <- paste(outcome,"~Map+",V1,sep = "")
  formula6 <- paste(outcome,"~Map+",M1,sep = "")
  formula7 <- paste(outcome,"~Map+",S1,sep = "")
  formula_list <- c(formula1,formula2,formula3,formula4,formula5,formula6,formula7)
  res <- expand.grid(model = formula_list,
                    week = weekStart:weekEnd)
  res$model <- as.character(res$model)
  res2 <- expand.grid(model.label = c("VMS", "MS", "VS", "VM", "V", "M", "S"),
                      week2 = weekStart:weekEnd)
  res <- cbind(res, res2)
  eachComplete <- function(eachRow) {
    formulaP <- as.formula(eachRow$model[1])
    training_set <- prep_data[prep_data$week_index <= eachRow$week[1]-1,]
    testing_set <- prep_data[prep_data$week_index == eachRow$week[1],]
    res_set <- testing_set[c("Map", outcome)]
    model <- lm(formulaP,training_set)
    pred <- predict(model,testing_set)
    true_value <- res_set[[outcome]]
    res_set$MSE <- (pred-true_value)^2
    res_set$model.label <- eachRow$model.label[1]
    res_set$week <- eachRow$week[1]
    res_set$model <- eachRow$model[1]
    res_set$i <- optimalCombo$i[1]
    res_set$j <- optimalCombo$j[1]
    res_set$k <- optimalCombo$k[1]
    return(res_set)
  }
  res <- do.call(rbind, by(res, res[c("model", "week")], eachComplete))
  return(res)
}
# This function is written by Leo, which is to generate predicted log ratio and ratio
Gen_predict_each <- function(model.row, data_LTLA = data8.03) {
  message(paste("Predicting...", model.row$model.index[1], sep= ""))
  prospective <- as.numeric(substr(model.row$outcome[1],nchar(model.row$outcome[1]), nchar(model.row$outcome[1])))
  if(model.row$model=="naive"){
    prep_data <- data_LTLA
    week_begin <- max(prep_data$week_begin)
    week_index <- max(prep_data$week_index)
    predict1 <- data.frame(
      index = 1,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index,
      week_begin_y = week_begin + 7*prospective,
      week_reference_y = week_begin,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = prep_data[prep_data$week_index == week_index-model.row$h[1], c(model.row$outcome[1])],
      se = NA,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
    predict2 <- data.frame(
      index = 2,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index-1,
      week_begin_y = week_begin + 7*(prospective-1),
      week_reference_y = week_begin -7,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = prep_data[prep_data$week_index == week_index-model.row$h[1]-1, c(model.row$outcome[1])],
      se = NA,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
    predict3 <- data.frame(
      index = 3,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index-2,
      week_begin_y = week_begin + 7*(prospective-2),
      week_reference_y = week_begin -14,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = prep_data[prep_data$week_index == week_index-model.row$h[1]-2, c(model.row$outcome[1])],
      se = NA,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
  }else{
    prep_data <- do.call(rbind, 
                         by(data_LTLA,data_LTLA$Map,my_lag,
                            V_lag = model.row$i[1], M_lag = model.row$j[1],S_lag = model.row$k[1]))
    week_begin <- max(prep_data$week_begin)
    week_index <- max(prep_data$week_index)
    model <- lm(as.formula(model.row$model),prep_data)
    predict1 <- data.frame(
      index = 1,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index,
      week_begin_y = week_begin + 7*prospective,
      week_reference_y = week_begin,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = predict(model, prep_data[prep_data$week_index == week_index,], se = TRUE)$fit,
      se = predict(model, prep_data[prep_data$week_index == week_index,], se = TRUE)$se.fit,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
    predict2 <- data.frame(
      index = 2,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index - 1,
      week_begin_y = week_begin + 7*(prospective-1),
      week_reference_y = week_begin -7,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = predict(model, prep_data[prep_data$week_index == week_index - 1,], se = TRUE)$fit,
      se = predict(model, prep_data[prep_data$week_index == week_index - 1,], se = TRUE)$se.fit,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
    predict3 <- data.frame(
      index = 3,
      outcome = substr(model.row$outcome[1], 1, nchar(model.row$outcome[1]) - 2),
      week_index_x = week_index - 2,
      week_begin_y = week_begin + 7*(prospective-2),
      week_reference_y = week_begin -14,
      Map = prep_data[prep_data$week_index == week_index,c("Map")],
      id = prep_data[prep_data$week_index == week_index,c("id")],
      est = predict(model, prep_data[prep_data$week_index == week_index - 2,], se = TRUE)$fit,
      se = predict(model, prep_data[prep_data$week_index == week_index - 2,], se = TRUE)$se.fit,
      model.label = model.row$model.label[1],
      MSE = model.row$MSE[1]
    )
  }

  return(rbind(predict1, predict2, predict3))
}
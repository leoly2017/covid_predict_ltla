library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)

#get the date of the week beginning, then calculates the next 2 week dates
todays_date <- as.Date(Sys.Date())
week1<-as.character(floor_date(todays_date, 'week')-7)
week2<-as.character(floor_date(todays_date, 'week'))
week3<-as.character(floor_date(todays_date, 'week')+7)

#lists the dates as characters
week <- c(week1, week2, week3)

  
prediction <- read.csv(file = "Prediction2021-08-05.csv")

names(prediction)[12] <- "Change_in_cases"


#get data of log_rate and log_specimen_rate
prediction_1<- prediction[prediction$outcome=="log_rate",]
prediction_2<- prediction[prediction$outcome=="log_specimen_rate",]


map.data$id <- as.numeric(map.data$id)
#loops 3 times. Once for each week
for(i in 1:3){
  prediction_1_join<-prediction_1[prediction_1$week_begin_y==week[i],]
  prediction_2_join<-prediction_2[prediction_2$week_begin_y==week[i],]
  
  main.res_map_1 <- left_join(map.data,prediction_1_join)
  relavent_cols_1<- prediction_1_join[c(2,4,6,12,13,14)]
  write_as_csv(relavent_cols_1, paste("logratemid", week[i], ".csv", sep = ""))
  
  main.res_map_2 <- left_join(map.data,prediction_2_join)
  relavent_cols_2<- prediction_2_join[c(2,4,6,12,13,14)]
  write_as_csv(relavent_cols_2, paste("logspecimenratemid", week[i], ".csv", sep = ""))
  #  main.res_map$est <- getSig(main.res_map$est, main.res_map$se)
  #create the log_rate map of week i

    a<-ggplot(data = main.res_map_1, 
           aes(x = long, y = lat, group = group, fill = Change_in_cases,
               text = paste("LTLA:",Map))) +
      geom_polygon(colour = "grey50", size = 0.05)+
      scale_fill_gradient2(name = "% Change from baseline", midpoint=0,
                           high = "red", low = "blue", na.value = "grey")+
      ggthemes::theme_map()+
      theme(text = element_text(size = 10))
    
    ggplotly(a)

  
  #create the log_specimen_rate of week i
  ggsave(
    ggplot(data = main.res_map_2, 
           aes(x = long, y = lat, group = group, fill = est.percentage)) +
      geom_polygon(colour = "grey50", size = 0.05)+
      scale_fill_gradient2(name = "R.ratio", midpoint =0,
                           high = "red", low = "blue", na.value = "grey")+
      ggthemes::theme_map()+
      theme(text = element_text(size = 10)),
    filename = paste("logspecimenratemid", week[i], ".png", sep = ""), width = 5, height = 8
  )

}


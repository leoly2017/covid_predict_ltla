library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

#add comments
load(file = "cleaned/master_2021-02-19.RData")
#load(file = "shinydata.Rdata")

#todays_date <- as.Date(Sys.Date())
#week1<-paste(floor_date(todays_date, 'week'))
#week2<-paste(floor_date(todays_date, 'week')+7)
#week3<-paste(floor_date(todays_date, 'week')+14)

#referenceweek<- paste(floor_date(todays_date, 'week')-7)

load(file = "dates.Rdata")



week <- c(week1, week2, week3)



log_rate_name <- as.character("lograte")
log_specimen_rate_name <-as.character("logspecimenrate")


#filename <-filename <- paste0("forShiny/Prediction2021-09-15.csv")
prediction <- read.csv(file = paste(filename))

week1<-paste(floor_date(todays_date, 'week')+1)
week2<-paste(floor_date(todays_date, 'week')+8)
week3<-paste(floor_date(todays_date, 'week')+15)
referenceweek<- paste(floor_date(todays_date, 'week')-6)

#get data of log_rate and log_specimen_rate

#get data of log_rate and log_specimen_rate
prediction_1<- prediction[prediction$outcome=="log_rate",]
prediction_2<- prediction[prediction$outcome=="log_specimen_rate",]


map.data$id <- as.numeric(map.data$id)
#loops 3 times. Once for each week
for(i in 1:3){
  prediction_1_join_st<-prediction_1[prediction_1$week_begin_y==week[i],]
  prediction_2_join_st<-prediction_2[prediction_2$week_begin_y==week[i],]
  
  relavent_cols_1<- prediction_1_join_st[c(6,12,13,14)]
  write.csv(relavent_cols_1, paste("lograte", i, ".csv", sep = ""))
  
  relavent_cols_2<- prediction_2_join_st[c(6,12,13,14)]
  write.csv(relavent_cols_2, paste("logspecimenrate", i, ".csv", sep = ""))
  
}


table1 <- read.csv(paste0(log_rate_name, "1", '.csv'),header = TRUE, sep = ",")
table2 <- read.csv(paste0(log_rate_name, "2", '.csv'),header = TRUE, sep = ",")
table3 <- read.csv(paste0(log_rate_name, "3", '.csv'),header = TRUE, sep = ",")
table4 <- read.csv(paste0(log_specimen_rate_name, "1", '.csv'),header = TRUE, sep = ",")
table5 <- read.csv(paste0(log_specimen_rate_name, "2", '.csv'),header = TRUE, sep = ",")
table6 <- read.csv(paste0(log_specimen_rate_name, "3", '.csv'),header = TRUE, sep = ",")
#need to generate the csv tables


#================================================================================
ui <- fluidPage(
  fluidRow(box(width =12, title="Predicting COVID-19 epidemics in local authorities of the UK", solidHeader = TRUE, br(),
               tags$p("By combining the use of artificial intelligence with vaccination data, localised mobility and Google search queries, we have built a simple prediction model that can predict how the number of COVID-19 cases will change in the immediate short term. The change is presented as a % change for the current week, compared to the previous week, and also predicts the change for the following two weeks. ", br(), br(), " Our model looks at two forms of COVID-19 case information available: by notification date- when cases were reported to the authorities; and also by specimen date (beta)- when the sample was collected from the patient.",style = "font-size:16px;"),
               fluidRow(HTML('&emsp;'),tags$b("    Last Update:", todays_date), br(), HTML('&emsp;'),tags$b("    Reference Week:", referenceweek),style = "font-size:15px;"),br())),
  tabsetPanel(
    id='tabset',
    tabPanel("By Notification Date",
             #fluidRow(br(), HTML('&emsp;'),tags$b("    Last Update:", todays_date), br(), HTML('&emsp;'),tags$b("    Reference Week:", todays_date),style = "font-size:17px;"),
             fluidRow(
               br(), column(width = 6, wellPanel( 
                 tagList(
                   tags$style(type = 'text/css', '#slider1 .irs-grid-text {font-size: 15px}'),
                   tags$style(type = 'text/css', '#slider1 .control-label {font-size: 20px}'),
                   div(id="slider1", width = 6,
                       sliderInput("log", "Select week beginning to predict",
                                   min=as.Date(week1), max=as.Date(week3),value=as.Date(week1), step = 7 ))),style = "padding: 10px 40px;")
               )),
             
             fluidRow(box(width = 12, br(), br(),
                          tags$p("View the predicted percentage change in the number of COVID-19 cases week by week by moving the slider bar to the week of your choice. The percentage change predicted is a percentage figure comparing to the COVID-19 case levels for week beginning", tags$b(referenceweek), br(), br(), "Use the", tags$b("search function on the right hand side"), "to select a specific local UK authority.",br(), br(),style = "font-size:16px;") 
             )),
             
             fluidRow(
               column(width=5, wellPanel(
                 uiOutput("image"))),
               column(width=7, 
                      DT::dataTableOutput("table")
                      
               )
             ),
	     fluidRow(box(width =12,tags$p("This work was supported by the WellcomeTrust ITPA programme at the University of Edinburgh",style = "font-size:14px;")))
             #uiOutput("interactive_slider")),
             #fluidRow(column(width = 9, wellPanel(
             #shinycssloaders::withSpinner(
             #plotlyOutput("heatmaps1", height = "700px", width = "600px"))))
             
             #)
    ),
    
    tabPanel("By Specimen Date (beta)",
             #fluidRow(box(br(), tags$b("    Reference date:", todays_date) ,collapsible = TRUE, color = "blue", status = "primary", style = "font-size:17px;")),
             fluidRow(
               br(), column(width = 6, wellPanel( 
                 tagList(
                   tags$style(type = 'text/css', '#slider1 .irs-grid-text {font-size: 15px}'),
                   tags$style(type = 'text/css', '#slider1 .control-label {font-size: 20px}'),
                   div(id="slider1", width = 6,
                       sliderInput("log2", "Select week beginning to predict",
                                   min=as.Date(week1), max=as.Date(week3),value=as.Date(week1), step = 7 ))),style = "padding: 10px 40px;")
               )),
             
             fluidRow(box(width = 12, br(), br(),
                          tags$p("View the predicted percentage change in the number of COVID-19 cases week by week by moving the slider bar to the week of your choice. The percentage change predicted is a percentage figure comparing to the COVID-19 case levels for week beginning", tags$b(referenceweek), br(), br(), "Use the", tags$b("search function on the right hand side"), "to select a specific local UK authority.",br(), br(),style = "font-size:16px;")
             )),
             
             fluidRow(
               column(width=5, wellPanel(
                 uiOutput("image1"))),
               column(width=7, 
                      DT::dataTableOutput("table_1")
                      
               )
             ),
             fluidRow(box(width =12,tags$p("This work was supported by the WellcomeTrust ITPA programme at the University of Edinburgh",style = "font-size:14px;")))
             #uiOutput("interactive_slider")),
             #fluidRow(column(width = 9, wellPanel(
             #shinycssloaders::withSpinner(
             #plotlyOutput("heatmaps1", height = "700px", width = "600px"))))
             
             #)
    )
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #output$interactive_slider <-renderUI({
  
  #for loop in i
  #opbase<- prediction_1_join<-prediction_1[prediction_1$week_begin_y==week[1],]
  
  #})  
  
  output$image<-renderUI({
    if(input$log ==paste0(week1)){
      img(src =paste("images/",log_rate_name, "1", ".png", sep = ""), width ="100%")
    } else if (input$log== paste0(week2)){
      img(src =paste("images/",log_rate_name, "2", ".png", sep = ""), width ="100%")
    } else if (input$log == paste0(week3)){
      img(src =paste("images/",log_rate_name, "3", ".png", sep = ""), width = "100%")
    }
  })
  
  output$image1<-renderUI({
    if(input$log2 ==paste0(week1)){
      img(src =paste("images/", log_specimen_rate_name, "1", ".png", sep = ""), width ="100%", alt ="graph")
    } else if (input$log2== paste0(week2)){
      img(src =paste("images/", log_specimen_rate_name, "2", ".png", sep = ""), width = "100%" )
    } else if (input$log2 == paste0(week3)){
      img(src =paste("images/", log_specimen_rate_name, "3", ".png", sep = ""), width ="100%")
    }
  })
  
  output$table <- DT::renderDataTable({
    if(input$log == paste0(week1)){
      DT::datatable(table1, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px", pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    } else if (input$log == paste0(week2)){
      DT::datatable(table2, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px",pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    } else if (input$log == paste0(week3)){
      DT::datatable(table3, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px",pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    }
  })
  
  output$table_1 <- DT::renderDataTable({
    if(input$log2 == paste0(week1)){
      DT::datatable(table4, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px", pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    } else if (input$log2 == paste0(week2)){
      DT::datatable(table5, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px",pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    } else if (input$log2 == paste0(week3)){
      DT::datatable(table6, caption = "Table",
                    colnames= c( 'LTLA' ,"Point Estimate","95% CI lower","95% CI upper"),
                    extensions = c("Scroller", "Buttons"), width = '100%',
                    options =list(dom ='Bfrtip', buttons = c('copy','csv','excel'), 
                                  autoWidth= TRUE,scroller = TRUE, scrollX = TRUE, scrollY="700px",pageLength=50), rownames = FALSE) %>% formatRound(c(3:5), 2)
    }
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

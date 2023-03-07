#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(httpuv)
library(shinyTime)
library(shinyFeedback)
library(caret)
library(lattice)
library(ggplot2)
library(digest)
library(randomForest)
library(C50)
library(plyr)
library(rdrop2)
library(xgboost)
library(shinyLP)
library(stringr)
library(shinyjs)
library(shinydashboardPlus)
library(fresh)

drop_auth(rdstoken = "./SL_CAiRS_token1.rds")

#drop_auth(rdstoken = "./slumt1.rds")

load("./top3Models/Random_forest_t1.rda")
load("./top3Models/C5_t3.rda")
load("./top3Models/XGBoost_t2.rda")

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}


# Define UI for application that draws a histogram
ui<-dashboardPage(skin = "black",# User side ----
                  
  dashboardHeader(title = p("An Interface for Concrete Slump Prediction",
                            style="font-family: 'times'; font-size:32pt"),
                  titleWidth =   "100%"),
  
  dashboardSidebar(useShinyFeedback(),width =  "350px",
                   # p(a(img(src = "hkustlogo.png"),href="https://hkust.edu.hk/home")),
                   tags$head(
                   tags$style(
                   HTML(".shiny-output-error-validation 
                        {font-family:times; color: red; font-size:22pt}"))),
                   sidebarMenu(id = "tabs",collapsed = FALSE,
                               menuItem(p(icon("info-circle"),"Introduction",
                               style="font-size:22pt;font-family:times"),
                               tabName = "info"),
                               # menuItem(p(icon("list-ul"),"Instructions to use",
                               #              style="font-size:22pt;
                               #            font-family:times"),tabName = "inst"),
                               menuItem(p(icon("cogs"),"Run the analysis",
                               style="font-size:22pt;font-family:times"),
                               tabName = "analysis")
                               # menuItem(p(icon("address-book"),"About us",
                               #              style="font-size:22pt;
                               #              font-family:times"),tabName = "contact")
                               )),
  dashboardBody(
    useShinyjs(),
    useShinyFeedback(),
    tabItems(
      tabItem(tabName = "info", ## Info tab----
        wellPanel(### Introduction----
          style = "font-family: 'times';font-size:18pt;background:#dcfce4;",
          span("Introduction:",style="color:red"),br(),
          p("This interactive web-interface applies state of art machine 
          learning (ML) tools in prediction of slump of fresh industrial 
          concrete. The interface follows a simple input-output gateway to
          support a user-friendly implementation of ML tools in concrete
          performance prediction. The users are required to follow a simple
          three-step methodology (1) Providing the inputs, (2) Running the 
          analysis, and (3) Providing the feedback. Inputs includes the 
          details of concrete batching such as mixture proportions, and 
          weather and mixing conditions. There are a total of 16 inputs; 
          nine inputs are quantities of concrete constituents, and other 
          includes batching/mix code, time for batching and testing, 
          duration of mixing and atmospheric conditions. Running the 
          analysis will display the predicted slump class.
          The predictions are displayed based on three ML 
          models including C5.0 decision trees (C5.0), Random forest (RF),
          and Extreme gradient boosting (XGBoost). These three models are 
          selected based on best training and testing performance.
          Finally, users are required to give feedback in terms of actual
          slump measured at construction site. User can go to",
          span(actionButton(inputId = "analysis1",icon = icon("cogs"),
            style="font-size:18pt;color:red;background:#dcfce4",
            label = "Run the analysis")),
        "tab to get prediction of slump of a concrete batch. 
        Before that please note followings:")),
        wellPanel(### Key Notes:----
          style = "font-family: 'times';font-size:18pt;background:#dcfce4",
          span("Key Notes:",style="color:red"),
          p(strong("(1)"), "This framework is developed for research purpose 
          only and is subjected to the data and modeling constraints. 
          Therefore, the users should not completely rely on the prediction 
          results of this framework to assess fresh concrete performance."),
          p(strong("(2)"), "Users are required to give all inputs to run the 
          modeling, otherwise model will not run."),
          p(strong("(3)"), "Inputs related to mixing constituents are subjected
          to lower and upper constraints based on the data used for model 
          learning. For example, in case of water you can only put the values
          between 132 to 1639 (kg), because the concrete batches used for model
          training have water quantities between these limits. To facilitate 
          the user this application is automated to highlight these 
            boundaries."),
          p(strong("(4)"), "The modeling is performed considering two types of 
          super plasticizing admixtures that are KFDN-SP8G and R1002. 
          Please be careful when providing the input, one of these two 
            admixtures should be zero."),
          p(strong("(5)"), "After getting the results, please provide the 
          feedback for actual slump. This feedback is very much important 
            for model improvements.")
          # ,
          # p(strong("(6)"), "To facilitate the user further an example of
          # batching ticket (showing different inputs) and a case of model use
          # (video-illustration) is provided as follow.")
          ),
          br(),
        # wellPanel(### Batching Ticket----
        #   style = "font-family: 'times';font-size:18pt;background:#dcfce4",
        #   fluidRow(align="center",
        #    p(style="font-family: times;font-size:18pt;text-align:center",
        #      "This is an example of batching ticket"),br(),
        #    img(src = "exp_ticket_1.png",width="700pcx",height="300pcx"))),
        # wellPanel(### Example Video----
        #   fluidRow(align="center",
        #   actionButton("run_video", "See Video",
        #   style="font-family: times;color:white;
        #   font-size:20pt;background:#16347a"),htmlOutput("video")))
        ),
      tabItem(tabName = "analysis",## Analysis tab----
        fluidRow(style = "background:#dcfce4",
        p(icon("keyboard"),
        style="font-family:'times';font-size:26pt;face:bold;text-align:center",
        strong(" Please input following details:"))),
        div(id="inputs", ### input division----
            fluidRow(
              fluidRow(h2("Mixing Proportions"),align="center"), 
              column(4,style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",  
                        numericInput("Batch_size",
                                     HTML(paste0("Batch size (e.g.,7.2 m",tags$sup("3"),')')),
                                     min = 1,max = 10,step = 0.1,value = 7.2),
                        numericInput("Water","Water (e.g, 1129kg)" ,
                                     min = 132,max = 1639,step = 1,value = 1129),
                        numericInput("CEM","Cement (e.g, 2225kg)" ,
                                     min = 164,max = 3306,step = 1,value =2225))),
              column(4,style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",      
                        numericInput("PFA","Fly ash (e.g, 718kg)" ,
                                     min = 56,max = 1615,step = 1,value = 718),
                        numericInput("CAG_10mm",
                                     "Coarse aggregate 10mm (e.g., 3290kg)",
                                     min = 410,max = 4465,step = 1,value = 3290),
                        numericInput("CAG_20mm",
                                     "Coarse aggregate 20mm (e.g., 3970kg)",
                                     min = 495,max = 7655,step = 1,value = 3970),
                        numericInput("FAG","Fine aggregate (e.g., 6085kg)",
                                     min = 590,max = 8930,step = 1,value = 6085))),  
              column(4,style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",
                        numericInput("WRA_KFDN100","KFDN-100 (e.g., 26500g)",
                                     min = 860,max = 54500,step = 1,value = 26500),
                        numericInput("SP1_KFDNSP8G","KFDN-SP8G (e.g., 0g)" ,
                                     min = 0,max = 42060,value = 0,step = 1),
                        numericInput("SP2_R1002","R1002 (e.g.,30200g)" ,
                                     min = 0,max = 61440,value = 30200,step = 1)))),
            fluidRow(
              fluidRow(h2("Batching Details"),align="center"),
              column(4,
                     style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",
                        textInput("Batch_mix",
                                  "Batching/Mixing code (e.g., 5B68BK29NN)",
                                  "5B68BK29NN"),
                        numericInput("mix_time","Batch mixing time (Sec)",45))),
              column(4,
                     style = "font-family: 'times';background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",
                        strong("Time of batching")),
                     h5(style="color:black;font-family:times",
                        selectizeInput(inputId = "Time_b1",
                                       choices = as.character(c(0:23)),"hours"),
                        selectizeInput(inputId = "Time_b2",
                                       choices = as.character(c(0:59)),"minutes"))),
              column(4,
                     style = "font-family: 'times';background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",
                        strong("Time of slump testing")),
                     h5(style="color:black;font-family:times",
                        selectizeInput(inputId = "Time_t1",
                                       choices = as.character(c(0:23)),"hours"),
                        selectizeInput(inputId = "Time_t2",
                                       choices = as.character(c(0:59)),"minutes")))),
            fluidRow(
              fluidRow(h2("Weather Conditions"),align="center"), 
              column(4,style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",  
                        numericInput("atm_temp",
                                     "Temperature (degree C)",
                                     min = 15,max = 41,step = 0.1,value = 25))),
              column(4,style = "font-family: 'times';font-size:18pt;background:#dcfce4",
                     h4(style="color:#086A99;font-family:times",      
                        numericInput("RH","Relative humidity (%) " ,
                                     min = 35,max = 100,step = 1,value = 65))))),
      div(id="results", ### Results devesion ----
          wellPanel(style="font-family: times;font-size:18pt;color: 
          black;background:#dce4fc",
            fluidRow(
              column(12,
                     align="center",
                     HTML(
                       as.character(
                         actionButton(
                           inputId = "get_results",
                           icon = icon("cogs")," Run",width = "800px",
                           style="display:inline-block;font-size:22pt;color:white;background:red"))))),
            br(),
            fluidRow(
              column(12,
                     valueBoxOutput(
                       "Model_1",width = "100%"))))), 
      conditionalPanel(condition = "output.Model_1",### Output ----
     div(
       id="div_fb_sub",
       wellPanel(style="font-family: times;background:#d6c93a",
                 fluidRow(align="center",
                          style="font-family: times;color:white;font-size:26pt",
                          strong(icon("spell-check"),"Please provide feedback:")),
                 hr(),
                 fluidRow(
                   column(4,style="font-family: times;color:black;font-size:22pt",
                          numericInput("slump", "Actual Slump (mm)","")),
                   column(8,style="font-family: times;color:black;font-size:22pt",
                          textInput("comment", "Your comment","Do you have any comment.?"))),
                 fluidRow(align="center",
                          HTML(
                            as.character(
                              actionButton(
                                inputId = "fb_sub","Submit the feedback..!",
                                icon=icon("user-edit"),width = '800px',
                                style="display:inline-block;font-size:22pt;
                                color:white;background:red"))))))),
conditionalPanel(condition = "input.fb_sub", ### input feedback---- 
     wellPanel(align="center",style="font-family: times;color:red;
font-size:22pt;background:#dce4fc",htmlOutput("sub_sl_out"))),
conditionalPanel(condition = "output.sub_sl_out && output.Model_1",
     wellPanel(fluidRow(column(12,align="center",
                actionButton("reset",
                            icon=icon("cogs"),"Run another simulation",
                            width = '800px',
                            style="font-family: times;color:white;
                            font-size:22pt;background:red")))))),
tabItem(tabName = "contact",## Contact tab ----
      br(),
      wellPanel(
        fluidRow(
          column(3,
                 img(src = "zeshan_pic.jpg",width="156.5pcx",height="206pcx")),
          column(9,align="top",
                 h2(a(
                   href="https://www.linkedin.com/in/muhammad-zeshan-akber-5998a025/",
                   "Muhammad Zeshan AKBER",style="font-family:times")),
                 h3("PhD Candidate,",style="font-family:times",
                    p("Department of Civil and Environmental Engineering"),
                    p("The Hong Kong University of Science and Technology")),
                 h4("Contact Number: (+852) 5494 3577",style="font-family:times",
                    p("Email:",
                      a(href="mailto:zeshan.akber@alumni.ust.hk",
                        "zeshan.akber@alumni.ust.hk")))))),
      wellPanel(
        fluidRow(
          column(3,
                 img(src = "prof_pic.png",width="156.5pcx",height="206pcx")),
          column(9,align="top",
                 h2(a(href="https://seng.hkust.edu.hk/about/people/faculty/xueqing-zhang",
                      "Xueqing ZHANG",style="font-family:times")),
                 h3("Associate Professor,",style="font-family:times", 
                    p("Department of Civil and Environmental Engineering"),
                    p("The Hong Kong University of Science and Technology")),
                 h4("Contact Number: (+852) 2358 8480",
                    style="font-family:times",
                    p("Email:",
                      a("zhangxq@connect.ust.hk", href="mailto:zhangxq@ust.hk"))))))
                      ))))

# Server side ----
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # outputDir <- "www"; token<-readRDS("SL_CAiRS_token1.rds"); token$refresh()
  # 
  onevent("mouseenter", "sidebarCollapsed", 
          shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", 
          shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  observeEvent(input$analysis1,{
    updateTabItems(session, "tabs", "analysis")
    
  }) 
  
  observeEvent(input$exp_link,{
    updateTabItems(session, "tabs", "exp")
    
  }) 
  
  rv<-reactiveValues()
  rv$rf<-fit_t1_rf
  rv$c5<-fit_t3_C5
  rv$xgb<-fit_t2_xgbTree
  
  outputDir <- "responses"
  
  
  # setting conditions on inputs
  
  Batch_size_con<-reactive({
    
    Batch_size_con <- (input$Batch_size> 0 & input$Batch_size <11 )
    feedbackDanger("Batch_size", !Batch_size_con, "Please input the Batch size between 
                   0 and 11")
    req(Batch_size_con)
    input$Batch_size
  })
  Water_con<-reactive({
    Water_con<- (input$Water > 131 & input$Water < 1640)
    feedbackDanger("Water",!Water_con  , "Please input the water quantity between 131 
                   and 1640")
    req(Water_con)
    input$Water
  })
  Cem_con<-reactive({
    CEM_con<- (input$CEM > 163 & input$CEM < 3307)
    feedbackDanger("CEM",!CEM_con, " Please input the cement quantity between 163 and 
                   3307")
    req(CEM_con)
    input$CEM
  })  
  PFA_con<-reactive({  
    PFA_con<-(input$PFA > 55 & input$PFA < 1617)
    feedbackDanger("PFA", !PFA_con, "Please input the fly ash quantity between 55 and 
                   1617")
    req(PFA_con)
    input$PFA
  })
  CAG_10mm_con<-reactive({
    CAG_10mm_con <- (input$CAG_10mm> 409 & input$CAG_10mm <4466)
    feedbackDanger("CAG_10mm", !CAG_10mm_con, "Please input the coarse aggregate (10mm)
                   quantity between 409 and 4466")
    req(CAG_10mm_con)
    input$CAG_10mm
  })
  CAG_20mm_con <-reactive({
    CAG_20mm_con <- (input$CAG_20mm> 494 & input$CAG_20mm <7656)
    feedbackDanger("CAG_20mm", !CAG_20mm_con, "Please input the coarse aggregate (20mm)
                   quantity between 494 and 7656")
    req(CAG_20mm_con)
    input$CAG_20mm
  })
  FAG_con <- reactive({
    FAG_con <- (input$FAG > 589 & input$FAG < 8931)
    feedbackDanger("FAG", !FAG_con, "Please input the fine aggregate quantity between 
                   589 and 8931")
    req(FAG_con)
    input$FAG
  })
  WRA_KFDN100_con <- reactive({ 
    WRA_KFDN100_con <- (input$WRA_KFDN100> 859 & input$WRA_KFDN100 <54501) 
    feedbackDanger("WRA_KFDN100", !WRA_KFDN100_con, "Please input the KFDN-100 quantity
                   between 859 and 54501")
    req(WRA_KFDN100_con)
    input$WRA_KFDN100
  })
  SP1_KFDNSP8G_con <-reactive({
    SP1_KFDNSP8G_con <- (input$SP1_KFDNSP8G>= 0 & input$SP1_KFDNSP8G <42061) 
    feedbackDanger("SP1_KFDNSP8G", !SP1_KFDNSP8G_con, "Please input the KFDN-SP8G quantity
                   from 0 to 42060")
    req(SP1_KFDNSP8G_con)
    input$SP1_KFDNSP8G
  })
  SP2_R1002_con <- reactive({
    SP2_R1002_con <- input$SP2_R1002>= 0 & input$SP2_R1002 <61441 
    shinyFeedback::feedbackDanger("SP2_R1002", !SP2_R1002_con, "Please input the R1002 
                                  quantity from 0 to 61440")
    req(SP2_R1002_con)
    input$SP2_R1002
  })
  input_data<-eventReactive(input$get_results,{
    
    validate(
      need(input$Batch_mix != "", "Please provides all inputs") %then%
        need(input$Batch_size != "", "Please provides all inputs") %then%
        need(input$Water != "", "Please provides all inputs") %then%
        need(input$CEM != "", "Please provides all inputs")%then%
        need(input$PFA != "", "Please provides all inputs")%then%
        need(input$CAG_10mm != "", "Please provides all inputs")%then%
        need(input$CAG_20mm != "", "Please provides all inputs")%then%
        need(input$FAG != "", "Please provides all inputs")%then%
        need(input$WRA_KFDN100 != "", "Please provides all inputs")%then%
        need(input$SP1_KFDNSP8G != "", "Please provides all inputs")%then%
        need(input$SP2_R1002 != "", "Please provides all inputs")%then%
        need(input$Time_t1 != "", "Please provides all inputs")%then%
        need(input$Time_t2 != "", "Please provides all inputs")%then%
        need(input$Time_b1 != "", "Please provides all inputs")%then%
        need(input$mix_time != "", "Please provides all inputs")%then%
        need(input$atm_temp != "", "Please provides all inputs")%then%
        need(input$RH != "", "Please provides all inputs")
    )
    
    input_data<-data.frame("Batch_size"=Batch_size_con(),
                           "Water"=Water_con(),
                           "Cement"=Cem_con(),  
                           "PFA"=PFA_con() ,
                           "CAG_10mm"=CAG_10mm_con(),
                           "CAG_20mm"=CAG_20mm_con(),
                           "FAG"=FAG_con(),
                           "WRA_KFDN100"=WRA_KFDN100_con(),
                           "SP1_KFDNSP8G"=SP1_KFDNSP8G_con() ,
                           "SP2_R1002" =SP2_R1002_con())
    
    
    
    input_data$pred_sl_rf<-sub(x = paste(predict(rv$rf,input_data)),
                               pattern = "sl_",replacement = "")
    input_data$pred_sl_C5<-sub(x = paste(predict(rv$c5,input_data)),
                               pattern = "sl_",replacement = "")
    input_data$pred_sl_xgb<-sub(x = paste(predict(rv$xgb,input_data)),
                                pattern = "sl_",replacement = "")
    input_data
  }) 
  pred_prob<-reactive({
    prob<-list()
    prob$rf<-max(predict(rv$rf,input_data(),type = "prob"))
    prob$c5<-max(predict(rv$c5,input_data(),type = "prob"))
    prob$xgb<-max(predict(rv$xgb,input_data(),type = "prob"))
    prob
  })
  
  
  
  output$Model_1<-renderValueBox({
    
    valueBox(color = "purple",
             HTML(paste0(
               fluidRow(align="center",
                        span("The predicted slump are:",
                             style="color:white;font-family:times;font-size:26pt")))),
             
             
             HTML(paste0(hr(),
                         fluidRow(column(4,span("Machine learning model",style="font-family:times;font-size;22")),
                                  column(4,span("Slump (mm)",style="font-family:times;font-size;22")),
                                  column(4,span("Probability",style="font-family:times;font-size;22"))),hr(),
                         fluidRow(column(4,span("Random Forest",style="font-family:times;font-size;22")),
                                  column(4,span(input_data()$pred_sl_rf,style="font-family:times;font-size;22")),
                                  column(4,span(round(pred_prob()$rf,digits = 4),style="font-family:times;font-size;22"))),hr(),
                         fluidRow(column(4,span("XGBoost",style="font-family:times;font-size;22")),
                                  column(4,span(input_data()$pred_sl_xgb,style="font-family:times;font-size;22")),
                                  column(4,span(round(pred_prob()$xgb,digits = 4),style="font-family:times;font-size;22"))),hr(),
                         fluidRow(column(4,span("C5Tree",style="font-family:times;font-size;22")),
                                  column(4,span(input_data()$pred_sl_C5,style="font-family:times;font-size;22")),
                                  column(4,span(round(pred_prob()$c5,digits = 4),style="font-family:times;font-size;22")))
             )))})
  
  outputOptions(output, "Model_1", suspendWhenHidden = FALSE)
  
  
  observeEvent(pred_prob(), {
    showModal(modalDialog(
      title = "",
      "Please provide the feedback",
      easyClose = FALSE,footer = modalButton("Close this")
    ))
    
  })
  
  
  df_fin<-eventReactive(input$fb_sub,{
    
    validate(
      need(input$slump != "", "Please provides value of slump"))
    
    df_fin<-input_data()
    slump<-input$slump
    df_fin$actual_slump<-slump
    df_fin$rf_fb<-input$Mf_1
    df_fin$xgb_fb<-input$Mf_2
    df_fin$c5_fb<-input$Mf_3
    df_fin$test_time<-paste(input$Time_t1,":",input$Time_t2)
    df_fin$batch_time<-paste(input$Time_b1,":",input$Time_b2)
    df_fin$comments<-paste(input$comment)
    df_fin$mix_code<-paste(input$Batch_mix)
    df_fin$mix_time<-paste(input$mix_time)
    df_fin$Temp<-paste(input$atm_temp)
    df_fin$RH<-paste(input$RH)
    
    df_fin
    
  }) 
  
  
  saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()),digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
  }
  
  
  sl_fb<-eventReactive(input$fb_sub,{
    
    validate(
      need(input$slump != "", "Please provides value of actual slump") %then%
        need(input$Batch_mix != "", "Please provides all inputs and run model again") %then%
        need(input$Batch_size != "", "Please provides all inputs and run model again") %then%
        need(input$Water != "", "Please provides all inputs and run model again") %then%
        need(input$CEM != "", "Please provides all inputs and run model again")%then%
        need(input$PFA != "", "Please provides all inputs and run model again")%then%
        need(input$CAG_10mm != "", "Please provides all inputs and run model again")%then%
        need(input$CAG_20mm != "", "Please provides all inputs and run model again")%then%
        need(input$FAG != "", "Please provides all inputs and run model again")%then%
        need(input$WRA_KFDN100 != "", "Please provides all inputs and run model again")%then%
        need(input$SP1_KFDNSP8G != "", "Please provides all inputs and run model again")%then%
        need(input$SP2_R1002 != "", "Please provides all inputs and run model again")%then%
        need(input$Time_t1 != "", "Please provides all inputs and run model again")%then%
        need(input$Time_t2 != "", "Please provides all inputs and run model again")%then%
        need(input$Time_b1 != "", "Please provides all inputs and run model again")%then%
        need(input$Time_b2 != "", "Please provides all inputs and run model again"))
    
    
    sl_fb<-input$slump
    saveData(df_fin())
    sl_fb
  })
  
  
  
  output$sub_sl_out<-renderText(paste("Your last submitted slump is:",strong(sl_fb()),"mm"))
  
  outputOptions(output, "sub_sl_out", suspendWhenHidden = FALSE)
  
  observeEvent(sl_fb(),{
    
    
    updateNumericInput(session,inputId = "Water",value = "")
    updateNumericInput(session,inputId = "CEM",value = "")
    updateNumericInput(session,inputId = "PFA",value = "")
    updateTextInput(session,inputId = "Batch_size",value = "")
    updateNumericInput(session,inputId = "CAG_10mm",value = "")
    updateNumericInput(session,inputId = "FAG",value = "")
    updateNumericInput(session,inputId = "CAG_20mm",value = "")
    updateNumericInput(session,inputId = "WRA_KFDN100",value = "")
    updateNumericInput(session,inputId = "SP1_KFDNSP8G",value = "")
    updateNumericInput(session,inputId = "SP2_R1002",value = "")
    updateSelectizeInput(session,inputId = "Time_b1",selected = "")
    updateSelectizeInput(session,inputId = "Time_b2",selected ="")
    updateSelectizeInput(session,inputId = "Time_t1",selected = "")
    updateSelectizeInput(session,inputId = "Time_t2",selected = "")
    updateNumericInput(session,inputId = "slump",value = "")
    updateTextInput(session,"Batch_mix",value = "")
    updateTextInput(session,"comment",value = "")
    updateNumericInput(session,inputId = "mix_time",value = "")
    updateNumericInput(session,inputId = "RH",value = "")
    updateNumericInput(session,inputId = "atm_temp",value = "")

    
    showModal(modalDialog(title = "Thankyou..!",
                          "For submitting your valuable feedback.
                          You can refresh the webpage to run another simulation"
                          , easyClose = TRUE,
                          footer = modalButton("Close this")))
    
  })
  
  # observeEvent(input$run_video, {
  #   
  #   output$video <- renderUI({
  #     HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/9mETVP-1lvE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; 
  #       clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  #   })
  #   
  # })
  # 
  observeEvent(input$reset,{
    session$reload()
  }
  )

  
}
# Run the application 
shinyApp(ui = ui, server = server)





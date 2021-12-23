library(shiny)
library(referenceIntervals)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "style.css",
  titlePanel("refVal Web Tool"),
             tabsetPanel(
               tabPanel("Introduction",icon = icon("align-left"),
                        sidebarLayout(
                          sidebarPanel(width = 5,
                                       fluidRow(
                                         column(4,
                                           img(src = "images.png", width = 260, height = 200),
                                           br(),
                                         ),
                                         column(4,
                                           img(src = "images2.png", width = 260, height = 200),
                                           br()))),
                          mainPanel(
                            h3("What is Reference Intervals?",style = "font-family: 'times'"),
                            hr(),
                            p("-Reference interval (called the normal range), 
                              the value of a range measured in healthy individuals and the range of the reference value that contains a certain percentage of the range between the lower and upper limit of the reference definitions.
                              Reference intervals are a decision support method for interpreting numerical reports. Possible laboratory results and numerical reports can be interpreted by comparing them with reference intervals, 
                              the quality and correct calculation of reference intervals is of  importance for the correct interpretation of the results.
                              To calculate the reference intervals in the literature, and published the work of a range of reference proposed protocol can be carried out according to standard procedures. 
                              Therefore, in order to calculate the reference intervals, 
                              it is necessary to take into account the data acquisition processes and calculation methods in accordance with these standards.",style = "font-family: 'times'; font-size:16pt"),
                            h3("What does this Web-Tool do?",style = "font-family: 'times'"),
                            hr(),
                            p("-This web-tool, allows people to determine reference intervals using different method algorithms such as parametric, 
                              non-parametric and robust on their own data sets and also to determine outliers values in the relevant columns of the data sets.",style = "font-family: 'times'; font-size:16pt"),
                            hr(),
                            helpText(h3("How do we do these calculations?")),
                            helpText("Firstly, you should upload a dataset.after then,you should choose which column to processing in your dataset.
                                     Then you should choose which method to calculate reference ıntervals. After reference intervals calculating , you see plot and table your values.")
                          )
                        )),
               tabPanel("DataSet Input",icon = icon("table"),
                        sidebarLayout(
                          sidebarPanel(
                            h4("Data Upload"),
                            radioButtons("dataInput","Chooise Dataset Input",list("None","Load Example Dataset"=0,"Upload a file"=1,"Paste a Dataset"=2),selected = "None"),
                            conditionalPanel(condition = "input.dataInput=='0'",
                                             h5(tags$b("Datasets:")),
                                             radioButtons("sampleData", "", 
                                                          list("set120 (1:120)" = 1, "set200 (1:200)" = 2, "set20 (1:20)"=3), 
                                                          selected = 1)),
                            conditionalPanel(condition = "input.dataInput=='1'",
                                             h5("Upload a Dataset"),
                                             fileInput("UploadD", "Upload CSV File:", accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                             hr(),
                                             h5("Warner!"),
                                             p("In order not to receive an upload error, upload a dataset consisting of numeric data."),
                                             hr(),
                                             checkboxInput("header", "Header", TRUE),
                                             radioButtons("sep", "Separator", choices = list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),
                                                          selected = 1),
                                             radioButtons("show", "Show the Dataset",
                                                          choices = c(Head="head",All="all"),
                                                          selected = "head")),
                            conditionalPanel(condition ="input.dataInput=='2'",
                                             h5("Paste or enter your data below:"),
                                             tags$textarea(id = "myData", rows = 10, cols = 5, ""),
                                             actionButton('clearText_button','Clear data'),
                                             HTML('<br>'),
                                             HTML('<br>'),
                                             radioButtons("fileSepP", "Separator:", 
                                                          list("Comma" = 1, "Tab" = 2, "Semicolon" = 3), 
                                                          selected = 2),
                                             HTML('<p>You can paste or manually enter your data as separated by comma, tab or semicolon.</p>'),
                                             HTML('<p>Note: First row must be header.</p>'))
                          ),
                          mainPanel(
                            h3(textOutput("caption", container = span)),
                            dataTableOutput("table")
                          )
                        )
               ),
               tabPanel("Calculating Reference Intervals",icon = icon("chart-line"),
                        sidebarLayout(
                          sidebarPanel(selectInput("variable","Variable",choices = c("Unspecified"),selected = "Unspecified"),
                                       #selectInput("variable","Variable",choices =(names(datasetfile))),
                                       h5("Warner!!"),
                                       p("You can select only the numeric column of your dataset."),
                                       hr(),
                                       h5("Options"), hr(),
                                       #selectInput("func","Select function for Reference Intervals Calculation",choices = list("None","singleRefLimit"=1),selected = "None"),
                                       selectInput("test","Select method for Reference Intervals Calculation:",choices = c("None","Horn's Algorithm"=2,"Dixon Algorithm"=3,
                                                                                                                           "Cook Distance"=4,"VanderLoo Algorithm"=5)),
                                      
                                       conditionalPanel(condition = "input.test=='2'",
                                                        h5("*Introduction"),
                                                        p(em("This function determines outliers in a Box-Cox transformed dataset using Horn’s method of outlier
                                                          detection using Tukey’s interquartile fences. If a data point lies outside 1.5 * IQR from the 1st or
                                                          3rd quartile point, it is an outlier."))),
                                       conditionalPanel(condition = "input.test=='3'",
                                                        h5("Warner!!"),
                                                        p(em("This method can only determine outliers for datasets of size 3 <= n <= 30."))
                                       ),
                                       conditionalPanel(condition = "input.test=='4'",
                                                        h5("*Introduction"),
                                                        p(em("A linear regression model is calculated for the data (which is the mean for one-dimensional data.
                                                           From that, using the Cook Distances of each data point, outliers are determined and returned."))),
                                       conditionalPanel(condition = "input.test=='5'",
                                                        h5("*Introduction"),
                                                        p(em("Separates data into vectors of outliers and a cleaned subset of the data."))),
                                       selectInput("RI","Method for Reference Interval Calculations",choices = c("None","nonparametric"="n","parametric"="p","Robust"="r"),selected = "None"),
                                       h5("*Introduction"),
                                       p(em("Method for reference interval calculations. Valid options include 'p' (default)
                                                           for parametric, 'n' for non-parametric, and 'r' for robust method.")),
                                       selectInput("ınterval","refConf",choices = c("%90","%95","%99"),selected = "%95"),
                                       selectInput("CI","Method for Confidence Interval Calculations",choices = c("None","nonparametric"="n","parametric"="p","bootstrapping"="boot"),selected = "None"),
                                       h5("*Introduction"),
                                       p(em("Method for confidence interval calculations. Valid options include 'p' for parametric (default), 
                                                          'n' for non-parametric, and 'boot' for bootstrapping method.")),
                                       selectInput("Confidence","limitConf",choices = c("%90","%95","%99"),selected = "%90"),
                                       selectInput("plot_type","Select Plot Type",choices = list("None","Boxplot"=1,"Barplot"=2))
                          ),
                          mainPanel(
                            tabsetPanel(
                            tabPanel("Reference Intervals",icon = icon("align-left"),
                                     verbatimTextOutput("refVal"),
                                     tableOutput("tab"),
                                     plotOutput("plot1")),
                            tabPanel("Plot",icon = icon("chart-line"),
                                     plotOutput("plot"),
                                     h4("Outliers-Subset Values"),
                                     verbatimTextOutput("outliers")),
                            tabPanel("Dataset Summary",icon = icon("align-left"),
                                     h4("Summary in Dataset:"),
                                     verbatimTextOutput("summary")))
                 )
                )
               )
              )#TabsetPanel
)#fluidpage

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # observe({ #clearbutton için
  #   if (input$clearText_button == 0) return()
  #   isolate({ updateTextInput(session, "myData", label = ",", value = "") })
  # })
  dataIn<-reactive({
    if(input$dataInput==0){ #load example dataset
      if(input$sampleData==1){
        data<-data.frame(set120)
      } else if(input$sampleData==2){
        data<-data.frame(set200)
      } else{
        data<-data.frame(set20)
      }
    }
    else if(input$dataInput==1){ #upload data
      Infile<-input$UploadD
      Sep<-switch(input$sep,'1'=",",'2'="\t",'3'=";", '4'="")
      
      if (is.null(input$UploadD))  {return(NULL)}
      
      if (file.info(Infile$datapath)$size <= 10485800){
        data <- read.table(Infile$datapath, sep=Sep, header=TRUE, fill=TRUE, na.strings = c("", "NA","."))
      }
      else{ print("File is bigger than 10MB and will not be uploaded.")
      }
    }
    # else if(input$dataInput==2) {  ## Paste data.
    #   if(is.null(input$myData)) {return(NULL)}
    #   
    #   tmp <- matrix(strsplit(input$myData, "\n")[[1]])
    #   mySep <- switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
    #   myColnames <- strsplit(tmp[1], mySep)[[1]]
    #   data <- matrix(0, length(tmp)+1, length(myColnames))
    #   colnames(data) <- myColnames
    #   
    #   for(i in 2:length(tmp)){
    #     myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
    #     data[i-1,] <- myRow[-length(myRow)]
    #   }
    #   
    #   data <- data.frame(data)
    # }
    
    return(data)
    
  })
  
  Variable<-reactive({return(input$variable)})
  
  observe({
    dataInput<-dataIn()
    if (!is.null(dataInput)){
      updateSelectInput(session = session, inputId = "variable", 
                        choices = colnames(dataInput), selected = colnames(dataInput)[1])
    } else {
      updateSelectInput(session = session, inputId = "variable", 
                        choices = "", selected = "")
    }
  })
  
  {
    output$table<-renderDataTable({
      dataIn()
    })
  }
  refConf<-reactive({
    switch(input$ınterval,
           '%90'=0.90,
           '%95'=0.95,
           '%99'=0.99)
  })
  limitConf<-reactive({
    switch(input$Confidence,
           '%90'=0.90,
           '%95'=0.95,
           '%99'=0.99)
  })
  output$refVal<-renderPrint({
    dataset<-dataIn()
    #if(input$func==1){
      if(input$test==2){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
      }
      else if(input$test==3){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
      }
      else if(input$test==4){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
      }
      else{
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          print(refVal)
        }
      }
     #}
  })
  output$tab<-renderTable({
    dataset<-dataIn()
      if(input$test==2){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
      }
      else if(input$test==3){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
      }
      else if(input$test==4){
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
      }
      else{
        if(input$RI=="n" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="n" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="p" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="n"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="p"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
        else if(input$RI=="r" & input$CI=="boot"){
          refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
          Tab<-data.table(ReferansIntervals=paste(format(100 * refVal$refConf), sep = ""),
                          ConfidenceIntervals=paste(format(100 * refVal$limitConf), sep = ""),
                          OutliersMethod=paste(refVal$out.method),
                          ShapiroWilk=paste(refVal$norm[[1]]),
                          KolmorgorovSmirnov=paste(refVal$norm[[2]]),
                          ReferenceInterval=paste(paste(format(refVal$Ref_Int, digits = 6), collapse = ", "),sep = " "),
                          LowerConfidenceInterval=paste(paste(format(refVal$Conf_Int[1:2], digits = 6), collapse = ", "), sep = " "),
                          UpperConfidenceInterval=paste(paste(format(refVal$Conf_Int[3:4], digits = 6), collapse = ", "), sep = " "))
        }
      }
  })
  output$plot1<-renderPlot({
    dataset<-dataIn()
    #if(input$func==1){
    if(input$test==2){
      if(input$RI=="n" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "horn",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
    }
    else if(input$test==3){
      if(input$RI=="n" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "dixon",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
    }
    else if(input$test==4){
      if(input$RI=="n" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "cook",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
    }
    else{
      if(input$RI=="n" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="n" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="n",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="p" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="p",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="n"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="n",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="p"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="p",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
      else if(input$RI=="r" & input$CI=="boot"){
        refVal<-singleRefLimit(dataset[[input$variable]],out.method = "vanderLoo",out.rm = FALSE,RI="r",CI="boot",refConf = refConf(),limitConf = limitConf(),bootStat = "basic")
        hist(dataset[[input$variable]], breaks = 8,probability = T, main = "Reference Intervals",col = '#8dce5a', xlim = c(refVal$Conf_Int[[1]],refVal$Conf_Int[[4]]))
        lines(density(dataset[[input$variable]]), col = 'orange', lwd = 2)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Ref_Int, col = '#4e90b5', lwd = 3)
        abline(v=refVal$Conf_Int, col = '#4e90b5', lty = 'dashed', lwd = 3)
      }
    }
    #}
  })
  output$plot<-renderPlot({
    h4("Outliers Intervals in Your Dataset")
    dataset<-dataIn()
    if(input$plot_type==1){ #Boxplot
      if(input$test==2){
        horn<-horn.outliers(dataset[[input$variable]])
        boxplot(horn,col = c("#8dce5a","#4e90b5"))
        }
      else if(input$test==3){
        dixon<-dixon.outliers(dataset[[input$variable]])
        boxplot(dixon,col = c("#8dce5a","#4e90b5"))
      }
      else if(input$test==4){
        cook<-cook.outliers(dataset[[input$variable]])
        boxplot(cook,col = c("#8dce5a","#4e90b5"))
      }
      else{
        vanderLoo<-vanderLoo.outliers(dataset[[input$variable]])
        boxplot(varderLoo,col = c("#8dce5a","#4e90b5"))
      }
    }
    else if(input$plot_type==2){ #BarPlot
      h4("Outliers Intervals in Your Dataset")
      if(input$plot_type==2){
        if(input$test==2){
          horn<-horn.outliers(dataset[[input$variable]])
          hornD<-c(horn$outliers,horn$subset)
          print(paste("Outliers and Subset",horn))
          barplot(hornD,xlab = "ouitliers-subset",ylab = "Values",col = c("#8dce5a","#4e90b5"),main = "Horn Outliers")
        }
        else if(input$test==3){
          dixon<-dixon.outliers(dataset[[input$variable]])
          dixonD<-c(dixon$outliers,dixon$subset)
          print(paste("Outliers and Subset",dixon))
          barplot(dixonD,xlab = "ouitliers-subset",ylab = "Values",col = c("#8dce5a","#4e90b5"),main = "Dixon Outliers")
          
        }
        else if(input$test==4){
          cook<-cook.outliers(dataset[[input$variable]])
          cookD<-c(cook$outliers,cook$subset)
          print(paste("Outliers and Subset",cook))
          barplot(cookD,xlab = "ouitliers-subset",ylab = "Values",col = c("#8dce5a","#4e90b5"),main = "Cook Outliers")
        }
        else{
          vanderLoo<-vanderLoo.outliers(dataset[[input$variable]])
          vanderLooD<-c(vanderLoo$outliers,vanderLoo$subset)
          print(paste("Outliers and Subset",vanderLoo))
          barplot(vanderLooD,xlab = "ouitliers-subset",ylab = "Values",col = c("#8dce5a","#4e90b5"),main = "VanderLoo Outliers")
        }
      }
    }
    
  })
  output$outliers<-renderPrint({
    dataset<-dataIn()
      h4("Outliers Intervals in Your Dataset")
        if(input$test==2){
          horn<-horn.outliers(dataset[[input$variable]])
          hornD<-c(horn$outliers,horn$subset)
          print(horn)
        }
        else if(input$test==3){
          dixon<-dixon.outliers(dataset[[input$variable]])
          dixonD<-c(dixon$outliers,dixon$subset)
          print(dixon)
         
        }
        else if(input$test==4){
          cook<-cook.outliers(dataset[[input$variable]])
          cookD<-c(cook$outliers,cook$subset)
          print(cook)
        }
        else{
          vanderLoo<-vanderLoo.outliers(dataset[[input$variable]])
          vanderLooD<-c(vanderLoo$outliers,vanderLoo$subset)
          print(vanderLoo)
        }
  })
  output$summary<-renderPrint({
    dataset<-dataIn()
    summary(dataset)
  })
} #server


# Run the application 
shinyApp(ui = ui, server = server)
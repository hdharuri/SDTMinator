#library(data.table)
library(ggplot2)
source("./helper.R")
getPrediction <- function(ptest) {
  pred <- unlist(strsplit(ptest,";"))
  for (i in 1:length(pred)) {
    eval(parse(text = pred[i]))
  }
  p <- eval(parse(text = pred[length(pred)]))
  
  p
}

getChiSqStatistic <- function(ptest1,ptest2) {
  tab<- matrix(c(275,68,600,50),ncol=2,byrow=TRUE)
  colnames(tab) <- c("control","case")
  rownames(tab) <- c("hasVariant","doesNot")
  test_result <- chisq.test(tab)
  test_result
}

fileName <- vector(mode="character",length=0)
fileName_base <- vector(mode="character",length=0)
tableNames <- vector(mode="character",length=0)
metaData <- data.frame(matrix(NA,nrow=0,ncol=4),stringsAsFactors=FALSE)
colnames(metaData) <- c("Working_directory","Annotation_table","Mapper_file","Domains")

mainScript <- "./main.R"
server <- shinyServer(function(input, output,session) {
  
  inFile <- reactive({
    #mfile <- input$file
    mfile <- input$select
    if (is.null(mfile)){return(NULL)}
    #metaData_raw <- data.table(read.table(mfile$datapath,fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=TRUE))
    if (mfile == "2") {
      metaData_raw <- data.table(read.table("metaData.txt",fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=TRUE))
      metaData <<- metaDataNATOR(metaData_raw)
      fileName <<- unique(as.character(metaData[,1]))
      fileName_base <<- basename(fileName)
      metaData
    }
  })
  
  output$varselect <- renderUI({
    if(is.null(inFile())){return()}
    list(
      radioButtons("var", "File Type", choices = "MetaData", select = "MetaData")
      , div(em(strong("submit these tables for conversion to SDTM format?")),style="color:#0099FF"), 
      br(),
      actionButton('submit', 'Submit')
    )
  })
  
  observe({
    if (is.null(input$submit)) {return()}
    if (input$submit > 0) {
      source(mainScript,local = TRUE)
      output$varselect <- renderUI({
        radioButtons("var", "File Type", choices = c("MetaData",fileName_base), select = "MetaData")
      })
    }
  })
  
  output$twotabs <- renderUI({
    if (is.null(input$var)) {return(NULL)
    } else if (input$var=='MetaData') {
      
      tabsetPanel (
        tabPanel(title=span(strong("Meta data"),style="color:#0099FF"),dataTableOutput("metadata"))
      )
      
    } else if (input$var %in% fileName_base) {
      fy <- input$var
      fy <- fileName[grep(paste0("^.*",fy,"$"),fileName)]
      dirname <- paste0(fy,"/Tables")
      f <- reactive({list.files(dirname)})
      filePath <- reactive({paste(dirname,f(),sep="/")})
      domainTabName <- filePath()
      tableNames <<- filePath()
     
      tabs <- lapply(filePath(), function(nm) {
        domainTabName <- gsub("\\.txt$","",basename(nm))
        tabPanel(title=span(strong(domainTabName),style="color:#0099FF"),dataTableOutput(domainTabName)) #style='background-color:#DC143C'
      }    
      )
      do.call(tabsetPanel, c(tabs))
    }
  })
  observe({
    if (is.null(input$var)) {return(NULL)}
    output[['metadata']] <- renderDataTable(metaData)
    
  })
  
  observe({
    if (is.null(input$var)) {return(NULL)}
    if (input$var %in% fileName_base & input$submit > 0) {
      mfile1 <- input$var
      lapply(tableNames,function(g) {
        domainTab <- gsub("\\.txt$","",basename(g))
        ## This is to test if empty columns can be removed before rendering
        tableItself <- read.table(g,fill=TRUE,header=TRUE,sep="\t",comment.char = "",row.names=NULL,quote="\"",colClasses="character",check.names=FALSE)
        tableItself <- tableItself[,colSums(tableItself=="")<nrow(tableItself)]
        output[[domainTab]] <- renderDataTable(tableItself)
      })
    }
  })
##*************************************************************************************************#  
  h <- eventReactive(input$actionButton,{
    req(input$actionButton)
    getPrediction(input$caption)
  })
  
  output$plot <- renderPlot({
    #d <- as.numeric(lenCaption())
    # d
    h()
  })
  
  ##*************************************************************************************************# 
  
  num <- eventReactive(input$action3,{
    req(input$e2)
    getChiSqStatistic(input$e0,input$e2)
  })
  output$ex_out <- renderPrint({
    testResult <- paste0("ChiSq statistic: ",num()$statistic,"  ","p-value: ",num()$p.value)
    testResult
  })
  
})
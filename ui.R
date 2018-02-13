#tags$head(tags$script(src = "tableinput.js"))
#tags$head(tags$link(rel="stylesheet", type="text/css", href="tableinput.css"))
shinyUI(fluidPage(
  
  titlePanel(span(strong("SDTM-inator"),style="color:orange")),
  
  br(),
  sidebarPanel(
    div(strong("SDTM-inator converts legacy NextBio clinical data to SDTM format. The input for SDTM-inator is a tab-delimited meta data file containing the name of the directory, name(s) of the data and the corresponding mapper file."),style="color:#0099FF"), 
    br(),
    
    fileInput(
      'file', 
      'Choose file to upload.'
    ),
    br(),
    br(),
    uiOutput('varselect')
    
    
    
  ),
  mainPanel(
    
    #dataTableOutput("twotabs")#,
    
    uiOutput("twotabs")
    #uiOutput("SDTMtables")
  )
  
))
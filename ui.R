#tags$head(tags$script(src = "tableinput.js"))
#tags$head(tags$link(rel="stylesheet", type="text/css", href="tableinput.css"))
library(shinythemes)

tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "cerulean",   #<--- To use a theme, uncomment this
    div(strong(h1("Clinical-Next")),style="color:darkred"),
    tabPanel(div(strong(h3("SDTM-inator")),style="color:darkblue"),
fluidPage(
  
  titlePanel(span(strong("SDTM-inator"),style="color:orange")),
  br(),
  sidebarPanel(
    div(strong("SDTM-inator converts legacy clinical data to SDTM format. The input for SDTM-inator is a tab-delimited meta data file containing the name of the directory, name(s) of the data and the corresponding mapper file."),style="color:#0099FF"), 
    br(),
    
    selectInput("select", label = h3("Select mapper file"), 
                choices = list(" " = 1, "mapper_file_ulcerative_colitis" = 2, "mapper_file_RA" = 3), 
                selected = 1),
    
    br(),
    br(),
    uiOutput('varselect')
  
  ),
  mainPanel(
    uiOutput("twotabs")
    #uiOutput("SDTMtables")
  )
  
#))
)

),
tabPanel(div(strong(h3("PhenoAnalyzer")),style="color:darkblue"), 
         
         fluidPage(
           # App title ----
           titlePanel(span(strong("Phenotype-Analyzer"),style="color:orange")),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               div(strong("PhenoAnalyzer is a tool to analyze phenotype/patient specific data through queries written in a language of your choice."),style="color:#0099FF"), 
               br(),
               textAreaInput("caption", "Query",
                             '
options(warn = -1)
disease_datasets <- list.dirs("./SDTM_Table",recursive = FALSE)
dt_cm <- data.table(do.call(rbind,lapply(as.list(disease_datasets), function(disease) {
if("CM.txt" %in% list.files(disease)) {
filename_cm <- paste0(disease,"/CM.txt")
tbl_cm <- fread(filename_cm)
dt_per_disease <- tbl_cm[,.(`Number of Subjects` = length(unique(USUBJID))), by = CMTRT]
dt_per_disease$Disease <- disease
return(dt_per_disease)
}})))
dt_cm$CMTRT <- tolower(dt_cm$CMTRT)
pattern <- \'"^.\\\\/(.*)$"\'
dt_cm$Disease <- gsub(pattern,"\\\\1",dt_cm$Disease)
p4 <- ggplot() + geom_bar(aes(y = `Number of Subjects`, x = CMTRT, fill = Disease), data = dt_cm, stat="identity") + theme(axis.text.x = element_text(angle = -90))
p4 +  labs(title="Query: Non-protocol driven medication across studies",subtitle = "Mapping to a common data model facilitates easier querying across disparate data sources")
                             ', 
                             
                             
                             width = "250px",height = "500px"),
               actionButton("actionButton", label = "Submit")
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               # tags$style(type = 'text/css', 
               #            "footer{position: absolute; bottom:5%; left: 33%; padding:5px;}"
               # ),
               # Output: Tabset w/ plot, summary, and table ----
               # tabsetPanel(type = "tabs",
               #             tabPanel("Plot", plotOutput("plot"))
               # )
               
               plotOutput("plot")
               
             )
           )
         )
         
         ),

tabPanel(div(strong(h3("PheGen-Analyzer")),style="color:darkblue"), 
         # App title ----
         titlePanel(span(strong("PheGen-Analyzer"),style="color:orange")),
         
         fluidPage(
           title = 'Selectize examples',
           sidebarLayout(
             sidebarPanel(
               div(strong("PheGen-Analyzer is a precision medicine tool to combine phenotype (patient data) with sequencing data"),style="color:#0099FF"), 
               br(),
               textInput("vid", "Variant input:", "22:200200:200201:T"),
               selectInput(
                 'e0', 'Case', choices = c("Asthma","Crohn","Multiple_Myeloma","Multiple_Sclerosis","Psoriasis","Rheumatoid_Arthritis","T2D","Ulcerative_Colitis"),
                 selectize = FALSE
               ),
               selectizeInput(
                 'e2', 'Control', choices = c("Asthma","Crohn","Multiple_Myeloma","Multiple_Sclerosis","Psoriasis","Rheumatoid_Arthritis","T2D","Ulcerative_Colitis"), multiple = TRUE
               ),
               actionButton("action3", "Submit", class = "btn-primary")
             ),
             mainPanel(
               helpText(strong('Chi-square test statistic:')),
               verbatimTextOutput('ex_out')
             )
           )
         )
         
         )

)
)
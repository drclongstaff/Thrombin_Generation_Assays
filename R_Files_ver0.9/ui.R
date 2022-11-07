library(shiny)
#library(shinythemes)# Load the library
#library(thematic)

Thisversion <- 0.91
thematic::thematic_shiny()
fluidPage(                       # Setup the UI page
  theme = bslib::bs_theme(bootswatch = "simplex"),
  #includeCSS("./www/styles2.css"),
  # Add a title
  titlePanel("Thrombin Generation Assays"),        
  
  sidebarLayout(                                      
    
    sidebarPanel( 
     
      tags$h3("Calibrator Data"),
      
      
      
      fluidRow(
        column(9,  fileInput("data0", label = "Calibrator csv, txt or xlsx")), 
        
        column(3, numericInput("sheetc", label = h5("Excel sheet"), value = 1, step = 1))
        
        
      ),
      
      fluidRow(
        
        column(4, numericInput("calstart",
                               label = h5("start col"), value = 2)),
        
        column(4, numericInput("calend",
                               label = h5("end col"), value = 3)),
      ),
      
      fluidRow(
        
        column(5,   uiOutput("whatCal")),
        
        column(5, numericInput("limitD", label = h5("limit points for fitting"), value = 85, step = 5))
        
        
        
        
      ),
      
      fluidRow(
        
        column(4, numericInput("CalibT", label = h5("Conc of calibrator"), value = 110, step = 10)),
        
        column(4, numericInput("calSlope", label = h5("Calibrator initial rate"), value = 78, step = 10))#,
        
        
        ),
    
      
      tags$h3("Sample Data"), 
      
      fluidRow(
          column(9,  fileInput("data1", label = "Sample csv, txt, xlsx")), 
          
          column(3, numericInput("sheetd", label = h5("Excel sheet"), value = 1, step = 1))
          
      ),
      
      fluidRow(
        
        column(4, numericInput("numrows",
                             label = h5("plot nrows"), value = 4)),
        #label = h5("plot nrows"), value =strtoi(textOutput("value") ))),
        #label = h5("plot nrows"), value =strtoi(uiOutput("value") ))),
        #label = h5("plot nrows"), value =strtoi("14" ))),
        
        column(4,   numericInput("truncpoints",label = h5("truncate data"), value = 30))
      ),
      
      
      fluidRow(
      
        column(4, numericInput("datstart",
                               label = h5("start col"), value = 2)),
        
        column(4, numericInput("datend",
                               label = h5("end col"), value = 9)),
        
        column(4, radioButtons(inputId="Transf", label = "Method for correction ", 
                   choices = c("none" ,  "Polynomial"), selected = "none"))
      
     ),
     
     tags$h3("Transformed curves"), 
     
     fluidRow(
      
        column(4, numericInput("lagChange", label = h5("lag % for start"), value = 10, step = 1)),
        
        column(4, numericInput("smtail", label =h5("Smooth tail"), value = 50)),
       
       column(4, radioButtons(inputId="a2Mcor", label = "Display", choices =c( "F", "Thrombin", "Smooth", "no T-alpha-2M"), selected = "Thrombin"))
       
     )
     
    
    ),
    
    mainPanel( 
      tabsetPanel(#type="tab",
      tabPanel("Calibrator", #value = 1,
              
               fluidRow(
                 column(6,plotOutput("myCalib")),
                column(6,plotOutput("mytest"))),
                plotOutput("myplotsF"),
                
              ),
      tabPanel("Plots",
               
               plotOutput(outputId = "myplotAll"), 
               
               radioButtons(inputId="tabRes", label = h4("Select parameter", align = "center"), 
                            choices = c("Column names"=1, "Initial reading"=2, "Lag time "=3,  "Area under the curve"=4,
                                    "Peak "=5, "ttPeak"=6, "ttTail"=7, "Lag reading"=8 ),
                            selected = 6, inline = TRUE, width = "100%"),
               
              tags$h4("Results from plots"),
              
               tableOutput("plotsTable"), align = "center"
              
               
               ), 
      
      tabPanel("Curve",
               
               tags$h4("Individual curves"),
               uiOutput("what"),
               plotOutput("myplot"),
               tableOutput("curveTable"), align = "center"
               
      ),
      tabPanel("Results",
               
               tags$h4("Table of all results"),
               tableOutput("resultsTable"), align = "center"
        
      ),
      tabPanel("Raw Data",
               
               helpText("Raw fluorescence or plotted data"),

               radioButtons("whatRaw", label = h4("Show data", align = "center"),
                            choices = c("Raw fluorescence", "Analysed"), selected = "Raw fluorescence", inline = TRUE, width = "100%"), 
               tableOutput("rawData"), align = "center"
               
      ),
      
      tabPanel("Calibrator Data",
        
               tableOutput("calData"), align = "center"
      ),
      
      tabPanel("Settings",
               
               helpText(h5(paste("Simple thrombin generation app version number", Thisversion))),
              fluidRow(
               column(6, tags$h5("Calibrator file"),uiOutput("calset")),
               column(6, tags$h5("Data file"),uiOutput("fileset"))
               
      ),
      
               tableOutput("settings"), align = "center"
               ),
      
      
      tabPanel("Help",
               shinythemes::themeSelector(),
                   tags$h3("help notes"),           
               tags$blockquote(h5 ("►Calibrator tab",
                                   tags$br(),
                                   tags$br(),
                                   "►Load calibrator data and select a well: csv, txt or xlsx file types are accepted",
                                   tags$br(),
                                   "►Columns of calibrators or data can be selected over a range from 'start' to 'end' or on different Excel sheets",
                                   tags$br(),
                                   "►A curve (magenta -) is fitted to the calibrator fluorescence data (blue +) and intial rate calculated (red O)",
                                   tags$br(),
                                   "►The number of points selected for fitting should be chosen to cover the fluorescence range in the sample data",
                                   tags$br(),
                                   "►It is advisable to limit the number of points to be fitted to avoid noisy, unused data late in the time course",
                                   tags$br(),
                                   "►A polynomial correction algorithm is applied to correct the data for substrate depletion and inner filter effect (green X)",
                                   tags$br(),
                                   "►The quality of the fit is shown in the graphs to the right",
                                   tags$br(),
                                   "►Sample data are loaded using the dialogue box as csv, txt or xlsx" ,
                                   tags$br(),
                                   "►Figures of fluorescence versus time are shown and can be arranged by number of rows",
                                   tags$br(),
                                   "►'No correction' or 'polynomial correction' can be applied using the radio buttons",
                                   tags$br(),
                                   tags$br(),
                                   "►Plots and Curve tabs",
                                   tags$br(),
                                   tags$br(),
                                   "►Transformed data plots showing thrombin concentration are shown in these tabs",
                                   tags$br(),
                                   "►Results tables are also shown for a selected parameter ('Plots') or all parameters for a single curves ('Curve')",
                                   tags$br(),
                                   "►The end of the lag phase is signalled by a change of 10% in trombin concentration but this can be adjusted",
                                   tags$br(),
                                   "►Tail smoothing is used to select a steady rate to calculate the final Thrombin-alpha-2M contribution (gold curves)",
                                   tags$br(),
                                   "►Smooth the curves after the signal has returned to baseline and smoothing can be seen using the radio button selection",
                                   tags$br(),
                                   "►Graphs can display fluorescence or thrombin concentration with or without subtraction of the Thrombin-alpha-2M activity",
                                   tags$br(),
                                   "►Lag time and ttTail are the points where the curve reaches or returns to 10% (or selected value) of max",
                                   tags$br(),
                                   "►These points are also used as boundaries to calculate the area under the curve",
                                   tags$br(),
                                   tags$br(),
                                   "►Other tabs",
                                   tags$br(),
                                   tags$br(),
                                   "►'Results' presents a table of results",
                                   tags$br(),
                                   "►'Raw data' shows all the raw data and fluorescence. Thrombin or corrected thrombin values (with or without alpha-2-M-thrombin) can be selected",
                                   tags$br(),
                                   "►'Calibrator data' shows raw calibrator data columns as selected ",
                                   tags$br(),
                                   "►'Settings' summarises the setting used to generate the analysis as an aid to reproducibility",
                                   tags$br(),
                                   "►'Help' includes a theme selector drop down to permit control of fonts, colour and design ",
                                   tags$br(),
                                   tags$br(),
                                   "More detailed instructions and the code can be found in my github site:",
                                   tags$a (href="https://github.com/drclongstaff/Thrombin_Generation_Assays", "Here")
                                   #tags$a (href="https://github.com/drclongstaff/Thrombin_Generation/blob/master/Thrombin_Generation_help_notes_2019.pdf", "Here")
               ))
      )
    
      
      
           )
          )  
    
   
    
  )
 
)

#id = "tabselected"


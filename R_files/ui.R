library(shiny)                   # Load the library

Thisversion <- 0.6

fluidPage(                       # Setup the UI page
  includeCSS("styles2.css"),
  titlePanel("Thrombin Generation Assays"),         # Add a title
  
  sidebarLayout(                                      
    
    sidebarPanel( 
     
      tags$h3("Calibrator Data"),
      
      
      
      fluidRow(
        column(5,  fileInput("data0", label = "Select calibrator")), 
        
        helpText(h5("Load csv files "))
        
        
      ),
      
      fluidRow(
        
        column(5,   uiOutput("whatCal")),
        
        column(5, numericInput("limitD", label = h5("limit points for fitting"), value = 85, step = 5))
        
        
        
        
      ),
      
      fluidRow(
        
        column(4, numericInput("CalibT", label = h5("Conc of calibrator"), value = 107, step = 10)),
        
        column(4, numericInput("calSlope", label = h5("Calibrator initial rate"), value = 80.2, step = 25))#,
        
        
        ),
    
      
      tags$h3("Sample Data"), 
      
      fluidRow(
          column(5,  fileInput("data1", label = "Select sample data")), 
          
          column(5, numericInput("numrows",
                                 label = h5("How many rows?"), value = 2))
          
      ),
      
      fluidRow(
      
      column(6, radioButtons(inputId="Transf", label = "Method for correction ", 
                   choices = c("none" ,  "Polynomial"), selected = "none")),
     
       column(4,   numericInput("truncpoints",label = h5("truncate the data"), value = 0))
      
      #"H-transform",
      #column(6, numericInput("alpha", label = h5("alpha for H-transform"), value = 36000, step = 500))
     
      
     ),
     
     tags$h3("Transformed curves"), 
     
     fluidRow(
      
        column(4, numericInput("lagChange", label = h5("lag %"), value = 10, step = 1)),
        
        column(4, numericInput("smtail", label =h5("Smooth tail"), value = 75)),
       
       column(4, radioButtons(inputId="a2Mcor", label = "Display", choices =c("F", "Thrombin", "Smooth", "no T-alpha-2M"), selected = "Thrombin"))
       
     )
     
    
    ),
    
    mainPanel( 
      tabsetPanel(#type="tab",
      tabPanel("Calibrator", #value = 1,
                plotOutput("myCalib"),
                plotOutput("myplotsF"),
                
               #h5(helpText("Calibrator, initial rate and correction for ideal fluorescence")),
               # h5(helpText("Raw fluorescence data before any processing")), 
               tableOutput("Fluor")
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
               
               #helpText("This is the first derivative data"),
               #numericInput("resCol", label = h5("copy which column"), value = 1),
               uiOutput("nowwhat"),
               radioButtons("whatRaw", label = h4("Show data", align = "center"),
                            choices = c("Raw fluorescence", "Thrombin", "Corrected"), selected = "Corrected", inline = TRUE, width = "100%"), 
               tableOutput("rawData"), align = "center"
               
      ),
      
      tabPanel("Settings",
               
               helpText(h5(paste("Simple thrombin generation app version number", Thisversion))),
               uiOutput("fileset"),
               
               tableOutput("settings"), align = "center"
               ),
      
      
      tabPanel("Help",
               
                   tags$h3("help notes"),           
               tags$blockquote(h5 ("►Calibrator tab",
                                   tags$br(),
                                   tags$br(),
                                   "►Load calibrator data and select a well",
                                   tags$br(),
                                   "►A curve is fitted to the data (blue line) and intial rate cacluated (red line)",
                                   tags$br(),
                                   "►A polynomial correction algorithm is applied to correct the data for substrate depletion and inner filter effect (green crosses)",
                                   tags$br(),
                                    "►It is advisable to limit the number of points to be fitted to avoid noisy data late in the time course",
                                    tags$br(),
                                   "►Sample data are loaded using the dialogue box",
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
                                   "►The end of the lag phase is signalled by a change of 10% in Thrombin concentration but this can be adjusted",
                                   tags$br(),
                                   "►Tail smoothing is used to select a steady rate to calculate the final Thrombin-alpha-2M contribution (gold curves)",
                                   tags$br(),
                                   "►Smooth the curves after the signal has returned to baseline and smoothing can be seen using the radio button selection",
                                   tags$br(),
                                   "►Graphs can display fluorescence or thrombin concentration with or without subtraction of the Thrombin-alpha-2M activity",
                                   tags$br(),
                                   "►Uncorrected thrombin geneneration curves are always shown using the green dashed line", 
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
                                   "►'Raw data' shows all the raw data and fluorescence, thrombin or corrected thrombin values can be selected",
                                   tags$br(),
                                   "►'Settings' summarises the setting used to generate the analysis as an aid to reproducibility",
                                   tags$br()#,
                                  # "More detailed instructions can be found here",
                                  # tags$a (href="https://github.com/drclongstaff/Thrombin_Generation/blob/master/Thrombin_Generation_help_notes_2019.pdf", "Here")
               ))
      )
    
      
      
           )
          )  
    
   
    
  )
 
)

#id = "tabselected"


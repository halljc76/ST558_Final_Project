library(shinythemes)
library(shinyWidgets)
library(shinyjs)

shinyUI(
  navbarPage(
    useShinyjs(),
    setBackgroundImage(src = "roosevelt4.jpg"),
    title = "Forest Soil Cover Types",
    theme = shinytheme("simplex"),
    tabPanel(
      title = "About",
      fluidRow(
        h2("Purpose of This Application", style = "color:white"),
        h3("This application is the final project of the Fall 2023 course ST 558: Data 
            Science for Statisticians at North Carolina State Univeristy. It houses
            an exploration into a dataset involving soil cover types in forests through
            both an interactive and predictive medium, allowing users the ability to 
            fit, test, and evaluate appropriate statistical models.", style = "color:white"),
        style = "margin-left:10px;"
      ),
      fluidRow(
        h2("Info About the Data Used", style = "color:white"),
        fluidRow(
          column(width = 4, 
                 h3(tags$b("Title 1: Origin of Dataset", style = "color:white")),
                 h4("This dataset contains cartographic information derived from data originally 
                   from the United States Geological Survey (USGS) and United States Forestry Service (USFS) 
                   about wilderness areas within the Roosevelt National Forest, Colorado (the background image!). 
                   
                   The observations within are, per Jock Blackard, the author of this dataset, 
                   and Colorado State University, characteristic of areas within minimal human 
                   impact as to allow for the prediction/classification of soil types without the 
                   need to account for excessive human \"error\" (i.e., activity).",
                   style = "color:white")
          ),
          column(width = 4, 
                 h3(tags$b("Title 2: Information Within the Dataset", style = "color:white")),
                 h4("The dataset has been utilized in the Ph.D. dissertation work of Jock Blackard, 
                   where their work on utilizing neural networks and discriminant analysis to predict 
                   these forest cover types leads to the structure of the dataset seen here. 
                   
                   A total of twelve cartographic measures were utilized as independent variables 
                   in the predictive models, while seven major forest cover types were used as 
                   dependent variables.", style = "color:white"),
                 tags$ul(
                   tags$li("Elevation", style = "color:white"),
                   tags$li("Aspect", style = "color:white"),
                   tags$li("Slope", style = "color:white"),
                   tags$li("Horizontal Distance to Hydrology", style = "color:white"),
                   tags$li("Vertical Distance to Hydrology", style = "color:white"),
                   tags$li("Horizontal Distance to Roadways", style = "color:white"),
                   tags$li("Hillside Index at 9am, 12pm, and 3pm", style = "color:white"),
                   tags$li("Horizontal Distance to the Nearest Wildfire Ignition Points", style = "color:white"),
                   tags$li("Soil Type (40 possible types, based on the USFS ELU Codes)", style = "color:white")
                 )
          ),
          column(width = 4, 
                 h3(tags$b("Title 3: General Overview", style = "color:white")),
                 h4("The data has 581012 observations and 55 features and is imbalanced in classification; 
                   however, there are no missing entries within the data, and the data dictionary 
                   accompanying the data, which can be found",
                   tags$a(href = "https://archive.ics.uci.edu/dataset/31/covertype", target = "_blank", HTML("here,")),
                   "shows no evidence of mass imputation.", style = "color:white"
                 )
          )
        ),
        style = "margin-left:10px;"
      )
    ),
    tabPanel(
      title = "Data Exploration",
      sidebarLayout(
        sidebarPanel(
          h4("Add Conditions to Filter Data"),
          actionButton("addFilterCond", label = "Wait for Data...",
                       class = "btn-primary"),
          div(id = "conditions"),
          actionButton(inputId = "doFilter", label = "Filter Data",
                       btn = "class-secondary"),
          hr(),
          h4("Select Variables to Visualize"),
          fluidRow(
            column(width = 6,
                   selectInput("plot1Vars", label = "Plot #1 Variable",
                               choices = NULL, multiple = F)),
            column(width = 6,
                   selectInput("plot1Type", label = "Plot #1 Visualization",
                               choices = c("Histogram", "Boxplot"), 
                               multiple = F),
                   conditionalPanel("input.plot1Type == 'Histogram'",
                                    numericInput("numHistBins",
                                                 label = "Specify Number of Bins",
                                                 value = 10, min = 1)))
          ),
          fluidRow(
            column(width = 4,
                   selectInput("plot2XVar", label = "Plot #2 X-Variable",
                               choices = NULL, multiple = F)),
            column(width = 4,
                   selectInput("plot2YVar", label = "Plot #2 Y-Variable",
                               choices = NULL, multiple = F)),
            column(width = 4,
                   selectInput("plot2Type", label = "Plot #2 Visualization",
                               choices = c("Scatterplot", "Barplot"), 
                               multiple = F))
          ),
          fluidRow(
            column(
              width = 6,
            checkboxInput("plot2Group", 
                          label = "Add a Grouping/Coloring Variable for Plot 2?",
                          value = F)),
            column(width = 6,
            conditionalPanel(
              condition = "input.plot2Group == true",
              selectInput("plot2Grouping", label = "Plot #2 Grouping/Coloring Variable",
                          choices = NULL, multiple = F)
                )
              )
          ),
          hr(),
          h4("Select Variables to Summarize"),
          selectInput("summaryVars", label = "Select Variable(s)", 
                      choices = NULL, multiple = T),
          style = "overflow-y:scroll; height:80%;"
        ),
        mainPanel(
          fluidRow(
            column(width = 6,
                   panel(
                     heading = "Plot #1",
                     addSpinner(plotOutput("plot1",height = "275px")),
                     style = "height:80%;"
                   )
            ),
            column(width = 6,
                   panel(
                     heading = "Plot #2",
                     addSpinner(plotOutput("plot2", height = "275px")),
                     style = "height:300px;"
                   )
                   ),
            
          ),
          fluidRow(
            panel(heading = "Summary",
                  verbatimTextOutput("summary"))
          )
        )
      )
    ),
    tabPanel(
      title = "Modeling",
      tags$style(HTML("
        .tabbable > .nav > li > a[data-value='Modeling Info'] {background-color: grey; color:white}
        .tabbable > .nav > li > a[data-value='Model Fitting'] {background-color: grey; color:white}
        .tabbable > .nav > li > a[data-value='Prediction'] {background-color: grey; color:white}
        .tabbable > .nav > li[class=active]    > a {background-color: white; color:orange}
      ")),
      tabsetPanel(
        tabPanel(title = "Modeling Info", style = "background-color: #FFFFFF !important"),
        tabPanel(title = "Model Fitting",
                 sidebarLayout(
                   sidebarPanel(
                     h4("Train-Test Split Parameters"),
                     numericInput("splitProp", label = "Proportion of Data in Train Set",
                                  value = 0.7, min = 0, max = 1,step = 0.1),
                     hr(),
                     
                     h4("Random Forest Hyperparameters"),
                     fluidRow(
                       column(width = 6,
                              selectInput("mtrySel", 
                                          label = "Select Possible Number of Parameters for Random Forests",
                                          choices = NULL,multiple = T)),
                       column(width = 6,
                              numericInput("kfoldCVSel",
                                          label = "Select Number of Folds for CV, k",
                                          min = 1, value = 5))
                     ),
                     selectInput("rfParams", label = "Specify Predictors for Random Forest Model",
                                 choices = NULL, multiple = T),
                     
                     hr(),
                     
                     h4("Multinomial Logistic Regression Hyperparameters"),
                     selectInput("mlrParams", label = "Specify Predictors for Logistic Regression Model",
                                 choices = NULL, multiple = T),
                     
                     hr(),
                     actionButton("trainModels", label = "Train Both Models")
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6,
                              panel(
                                heading = "Random Forest Model",
                                h5("Model Fit Summary"),
                                div(id = "rfSummary"),
                                h5("Train Fit Results"),
                                verbatimTextOutput("rfTrainTable"),
                                h5("Test Fit Results"), 
                                h6("Note: Column headings are reference values!"),
                                verbatimTextOutput("rfTable"),
                                style = "overflow-x:scroll;
                                         overflow-y:scroll;"
                              )
                       ),
                       column(width = 6,
                              panel(
                                heading = "Logistic Regression Model",
                                h5("Model Fit Summary"),
                                div(id = "mlrSummary"),
                                h5("Train Fit Results"),
                                verbatimTextOutput("mlrTrainTable"),
                                h5("Test Fit Results"), 
                                h6("Note: Column headings are reference values!"),
                                verbatimTextOutput("mlrTable"),
                                style = "overflow-x:scroll;
                                         overflow-y:scroll;"
                              )
                       ),
                       
                     )
                   )
                 ),
                 style = "background-color: #FFFFFF !important"),
        tabPanel(title = "Prediction",
                 sidebarLayout(
                   sidebarPanel(
                     div(id = "dynamicPredictors")
                   ),
                   mainPanel(
                     uiOutput("predResults")
                   )
                 ),
                 style = "background-color: #FFFFFF !important")
      )
    ),
    footer = tags$footer(
      style = "text-align: center; padding: 10px; background-color: #f4f4f4;
      position: fixed; bottom: 0; left: 0; width:100%",
      tags$p("Connect with me:"),
      tags$a(href = "https://halljc76.github.io", 
             "My Blog", 
             style = "color: #333; text-decoration: none; font-weight: bold; margin-right: 10px;"),
      tags$a(href = "https://github.com/halljc76", 
             "GitHub", 
             style = "color: #333; text-decoration: none; font-weight: bold; margin-right: 10px;"),
      tags$a(href = "https://www.linkedin.com/in/carter-hall-a262ab1a6/", 
             "LinkedIn", 
             style = "color: #333; text-decoration: none; font-weight: bold;")
    )
  )
)

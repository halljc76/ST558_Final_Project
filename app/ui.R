library(shinythemes)
library(shinyWidgets)

shinyUI(
  navbarPage(
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
                   
                   A total of twelve cartographic measures were utitags$lized as independent variables 
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
          uiOutput("conditionButton"),
          div(id = "conditions"),
          style = "overflow-y:scroll; height:600px;"
        ),
        mainPanel()
      )
    ),
    tabPanel(
      title = "Modeling"
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
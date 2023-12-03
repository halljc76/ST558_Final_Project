library(shinythemes)
library(shinyWidgets)

shinyUI(
  navbarPage(
    
    ### Global UI Configuration
    setBackgroundImage(src = "roosevelt4.jpg"),
    title = "Forest Soil Cover Types",
    theme = shinytheme("simplex"),
    #############################
    
    
    ### About Tab
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
    
    ### Data Exploration Tab
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
          selectInput("summaryType", label = "Type of Summary Between Columns",
                      choices = c("Independent Summary of Each", "Contingency Table"),
                      selected = c("Independent Summary of Each")),
          style = "overflow-y:scroll; height:80%;"
        ),
        
        ### Data Exploration Visualizations
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
    
    ### Modeling Tab
    tabPanel(
      title = "Modeling",
      tags$style(HTML("
        .tabbable > .nav > li > a[data-value='Modeling Info'] {background-color: grey; color:white}
        .tabbable > .nav > li > a[data-value='Model Fitting'] {background-color: grey; color:white}
        .tabbable > .nav > li > a[data-value='Prediction'] {background-color: grey; color:white}
        .tabbable > .nav > li[class=active]    > a {background-color: white; color:orange}
      ")),
      tabsetPanel(
        
        ### MathJAX Stuff!
        tabPanel(title = "Modeling Info", 
                 
                 h3("What is Multinomial Logistic Regression?"),
                 withMathJax(
                   helpText(
                     "Multinomial logistic regression is a genearlized linear model that can be thought of as an extension of binary logistic regression by instead considering a dependent variable with more than two outcomes. ",
                     "Suppose we have \\(K\\) categories/levels/factors of some response, \\(Y\\). The model estimates \\(K-1\\) sets of parameters, i.e. coefficients, and lets the 'last' category serves as the reference category. The probability of an observation belonging to the \\(k\\)-th category (where \\(k = 1, 2, \\ldots, K-1\\)) is modeled using the softmax function.",
                     "The model can be represented as follows, for \\(k < K \\):",
                     "\\[ P(Y = k | X) = \\frac{e^{\\beta_{0k} + \\beta_{1k}X_1 + \\ldots + \\beta_{pk}X_p}}{1 + e^{\\beta_{01} + \\beta_{11}X_1 + \\ldots + \\beta_{p1}X_p} + \\ldots + e^{\\beta_{0(K-1)} + \\beta_{1(K-1)}X_1 + \\ldots + \\beta_{p(K-1)}X_p}} \\]",
                     "Here:",
                     "\\( P(Y = k | X) \\) is the probability of the observation belonging to the \\(k\\)-th category, ",
                     "\\( X_1, X_2, \\ldots, X_p \\) are the independent variables, and",
                     "\\( \\beta_{0k}, \\beta_{1k}, \\ldots, \\beta_{pk} \\) are the parameters to be estimated for the \\(k\\)-th category.",
                     "The softmax function ensures that the probabilities sum to 1 across all categories.",
                     "The likelihood function for the multinomial logistic regression model is derived from the joint probability of observing the given set of categories for all observations. The goal is to maximize this likelihood function, which is equivalent to minimizing the cross-entropy loss.",
                   style = "color: black;")
                 ),
                 
                 withMathJax(
                   helpText("We can think of Multinomial Logistic Regression as akin to fitting \\(K\\) individual binary logistic regression models and then 
                             classifying an observation based on the maximum probability of belong to each model's positive class.", style = "color:black;")
                 ),
                 
                 fluidRow(
                   column(6,
                          tags$h4("Pros:",style="color:black"),
                          tags$ul(
                            tags$li(tags$strong("Handles Multiple Classes:"), " Multinomial Logistic Regression is designed for problems with more than two classes.",style="color:black"),
                            tags$li(tags$strong("Interpretability:"), " Coefficients are interpretable, representing log-odds compared to the reference category.",style="color:black"),
                            tags$li(tags$strong("Probabilistic Output:"), " Can provides probabilities for each category, aiding decision-making.",style="color:black"),
                            tags$li(tags$strong("No Assumption of Linearity:"), " Doesn't assume a linear relationship between variables, allowing flexibility in characterizing representations between predictors and response.",style="color:black"),
                            tags$li(tags$strong("Widely Used in Classification:"), " Popular for multi-class classification tasks.",style="color:black")
                          )
                   ),
                   column(6,
                          tags$h4("Cons:",style="color:black"),
                          tags$ul(
                            tags$li(tags$strong("Assumption of Independence of Irrelevant Alternatives (IIA):"), " IIA assumption may not always hold in real-world scenarios. The assumption states that the odds of choosing one class over another do not depend on the presence of alternatives.",style="color:black"),
                            tags$li(tags$strong("Difficulty with Ordinal Response"), " This modeling technique struggles to effctively model relationships where the categorical response is ordinal (e.g., ranked).",style="color:black")
                          )
                   )
                 ),
  
                 h3("What is a Random Forest?"),
                 tags$p("A random forest, as its name implies, involves two things -- trees (lots of them)
                     and randomness. The idea behind a random forest model is to fit multiple trees, as in Bootstrap Aggregation (bagging) models, but 
                    considering a random subset of predictors in each tree fit.",style="color:black"),
                 tags$p(
                   "By convention, the cardinality of the random subset of predictors for each tree fit depends on the task at hand, and on ",
                   "specified hyperparameters. For a classification task, it is usually the square root of the number of predictors: ",
                   withMathJax(helpText("\\(m = \\sqrt{p}\\) with \\(p\\) the original number of predictors.", style = "color:black")),style="color:black"),
                 tags$p("For ", tags$strong("random forest trees"), ":",style="color:black"),
                 tags$ul(
                   tags$li("Create a bootstrap sample of the same size as the training data.",style="color:black"),
                   tags$li("Train a tree on the specified number of predictors using this bootstrapped sample.",style="color:black"),
                   tags$li("Do the above step for some number of trees, ", withMathJax("\\(B\\)"), ", usually 1000 or 500.",style="color:black"),
                   tags$li("Average the results of the above step to obtain regression predictions. If classification, use a majority-vote or some other scheme to determine final predictions.",style="color:black"),
                   tags$li("If hyperparameters specified (usually ", withMathJax("\\(\\text{mtry} := \\)"), "the number of predictors), compute accuracy or log-loss metric for each combination, and select the hyperparameters that maximize/minimize the desired value.",style="color:black")
                 ),
        fluidRow(
          column(6,
                 tags$h4("Pros:",style="color:black"),
                 tags$ul(
                   tags$li("Effective for both classification and regression tasks.",style="color:black"),
                   tags$li("Handles high-dimensional data well due to the ability to not get focused on one significant predictor and investigate multiple sub-relationships in data.",style="color:black"),
                   tags$li("Resistant to overfitting, thanks to ensemble averaging (in regression cases).",style="color:black"),
                   tags$li("Provides feature importances to determine which predictors were most significant in characterizing the response.",style="color:black"),
                   tags$li("Applicable to unbalanced datasets (e.g., this data!).",style="color:black")
                 )
          ),
          column(6,
                 tags$h4("Cons:",style="color:black"),
                 tags$ul(
                   tags$li("Can be computationally expensive for large datasets and many trees.",style="color:black"),
                   tags$li("May not provide easily interpretable models.",style="color:black"),
                   tags$li("Requires careful tuning of hyperparameters.",style="color:black"),
                   tags$li("Difficult to visualize for large forests.",style="color:black")
                 )
          )
        ),
                 style = "background-color: #FFFFFF !important"),
        
        ### Model Fitting Tab
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
        
        ### Prediction Tab
        tabPanel(title = "Prediction",
                 sidebarLayout(
                   sidebarPanel(
                     h5("Note: If this tab is blank, train models using the previous tab first!"),
                     div(id = "dynamicPredictors")
                   ),
                   mainPanel(
                     uiOutput("predResults")
                   )
                 ),
                 style = "margin-bottom: 100px; overflow-y:scroll; background-color: #FFFFFF !important")
      )
    ),
    footer = tags$footer(
      style = "text-align: center; background-color: #f4f4f4;
      position: fixed; bottom: 0; left: 0; width:100%",
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

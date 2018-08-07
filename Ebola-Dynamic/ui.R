library(shiny)
library(shinyjs)

hcu_choices <- c("none", "small", "medium", "large")

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Stop Ebola"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    tags$h3("Building strategy"),
    
    span("You are aiming to minimise the cumulative death toll after 10 months."),
    span("You can build:"),
    br(),
    span("• small HCU: 30 beds, 1 month"),
    br(),
    span("• medium HCU: 100 beds, 2 months"),
    br(),
    span("• large HCU: 400 beds, 4 months"),
    br(),
    br(),
    
    selectInput("build1", "month 1:", choices = hcu_choices), 
    selectInput("build2", "month 2:", choices = hcu_choices), 
    selectInput("build3", "month 3:", choices = hcu_choices), 
    selectInput("build4", "month 4:", choices = hcu_choices), 
    selectInput("build5", "month 5:", choices = hcu_choices), 
    selectInput("build6", "month 6:", choices = hcu_choices), 
    selectInput("build7", "month 7:", choices = hcu_choices), 
    selectInput("build8", "month 8:", choices = hcu_choices), 
    selectInput("build9", "month 9:", choices = hcu_choices), 
    
    br(),

    h5("Initial version created by:"),
    tags$a("Econometrics by Simulation", 
           href="http://www.econometricsbysimulation.com"),
    h5("For details on how data is generated"),
    tags$a("Blog Post", 
           href="http://www.econometricsbysimulation.com/2014/10/ebola-beds-labs-and-warnings-can-they.html"),
    h5("Adapted by:"),
    tags$a("Nick Golding", 
           href="https://github.com/goldingn/Shiny-Demos/tree/master/Ebola-Dynamic"),
    
    h5(textOutput("counter"))
    
    ),
  
  # Show a table summarizing the values entered
  mainPanel(
    
    tags$h3("Results"),
    
    fluidRow(
      column(12, align = "center",
             tableOutput('datatable')
      )
    ),
    # tableOutput("datatable"),

    fluidRow(
      column(12, align = "center",
             plotOutput("graph1", width = "95%")
      )
    ),
    fluidRow(
      column(12, align = "center",
             checkboxGroupInput("Indicators", "",
                                c("Beds",
                                  "Contageous", 
                                  "Dead"),
                                selected = c("Beds",
                                             "Contageous", 
                                             "Dead"),
                                inline=TRUE)
      )
    )

  )
))

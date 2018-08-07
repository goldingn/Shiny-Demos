library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Ebola Model"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    tags$h3("Data Generation"),
      
    sliderInput("bed0", "# of Quarantine Beds Available Initially:", 
                min=0, max=10^3, value=0, step=10), 

    sliderInput("bed1", "# of New Quarantine Beds Available at 1 months:", 
                min=0, max=10^3, value=10, step=10), 

    sliderInput("bed2", "# of New Quarantine Beds Available at 2 months:", 
                min=0, max=10^3, value=50, step=10), 
    
    sliderInput("bed3", "# of New Quarantine Beds Available at 3 months:", 
                min=0, max=10^3, value=50, step=10), 

    sliderInput("bed4", "# of New Quarantine Beds Available at 4 months:", 
                min=0, max=10^3, value=50, step=10), 

    sliderInput("bed5", "# of New Quarantine Beds Available at 5 months:", 
                min=0, max=10^3, value=50, step=10), 
    
    sliderInput("bed6", "# of New Quarantine Beds Available at 6 months:", 
                min=0, max=10^3, value=50, step=10), 
    
    sliderInput("bed7", "# of New Quarantine Beds Available at 7 months:", 
                min=0, max=10^3, value=50, step=10), 
    
    sliderInput("bed8", "# of New Quarantine Beds Available at 8 months:", 
                min=0, max=10^3, value=100, step=10), 
    
    sliderInput("bed9", "# of New Quarantine Beds Available at 9 months:", 
                min=0, max=10^3, value=100, step=10), 
    
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
    
    # h5(textOutput("counter")),
    
    tableOutput("datatable"),

    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"),
                  plotOutput("graph1"),
                  plotOutput("graph2"))
    ),
    
    checkboxGroupInput("Indicators", "",
                       c("Beds",
                         "Quarantined",
                         "Contageous", 
                         "Dead"),
                       selected = c("Beds",
                                    "Quarantined",
                                    "Contageous", 
                                    "Dead"),
                       inline=TRUE)

  )
))

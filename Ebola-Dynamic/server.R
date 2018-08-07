library(shiny)
library(ggplot2)
library(scales)

# options of building health care units
hcu_choices <- c("none", "small", "medium", "large")

# get the current build vector
get_build_vec <- function (input) {
  names <- paste0("build", 1:9)
  current_build_list <- lapply(names, function(name) input[[name]] )
  current_build_vec <- unlist(current_build_list)
  current_build_vec
}

# get the vector of bed availability
get_beds <- function (input) {
  
  # bed types & vector of number of new beds to fill in
  hcu_type <- get_build_vec(input)
  new_beds <- rep(0, 10)
  
  # loop through units, adding the beds *after they have been built*
  for (i in seq_along(hcu_type)) {
    
    if (hcu_type[i] == "small" & i <= 9) {
      new_beds[i + 1] <- 30
    }
    
    if (hcu_type[i] == "medium" & i <= 8) {
      new_beds[i + 2] <- 100
    }
    
    if (hcu_type[i] == "large" & i <= 6) {
      new_beds[i + 4] <- 400
    }
    
  }
  
  # return cumulative number of beds
  cumsum(new_beds)
  
}

# make sure the build vector is correct
assert_build_vec <- function (input, session) {
  # proposed building plan, and a corrected version
  current_build_vec <- get_build_vec(input)
  new_build_vec <- correct_build_vec(current_build_vec)
  # loop through mistakes, applying the corrections
  mistakes <- current_build_vec != new_build_vec
  for (i in which(mistakes)) {
    name <- paste0("build", i)
    label <- paste0("month ", i, ":")
    updateSelectInput(session, name, label,
                      choices = hcu_choices,
                      selected = "none")
  }
  
}

# get the next {number} of integers, from {from}, up to a maximum of {max}
get_next <- function (number = 1, from = 1, max = 10) {
  vec <- from + seq_len(number)
  vec <- vec[vec <= max]
  vec
}

# step through the build vector, deleting later builds if they conflict
correct_build_vec <- function (build_vec) {
  
  for (i in seq_along(build_vec)) {

    idx <- switch(build_vec[i],
           medium = get_next(1, i),
           large = get_next(3, i),
           c())
    
    build_vec[idx] <- "none"
    
  }
  
  build_vec
  
}


# Simulation and Shiny Application of Flue Season Dynamics
shinyServer(function(input, output, session) {
    
  # make sure the building suggestion is valid
  observe({assert_build_vec(input, session)})
  
  mydata <- reactive({
    # Model Parameters:
    
    IC <- 10 ^ 2   # Infected
    N  <- 10 ^ 7   # Total Population
    np <- 300  # Time periods
    
  # Infection Parameters:
    # Mortality rate Days
    Mr <- 0.6
    
    # Days till resolution
    Days <- 18
  
    # Resolution rate per day
    Dr <- 1/Days 
    
    # Transmition rate per day (for those contageous)
    P <- 0.081
        
    # Social adaption to disease rt=r0d/(1+S)^t
    
    M   <- 0.6
    DET <- 0.07
    K  <- 0.0003
    
    # get number of beds available
    bedsv <- get_beds(input)

    # # Gain in bumber of beds available
    # bedsv <- rep(0, 10)
    # for (i in 1:9) bedsv[i+1] <- input[[paste0('bed',i)]]+bedsv[i]
    
    # Model - Dynamic Change
    # Change in Susceptible
    DS  <- function(t) -P*Sr*S[t]*C[t]/(S[t]+C[t]+1)
    
    # Change in Contageous
    DC  <- function(t) P*Sr*S[t]*C[t]/(S[t]+C[t]+1) - 
              min(C[t]*DET, max(beds-Q[t]*(1-Dr),0)) - C[t]*Dr
    
    # Change in quarantined
    DQ <- function(t) 
      min(C[t]*DET, max(beds-Q[t]*(1-Dr),0)) -Q[t]*Dr 
                    
    # Change in deceased          
    DD <- function(t) (Q[t]+C[t])*Mr*Dr
    
    # Change in recovered
    DR <- function(t) (Q[t]+C[t])*(1-Mr)*Dr
    
    # Change in detection over time
    Et <- function(t) detI+(1-(1-detG)^t)*(detM-detI)
    
    S <- C <- Q <- D <- R <- E <- B <- r0 <- rep(NA, np+1)
      
    # Initial populations:
    S[1]  <- N-IC # Sesceptible population
    C[1]  <- IC   # Contageous
    Q[1]  <- 0    # Quarentined
    R[1]  <- 0    # Recovered
    D[1]  <- 0    # Deceased
    B[1]  <- 0
    r0[1] <- P*Days
    
    # Loop through periods
    for (t in 1:np) {
      # Detection rate of unifected per day 
      B[t+1] <- beds <- bedsv[ceiling(t/30)]
      Sr <- (1+K)^(-t)
      r0[t+1] <- P*Sr*Days*(S[t])/(S[t]+C[t])
      
      # Calculate the change values
      dS  <- DS(t) 
      dC  <- DC(t) 
      dQ  <- DQ(t)
      dR  <- DR(t)
      dD  <- DD(t)
      
      # Change the total populations
      S[t+1] <- S[t] + dS
      C[t+1] <- C[t] + dC
      Q[t+1] <- Q[t] + dQ
      R[t+1] <- R[t] + dR
      D[t+1] <- D[t] + dD
      
    }
    
    # Turn the results into a table
    long <- data.frame(
      Period=rep((0:np), 3), 
      Population = c(B, C, D), 
      Indicator=rep(c("Beds",
                      "Contageous", 
                      "Dead"), 
                    each=np+1))
    wide <- cbind(r0, B, C, D)
    
    # make some things integers
    wide <- as.data.frame(wide)
    wide$B <- as.integer(wide$B)
    
    # better names
    colnames(wide) <- c("R0",
                        "Beds",
                        "Contageous",
                        "Dead")
    
    
    list(long=long, wide=wide)
    
    })
  
  output$r0 <- 
    renderText(paste('Initial r0:', input$P*input$Days))
  output$LDET <- 
    renderText(paste('Likelihood of detection:', round(1-(1-input$DET)^input$Days,2)))
  
  output$datatable <- 
    renderTable({
      Tdata <- mydata()[["wide"]]
      Tdata <- Tdata[seq(1, nrow(Tdata), by = 30), ]
      Tdata <- Tdata[-1, ]
      Tdata <- cbind(month = 1:nrow(Tdata), Tdata)
    })
    
  output$graph1 <- renderPlot({
    long <- mydata()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
         aes(x=Period, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Population Totals")+
      scale_x_continuous(name="Days")+ 
      scale_y_continuous(labels = comma, name="")
    print(p)
  })
  
})

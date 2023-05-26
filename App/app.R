##########################################
# End Of Outbreak Probability App        # 
# Naomi Bradbury nvm4@leicester.ac.uk    #
##########################################

library(ggplot2)
library(shiny)
library(shinycssloaders)
library(shinyvalidate)

EbolaData <- read.csv("Likiti_outbreak.csv") # Load Ebola data
EbolaSerialInterval <- read.csv("Ebola_serial_interval.csv") # Load Ebola serial interval
NipahData <- read.csv("Nipah_outbreak.csv") # Load Nipah data
NipahSerialInterval <- read.csv("Nipah_serial_interval.csv") # Load Nipah serial interval 

ui <- navbarPage("End-of-Outbreak Probability",
                 

  tabPanel("Home",
           
      h3("End-of-Outbreak Probability"),
      
      br(),
      
      fluidRow(
          
          column (3, br(),
      
              img(src='virus.jpg', align = "left", height = 250, width = 320),
      
              p("Photo by",
                tags$a(href='https://unsplash.com/@cdc?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText', "CDC"),
                "on",  
                tags$a(href='https://unsplash.com/', "Unsplash"), 
                target="_blank")
      
          ),
          
          column (2),
              
          column (5, br(),
                  
              h4("Calculate the end-of-outbreak probability for outbreaks with a known transmission tree."),
              p("For guidance on using this app, please consult the",
                tags$a(href='https://github.com/nabury/End_of_outbreak_app/wiki/User-Guide',"User Guide"))
          ),
      ),
      
      hr(),
      p("Naomi Bradbury, William Hart, Francesca Lovell-Read, Jonathan Polonsky & Robin Thompson"),
      p("This app is still in active development. For feedback or questions about the app, please contact Naomi Bradbury at nvm4@leicester.ac.uk"),
      p("If you use this app, please cite it as: [insert later]"),
      p("Code available on", tags$a(href='https://github.com/nabury/End_of_outbreak_app', "Github"), target="_blank"),
  ),
               
  tabPanel("Load data",
           
      sidebarLayout(
          
          sidebarPanel(
            
              # Case study selector
              selectInput("case_study", label = ("Select case study"), 
                        choices = list("Ebola Likiti" = 1, "Nipah Bangladesh" = 2), 
                        selected = 1),
              
              h4("OR"),
      
              # Import an outbreak data csv file
              fileInput("outbreak_csv", "Select outbreak data file (.csv format) to upload",
                        multiple = TRUE
                       ,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              # Import a serial interval csv file
              fileInput("serial_interval_csv", "Select serial interval file (.csv format) to upload",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              actionButton("reset", "Reset to pre-loaded data"), # Button to reload page
              
              hr(),

              p("Please consult the", 
                tags$a(href='https://github.com/nabury/End_of_outbreak_app/wiki/User-Guide', "User Guide"),
                "for instructions on how to upload data files")
          ),
          
          mainPanel(
              
              HTML("<b>Outbreak data to be used in end-of-outbreak probability calculations</b>"),
      
              tableOutput('outbreak_tbl'), # Display outbreak data in a table
              
              hr(),
              
              HTML("<b>Serial interval distribution to be used in end-of-outbreak probability calculations</b>"),
              
              plotOutput('serial_interval_plot'), # Display the serial interval as a histogram
          ),
      ),
  ),
  
  tabPanel("End-of-outbreak probability",
           
       sidebarLayout(
           
           sidebarPanel(
               
               # Button to conduct end of outbreak probability calculations
               HTML("<b>Calculate end-of-outbreak probabilities using the selected inputs: </b>"),
               actionButton("go", "Go"),
               
               hr(),

               HTML("<b>Inputs for negative binomial offspring distribution</b> - choose appropriate values for user uploaded outbreaks"),

               p(),
               
               uiOutput("R"),  # Reactive user input for R 
               uiOutput("k"), # Reactive user input for k
               uiOutput("future_days") # Reactive future days slider
           ),
           
           mainPanel(
               
               HTML("<b>Outbreak cases and end-of-outbreak probabilities</b>"),
               
               p(),

               withSpinner(plotOutput("plot")), # Displays plot

               HTML("Daily reported cases are represented by the green bars and are scaled to the left-hand y-axis.
                    Daily end-of-outbreak probabilities are displayed in the line plot and scaled to the right-hand y-axis."),
           ),
       ),
  ),
  
  tabPanel("Table of results",
  
      sidebarLayout(
          
          sidebarPanel(
              downloadButton("downloadData", "Download results as .csv") # Button to download results
          ),
          
          mainPanel(
              tableOutput('results_tbl') # Display results in a table
          ),
      ),
  ),
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    session$reload()
  })
    
  # Reactively change outbreak data between case study and uploaded data
  outbreak_data <- reactive({

      if (is.null(input$outbreak_csv)) {
        if(input$case_study == 1) {return(EbolaData)} 
        if(input$case_study == 2) {return(NipahData)}
      }
    
      # Uploaded data
      else {
          df <- read.csv(input$outbreak_csv$datapath)
          return(df)
      }
  })
  
  # Reactively change serial interval between case study and uploaded file
  serial_interval <- reactive({
      
      if (is.null(input$serial_interval_csv)) {
        
        # Ebola serial interval
        if(input$case_study == 1) {return(as.vector(EbolaSerialInterval$Probability))}
        
        # Nipah serial interval
        if(input$case_study == 2) {return(as.vector(NipahSerialInterval$Probability))}
      }
      
      # Uploaded serial interval
      else {
          df2 <- read.csv(input$serial_interval_csv$datapath)
          df2a <- as.vector(df2$Serial_interval)
          df2b <- as.vector(df2$Probability)
          
          # Check for validity for user inputted serial interval
          validate(
              need(all(df2a==0:(length(df2a)-1)), "Probabilities must be provided for all serial interval values from 0 to the maximum possible value"),
              need(sum(df2b) <= 1+1e-5, "Serial interval too large"),
              need(sum(df2b) >= 0.99, "Serial interval too small"),
              need(!any(df2b<0), "Negative value in serial interval")
          )
          return(df2b/sum(df2b))
      }
  })
  
  # Create table of the outbreak data
  output$outbreak_tbl <- renderTable({outbreak_data()})
  
  # Create histogram of the serial interval
  output$serial_interval_plot <- renderPlot({
    
    prob <- serial_interval()
    time <- seq(0, length(prob)-1, by = 1)
    
    data <- data.frame(time, prob)
    
    ggplot(data = data, aes(x = time, y = prob)) +
      geom_bar(stat = "identity", fill = "#493756") +
      scale_x_continuous("Serial interval (days)") +
      ylab ("Probability") +
      theme_minimal(base_size = 14) 
  })

  # Check validity of numeric inputs for R and k
  iv <- InputValidator$new()
  iv$add_rule("R", compose_rules(sv_gt(0), sv_lte(100))) # 0 < R <= 100
  iv$add_rule("k", sv_gt(0)) # k > 0
  iv$enable()
  
  # Reactive user input for R
  output$R <- renderUI({
    if(input$case_study == 1) {r <- 2.1} # R for Ebola
    if(input$case_study == 2) {r <- 0.48} # R for Nipah
    numericInput("R",
                 label = "Reproduction number (R)",
                 value = r,
                 step = 0.05)
  })
  
  # Reactive user input for k
  output$k <- renderUI({
    if(input$case_study == 1) {disp <- 0.18} # R for Ebola
    if(input$case_study == 2) {disp <- 0.06} # R for Nipah
    numericInput("k",
                 label = "Dispersion parameter (k)",
                 value = disp,
                 step = 0.005)
  })

  # Slider for days to be plotted after last reported case - reactive so maximumum length = serial interval length
  output$future_days <- renderUI({
      w <- serial_interval()
      sliderInput("future_days", h5("Days considered after last known case"),
                  min = 1, max = length(w), value = length(w)/2, step = 1)
  })
  
  # Calculate end of outbreak probabilities
  results <- eventReactive (input$go, {
      
    # Check input validity
    req(iv$is_valid()) 
    
    # Labels for reactive plot
    
    if(!is.null(input$outbreak_csv)) {
      Data_lab <- "User uploaded"
    } else {
      if (input$case_study == 1) {
        Data_lab <- "Ebola Likiti"
      } else if (input$case_study == 2) {
        Data_lab <- "Nipah Bangladesh"
      }
    }
  
    R_lab <- input$R
    k_lab <- input$k
    
    # Get input data
    outbreak_data <- outbreak_data()
    k_offspring <- input$k
    R_offspring <- input$R
    p0_offspring <- R_offspring/(R_offspring+k_offspring)
    w <- serial_interval()
    
    end_t <- max(outbreak_data$Onset_day) + input$future_days # Total days from first case to end of calculation
    
    # Lengthen serial interval if necessary
    if (length(w) < end_t + 1) {w <- c(w, rep(0, end_t + 1 - length(w)))} 
    
    # Cumulative serial interval distribution
    
    F <- cumsum(w)
    
    # Create empty vector to store end of outbreak probabilities
    p_outbreak_over <- rep(NA, end_t) 
    
    # For each day into the future
    for (t in 1:end_t) {
        
        current_cases <- (outbreak_data$Onset_day <= t) # Select only the cases that have already occured
        Onset_day_current <- outbreak_data$Onset_day[current_cases]
        Infector_ID_current <- outbreak_data$Infector_ID[current_cases]

        A <- tabulate(Infector_ID_current, nbins = length(Infector_ID_current)) # Number of infections already caused by each individual
        TR <- t - Onset_day_current # Difference between current t and reporting date

        p <- prod((1-p0_offspring*(1-F[TR+1]))^(k_offspring+A))
        
        p_outbreak_over[t] <- p # Record probability for day t in vector
    }
    
    # Create data frame of times and outbreak probabilities
    times <- c(1:end_t)
    results <- data.frame(times, p_outbreak_over)
    resultsList <- list("results" = results, "Data_lab" = Data_lab, "R_lab" = R_lab, "k_lab" = k_lab, "outbreak_data" = outbreak_data)
    return(resultsList)
  })
  
  # Create interactive plot of probability outbreak is over
  output$plot <- renderPlot({
    
      inputs <- results()
      results <- inputs$results
      Data_lab <- inputs$Data_lab
      R_lab <- inputs$R_lab
      k_lab <- inputs$k_lab
      outbreak_data <- inputs$outbreak_data

      # Set y axes limits
      ylim.prim <- c(0, max(table(outbreak_data$Onset_day)))
      ylim.sec <- c(0,1)

      # Make calculations based on these limits
      ylim.b <- diff(ylim.prim)/diff(ylim.sec)
      ylim.a <- ylim.prim[1] - ylim.b*ylim.sec[1]

      sub_plot <- ggplot() +
          geom_histogram(data = outbreak_data, aes(Onset_day), fill = "#1b9621", colour = "black", binwidth = 1) +
          ggtitle(paste("Outbreak:", Data_lab, " R =", R_lab, " k =", k_lab)) +
          xlab("Outbreak duration (days)") +
          scale_y_continuous("Cases", sec.axis = sec_axis(~ (. - ylim.a)/ylim.b, name = "End-of-outbreak probability")) +
          theme(
            axis.title.y.left = element_text(colour = "#1b9621"),
            axis.text.y.left = element_text(colour = "#1b9621")
          ) 
      
      plot <- sub_plot +
          geom_line(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) +
          geom_point(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) 
      
      return(plot)
  })
  
  # Table of end of outbreak probability results
  output$results_tbl <- renderTable({
      results_tbl <- results()$results %>%
      rename(Outbreak_day = times, End_of_outbreak_probability = p_outbreak_over)
  })
  
  # Allow end of outbreak probability results to be downloaded as a csv file
  output$downloadData <- downloadHandler(
      filename = "End_of_outbreak.csv",
      content = function(file) {
          write.csv(results()$results %>%
                        rename(Outbreak_day = times, End_of_outbreak_probability = p_outbreak_over),
                    file)
      }
  )   
}   

shinyApp(ui = ui, server = server)

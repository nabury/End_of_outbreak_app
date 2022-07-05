###################################
# End Of Outbreak Probability App # 
# n.marsden.1@warwick.ac.uk       #
###################################

library(ggplot2)
library(plotly)
library(shiny)
library(shinycssloaders)
library(shinyvalidate)

defaultData <- read.csv("Likiti_outbreak.csv") # Load default data (Ebola Likiti)

ui <- navbarPage("End of Outbreak Probability",
    
    tabPanel("Home",
             
        h3("End of Outbreak Probability v1"),
        
        br(),
        
        fluidRow(
            
            column (3, br(),
        
                img(src='virus.jpg', align = "left", height = 250, width = 320),
        
                HTML("Photo by <a href='https://unsplash.com/@cdc?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText'>CDC</a> on <a href='https://unsplash.com/s/photos/virus-corona?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText'>Unsplash</a>")
        
            ),
            
            column (2),
                
            column (5, br(),
                    
                h4("Calculate the end of outbreak probability for outbreaks with a known transmission tree."),
                p("Powered by R Shiny")
            ),
        ),
        
        hr(),
        p("Naomi Bradbury, Francesca Lovell-Read & Robin Thompson"),
        p("This app is still in active development. For feedback or questions about the app, please contact Naomi Bradbury n.marsden.1@warwick.ac.uk"),
        p("If you use this app, please cite it as: [insert later]"),
        HTML("Code and additional files are available on <a href='https://github.com/nabury/End_of_outbreak_app'>Github</a>"),
    ),
                 
    tabPanel("Load data",
             
        sidebarLayout(
            
            sidebarPanel(
        
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
                
                actionButton("button", "Reset to pre-loaded data"),
                
                hr(),

                p("See documentation tab for guidance on uploading data")
            ),
            
            mainPanel(
                
                HTML("<b>Outbreak data</b>"),
        
                tableOutput('outbreak_tbl'), # Display outbreak data in a table
                
                hr(),
                
                HTML("<b>Serial interval distribution</b>"),
                
                plotOutput('serial_interval_plot'), # Display the serial interval as a histogram
            ),
        ),
    ),
    
    tabPanel("End of Outbreak Probability",
             
         sidebarLayout(
             
             sidebarPanel(
                 
                 # Button to conduct end of outbreak probability calculations
                 HTML("<b>Calculate outbreak probabilities using the selected inputs: </b>"),
                 actionButton("go", "Go"),
                 
                 hr(),

                 HTML("<b>Inputs for offspring distribution</b>"),
                 
                 p(),
                 
                 # User input for R 
                 numericInput("R", 
                              label = "Reproduction number (R)", 
                              value = 1.8,
                              step = 0.05), 
                 
                 # User input for k 
                 numericInput("k", 
                              label = "Dispersion parameter (k)", 
                              value = 0.18,
                              step = 0.005), 
                 
                 uiOutput("future_days") # Reactive future days slider
             ),
             
             mainPanel(
                 
                 HTML("<b>Outbreak cases and end of outbreak probabilities</b>"),
                 
                 p(),

                 withSpinner(plotOutput("plot")), # Displays plot
                 # withSpinner(plotlyOutput("plot")), # Displays plot,
                 
                 HTML("Daily cases are shown using bars and the probability the outbreak is over is displayed as a line plot "),
             ),
         ),
    ),
    
    tabPanel("Table of results",
    
        sidebarLayout(
            
            sidebarPanel(
                downloadButton("downloadData", "Download results as .csv") 
            ),
            
            mainPanel(
                tableOutput('results_tbl') # Display results in a table
            ),
        ),
    ),
    
    tabPanel("Documentation",
             
        HTML("<b>Outbreak data file</b>"),
        
        p("A csv file containing outbreak data can be uploaded to the app.
          The file must consist of three columns with the headings named exactly: ID, Onset_day, Infector_ID.
          All columns must contain only numeric, integer data."),
        
        HTML("<ul>
            <li>Column 1: ID must be numbered in integer order i.e. 1,2,3. </li>
            <li>Column 2: Onset_day contains the outbreak day when the case began i.e. the number of days after the first reported case.</li>
            <li>Infector_ID: Refers to the ID of the individual who infected this case. If this is unknown, it should be set to zero.</li>
            </ul>"),
        
        HTML("<b>Serial interval file</b>"),
        
        p("A serial interval can also be uploaded. This must contain only one column of data with the heading: Serial_interval.
          This column should sum to 1. Columns with a sum greater than 1, less than 0.99 or containing negative values will show an error message."),
        
        HTML("<b>Last updated 5th July 2022</b>")
    )
)

server <- function(input, output, session) {
    
    observeEvent(input$button, {
        session$reload()
    })
    
    # Reactively change outbreak data between dafault and uploaded data
    outbreak_data <- reactive({
        
        eventReactive(input$button, {
            input$outbreak_csv <- NULL
        })

        if (is.null(input$outbreak_csv)) {return(defaultData)}

        else {
            df <- read.csv(input$outbreak_csv$datapath)
            return(df)
        }
    })
    
    # Reactively change serial interval between default and uploaded file
    serial_interval <- reactive({
        
        if (is.null(input$serial_interval_csv)) {
        
            mean <- 15.3; sd <- 9.3
            alpha <- (mean/sd)^2
            beta <- sd^2/mean
            x <- c(1:100) 
            w <- dgamma(x, shape = alpha, scale = beta)
            return(w)
        }
        
        else {
            df2 <- read.csv(input$serial_interval_csv$datapath)
            df2 <- as.vector(df2$Serial_interval)
            
            # Check for validity for user inputted serial interval
            validate(
                need(sum(df2) <= 1, "Serial interval too large"),
                need(sum(df2) >= 0.99, "Serial interval too small"),
                need(!any(df2<0), "Negative value in serial interval")
            )
            return(df2)
        }
    })
    
    # Create table of the outbreak data
    output$outbreak_tbl <- renderTable({outbreak_data()})
    
    # Create histogram of the serial interval
    output$serial_interval_plot <- renderPlot({
        barplot(serial_interval(), xlab = "Serial interval duration", ylab = "Probability")
    })

    # Check validity of numeric inputs for R and k
    iv <- InputValidator$new()
    iv$add_rule("R", compose_rules(sv_gt(0), sv_lte(100))) # 0 < R <= 100
    iv$add_rule("k", sv_gt(0)) # k > 0
    iv$enable()
    
    # Days to be plotted after last reported case
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
        R_lab <- input$R
        k_lab <- input$k
        
        # Get input data
        outbreak_data <- outbreak_data()
        w <- serial_interval()
        
        end_t <- max(outbreak_data$Onset_day) + input$future_days # Total days from first case to end of calculation
        
        # Lengthen serial interval if necessary
        if (length(w) < end_t) {w <- c(w, rep(0, end_t - length(w)))} 

        # Create empty vector to store end of outbreak probabilities
        p_outbreak_over <- rep(NA, end_t) 
        
        # For each day into the future
        for (t in 1:end_t) {
            
            current_cases <- sum(outbreak_data$Onset_day <= t) # Select only the cases that have already occured
            p <- 1
            
            # For each infected individual
            for (i in 1:current_cases) {
                
                A <- sum(outbreak_data$Infector_ID == i & outbreak_data$Onset_day <= t) # Number of infections already caused by individual i
                TR <- t - outbreak_data$Onset_day[i] # Difference between current t and reporting date
                div <- 0
                
                # Calculate divisor sum
                if (TR == 0) {
                    for (j in A:100) {
                        div <- div + dnbinom(j, size = input$k, mu = input$R) * (factorial(j)/factorial(A)) * (1^(j-A) / factorial(j-A))
                    }
                } else {
                    for (j in A:100) {
                        div <- div + dnbinom(j, size = input$k, mu = input$R) * (factorial(j)/factorial(A)) * ((1-(sum(w[1:TR])))^(j-A) / factorial(j-A))
                    }
                }
                
                # Calculate individual i's probability and multiply by previous
                p <- p * (dnbinom(A, size = input$k, mu = input$R) / div)
            }
            
            p_outbreak_over[t] <- p # Record probability for day t in vector
        }
        
        # Create data frame of times and outbreak probabilities
        times <- c(1:end_t)
        results <- data.frame(times, p_outbreak_over)
        resultsList <- list("results" = results, "R_lab" = R_lab, "k_lab" = k_lab)
        return(resultsList)
    })
    
    # Create interactive plot of probability outbreak is over
    # output$plot <- renderPlotly({
    output$plot <- renderPlot({
        
        # plot <- ggplot(results, aes(x = times,
        #                             y = p_outbreak_over,
        #                             group = 1,
        #                             text = paste("Probability: ",signif(p_outbreak_over, digits = 3),
        #                                          "<br>Time: ",times))) +
        #     geom_line()+
        #     geom_point() +
        #     geom_histogram(data = outbreak_data, aes(x = Onset_day, y = count))
        #     xlab("Time since end of case data (days)") +
        #     ylab("Probability outbreak over") +
        #     ylim(c(0:1))
        #
        # plot <- ggplotly(plot, tooltip = c("text"))
        
        inputs <- results()
        results <- inputs$results
        R_lab <- inputs$R_lab
        k_lab <- inputs$k_lab
        outbreak_data <- outbreak_data()
        
        # Set y axes limits
        ylim.prim <- c(0, max(table(outbreak_data$Onset_day)))
        ylim.sec <- c(0,1)

        # Make calculations based on these limits
        ylim.b <- diff(ylim.prim)/diff(ylim.sec)
        ylim.a <- ylim.prim[1] - ylim.b*ylim.sec[1]

        sub_plot <- ggplot() +
            geom_histogram(data = outbreak_data, aes(Onset_day), fill = "#1b9621", colour = "black", binwidth = 1) +
            ggtitle(paste("R =", R_lab, " k =", k_lab)) +
            xlab("Outbreak duration (days)") +
            scale_y_continuous("Cases", sec.axis = sec_axis(~ (. - ylim.a)/ylim.b, name = "Probability outbreak over"))
        
        plot <- sub_plot +
            geom_line(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) +
            geom_point(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) 
        
        # plot <- ggplotly(plot, tooltip = c("text"))

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

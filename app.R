###################################
# End Of Outbreak Probability App # 
# n.marsden.1@warwick.ac.uk       #
###################################

library(ggplot2)
library(plotly)
library(shiny)
library(shinycssloaders)
library(shinyvalidate)

# Load default data
defaultData <- read.csv("Likiti_outbreak.csv")

ui <- navbarPage("End of Outbreak Probability",
                 
    tabPanel("Load data",
             
        sidebarLayout(
            
            sidebarPanel(
        
                # Import the outbreak data as a csv file
                fileInput("outbreak_csv", "Select outbreak data file (.csv format) to upload",
                          multiple = TRUE
                         ,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Import the serial interval as a csv file
                fileInput("serial_interval_csv", "Select serial interval file (.csv format) to upload",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            
            mainPanel(
        
                # Display outbreak data in a table
                tableOutput('outbreak_tbl'),
                
                # Display the serial interval in a table
                tableOutput('serial_interval_tbl')
                
            ),
        ),
    ),
    
    tabPanel("End of Outbreak Probability",
             
         sidebarLayout(
             
             sidebarPanel(

                 p("Inputs for offspring distribution"),
                 
                 numericInput("R", 
                              h3("R"), 
                              value = 2.1,
                              step = 0.05), 
                 
                 numericInput("k", 
                              h3("k"), 
                              value = 0.18,
                              step = 0.005), 
                 
                 sliderInput("future_days", h3("Future days"),
                             min = 1, max = 50, value = 25),
             ),
             
             mainPanel(
                 
                 p("Outbreak cases and end of outbreak probabilities"),
                 p("Daily cases are shown using bars and the probability the outbreak is over is displayed as a line plot "),
                 
                 withSpinner(plotOutput("plot")), # Displays plot
                 # withSpinner(plotlyOutput("plot")), # Displays plot,
                 
             ),
         ),
    ),
    
    tabPanel("Table of results",
    
        sidebarLayout(
            
            sidebarPanel(
                downloadButton("downloadData", "Download results as .csv")
            ),
            
            mainPanel(
                tableOutput('results_tbl')
            ),
        ),
    ),
    
    tabPanel("Info",
        
        p("Last updated 21st February 2022"),
        
        p("Change log"),
        
        HTML("<ul>
            <li>Ability to upload serial interval - 31/01/22</li>
            <li>Plot combines cases and end of outbreak probabilities - 14/02/22</li>
            <li>Display results in a table and enable download as csv file - 21/02/22 </li>
            <li>Input validation for R and k - 21/02/22 </li>
            <li>Bug fixes with uploaded serial intervals - 21/02/22</li>
            </ul>"),
        
        p("Future updates"),
        
        HTML("<ul>
            <li>Interactive combined plot</li>
            <li>Documentation including guidance on uploading csv files</li>
            <li>Button for when inputs are updated</li>
            <li>Ability to reset preloaded data</li>
            </ul>"),
    )
)

server <- function(input, output) {
    
    # Reactively change outbreak data between default and uploaded file
    outbreak_data <- reactive({
        
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
            x <- c(1:100) # May need to change this to make it reactive
            w <- dgamma(x, shape = alpha, scale = beta)
            return(w)
        }
        
        else {
            df2 <- read.csv(input$serial_interval_csv$datapath)
            df2 <- as.vector(df2$Serial_interval)
            return(df2)
        }
    })
    
    serial_interval_df <- reactive ({
        serial_interval_df <- as.data.frame(serial_interval())
        colnames(serial_interval_df) <- ("Serial_interval")
        return(serial_interval_df)
    })
    
    # Create table of the outbreak data
    output$outbreak_tbl <- renderTable({outbreak_data()})
    
    # Create table of the serial interval
    output$serial_interval_tbl <- renderTable({serial_interval_df()}, digits = 3)
    
    # Check validity of numeric inputs for R and k
    iv <- InputValidator$new()
    iv$add_rule("R", compose_rules(sv_gt(0), sv_lte(100)))
    iv$add_rule("k", sv_gt(0))
    iv$enable()
    
    # Process end of outbreak probabilities
    results <- reactive ({
        req(iv$is_valid())
        
        outbreak_data <- outbreak_data()
        w <- serial_interval()
        
        starting_t <- max(outbreak_data$Onset_day) # End of case data, start of end of outbreak probability calculation
        max_t <- starting_t + input$future_days # Total days from first case to end of calculation
        
        # Lengthen serial interval
        if (length(w) < max_t) {w <- c(w, rep(0, max_t - length(w)))}
            
        p_outbreak_over <- rep(NA, (max_t)) # Empty vector to hold probabilities
        
        # For each day into the future
        for (t in (starting_t+1):(max_t)) {
            
            current_t <- t
            p <- 1
            
            # For each infected individual
            for (i in 1:nrow(outbreak_data)) {
                
                A <- sum(outbreak_data$Infector_ID == i) # Number of infections caused by individual i
                TR <- current_t - outbreak_data$Onset_day[i] - 1 # Time remaining in outbreak (time between onset date and last onset date)
                
                div <- 0
                
                # Calculate divisor sum
                for (j in A:100) {
                    div <- div + dnbinom(j, size = input$k, mu = input$R) * (factorial(j)/factorial(A)) * ((1-(sum(w[1:TR])))^(j-A) / factorial(j-A))
                }
                
                # Calculate individual i's probability and multiply by previous
                p <- p * (dnbinom(A, size = input$k, mu = input$R) / div)
            }
            
            p_outbreak_over[t] <- p # Record probability for day t in vector
        }
        
        times <- c(1:(starting_t+input$future_days))
        results <- data.frame(times, p_outbreak_over)
        return(results)
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
        
        results <- results()
        outbreak_data <- outbreak_data()
        
        # Set y axes limits
        ylim.prim <- c(0, max(table(outbreak_data$Onset_day)))
        ylim.sec <- c(0, 1)

        # Make calculations based on these limits
        ylim.b <- diff(ylim.prim)/diff(ylim.sec)
        ylim.a <- ylim.prim[1] - ylim.b*ylim.sec[1]

        plot <- ggplot() +
            geom_line(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b))+
            geom_point(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) +
            geom_histogram(data = outbreak_data, aes(Onset_day), fill = "#1b9621", colour = "black", binwidth = 1) +
            xlab("Outbreak duration (days)") +
            scale_y_continuous("Cases", sec.axis = sec_axis(~ (. - ylim.a)/ylim.b, name = "Probability outbreak over"))

        # plot <- ggplotly(plot, tooltip = c("text"))

        return(plot)
    })
    
    output$results_tbl <- renderTable({
        results()%>%
        rename(Outbreak_day = times, End_of_outbreak_probability = p_outbreak_over)
    })
    
    output$downloadData <- downloadHandler(
        filename = "End_of_outbreak.csv",
        content = function(file) {
            write.csv(results() %>%
                          rename(Outbreak_day = times, End_of_outbreak_probability = p_outbreak_over), 
                      file)
        }
    )
    
}

shinyApp(ui = ui, server = server)

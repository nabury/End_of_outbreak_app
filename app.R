###################################
# End Of Outbreak Probability App # 
# n.marsden.1@warwick.ac.uk       #
###################################

library(ggplot2)
library(plotly)
library(shiny)
library(shinycssloaders)

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
    
    tabPanel("Results",
             
         sidebarLayout(
             
             sidebarPanel(

                 p("Inputs for offspring distribution"),
                 
                 numericInput("R", 
                              h3("R"), 
                              value = 2.1), 
                 
                 numericInput("k", 
                              h3("k"), 
                              value = 0.18), 
                 
                 sliderInput("future_days", h3("Future days"),
                             min = 1, max = 50, value = 25),
             ),
             
             mainPanel(
                 
                 p("Probability the outbreak is over"),
                 
                 withSpinner(plotlyOutput("plot")), # Displays plot

             ),
         ),
    ),
    
    tabPanel("Info",
        
        # Panel for notes and updates
        p("Last updated 31st January 2022"),
        
        p("Change log"),
        
        HTML("<ul>
            <li>Ability to upload serial interval - 31/01/22</li>
            <li>Interactive plot (hover to see values) - 31/01/22</li>
            </ul>"),
        
        p("Future updates"),
        
        HTML("<ul>
            <li>Improvements to visualisation of plot and tables</li>
            <li>Download button for results</li>
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
            return(df2)
        }
    })
    
    # Create table of the outbreak data
    output$outbreak_tbl <- renderTable({outbreak_data()})
    
    output$serial_interval_tbl <- renderTable({serial_interval()})
    
    # Create interactive plot of probability outbreak is over
    output$plot <- renderPlotly({
        
        outbreak_data <- outbreak_data()
        w <- serial_interval()
        
        starting_t <- max(outbreak_data$onset)
        
        p_outbreak_over <- rep(NA, (starting_t+input$future_days)) # Empty vector to hold probabilities

        # For each day into the future
        for (t in (starting_t+1):(starting_t+input$future_days)) {

            # current_t <- max(outbreak_data$onset + t)
            current_t <- t
            p <- 1

            # For each infected individual
            for (i in 1:nrow(outbreak_data)) {

                A <- sum(outbreak_data$infector == i) # Number of infections caused by individual i
                TR <- current_t - outbreak_data$onset[i] - 1 # Time remaining in outbreak (time between onset date and last onset date)

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
        
        # plot <- ggplot(results, aes(x = times,
        #                             y = p_outbreak_over,
        #                             group = 1,
        #                             text = paste("Probability: ",signif(p_outbreak_over, digits = 3),
        #                                          "<br>Time: ",times))) +
        #     geom_line()+
        #     geom_point() +
        #     geom_histogram(data = outbreak_data, aes(x = onset, y = count))
        #     xlab("Time since end of case data (days)") +
        #     ylab("Probability outbreak over") +
        #     ylim(c(0:1))
        # 
        # plot <- ggplotly(plot, tooltip = c("text"))
        
        # Set y axes limits
        ylim.prim <- c(0, max(table(outbreak_data$onset)))   
        ylim.sec <- c(0, 1)
        
        # Make calculations based on these limits
        ylim.b <- diff(ylim.prim)/diff(ylim.sec)
        ylim.a <- ylim.prim[1] - ylim.b*ylim.sec[1] 
        
        plot <- ggplot() +
            geom_line(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b))+
            geom_point(data = results, aes(x = times, y = ylim.a + p_outbreak_over * ylim.b)) +
            geom_histogram(data = outbreak_data, aes(onset), fill = "#1b9621", colour = "black", binwidth = 1) +
            xlab("Time since end of case data (days)") +
            scale_y_continuous("Cases", sec.axis = sec_axis(~ (. - ylim.a)/ylim.b, name = "Probability outbreak over"))
        

        return(plot)
    })
}

shinyApp(ui = ui, server = server)

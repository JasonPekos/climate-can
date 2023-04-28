#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Climate Data by station"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #input station names
          selectInput(inputId = "Station", 
                      # for internal use
                      label = "Choose a station", 
                      # tells the user what to do
                      choices = unique(clm$Station.Name)
                      ),
          
          #Input variables of climate data
          selectInput(inputId = "Feature", 
                      # for internal use
                      label = "Choose what to plot", 
                      # tells the user what to do
                      choices = c("Mean.Max.Temp...C.", "Mean.Min.Temp...C.", 
                                            "Mean.Temp...C.", "Extr.Max.Temp...C.", "Extr.Min.Temp...C.",
                                            "Total.Rain..mm." , "Total.Snow..cm." , "Total.Precip..mm.")
                    )
                    
                ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput(outputId = "temp")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$temp <- renderPlot({
      
      #Import Climate data
      clm = read.csv("~/Documents/Projects/climate-can/Data/climate_data/weather_Station_data.csv")
      
      #Tidy data
      clm = clm %>% mutate(Date.Time = as.yearmon(Date.Time, format = "%Y-%m")) %>%
        mutate(year = year(Date.Time)) %>%
        mutate(Province = NA) %>% # Add these for later use
        mutate(Region.Name = NA)
      
      #Plot
      dat_trunc <- clm %>%
        # filter(Date.Time < "Jan 1999") %>%
        filter(Station.Name == input$Station)
    
      
      ggplot(data = dat_trunc, aes(x = Month,
                                   y = !!sym(input$Feature),
                                   color = Year,
                                   group = Year)) +
        geom_point() +
        geom_line() + 
        theme_minimal() +
        scale_color_viridis(option = "inferno") +
        ggtitle("Change in the selected feature over the years")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

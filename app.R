library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Starwars"),h1("Star Wars Characters"),

    # Sidebar with a slider input for number of taille 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId="taille",
                        label="Height of characters:",
                        min = 0,
                        max = 250,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId="StarWarsPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$StarWarsPlot <- renderPlot({
      starwars |> 
        filter(height > input$taille) |>
        ggplot(aes(x = height)) +
        geom_histogram(
          binwidth = 10, 
          fill = "blue", 
          color = "white"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

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
                        value = 100), 
            selectInput(inputId="gender",
                        label="character's gender",c("masculine", "feminine"),selected = NULL, multiple = FALSE, selectize = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput(outputId = "nbtext"),
           plotOutput(outputId="StarWarsPlot"),
           DTOutput(outputId="StarwarsDT")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$nbtext<-renderText({paste(starwars |> 
        filter(height > input$taille, gender==input$gender) |> nrow()," personnages concernés")
    })
  
    output$StarWarsPlot <- renderPlot({
      starwars |> 
        filter(height > input$taille & gender==input$gender) |> 
        ggplot(aes(x = height)) +
        geom_histogram(
          binwidth = 10, 
          fill = "blue", 
          color = "white"
        ) +
        labs(title=paste("Taille pour les personnes ", input$gender,"mesurant plus de",input$taille, " cm" ))
    })
    
    output$StarwarsDT<-renderDT({
      starwars|> 
        filter(height > input$taille & gender==input$gender)|> select(name) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

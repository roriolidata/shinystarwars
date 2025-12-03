library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(DT)


#bslib::bs_theme_preview()  pour aller faire du CSS et qu'il soit importé dans la console

thematic::thematic_shiny(font = "auto")



# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(
  version = 5,
  bootswatch="minty"
),

    # Application title
    titlePanel("Starwars"),h1("Star Wars Characters"),

    # Sidebar with a slider input for number of taille 
    sidebarLayout(
        sidebarPanel(
          actionButton(
            inputId="bouton",
            label="clique-moi"
          ),
          actionButton(
            inputId="bouton2",
            label="clique-moi-encore"
          )

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
  
  rv <- reactiveValues(df = NULL)
  
  observeEvent(input$bouton, {
    message("vous avez cliqué sur le bouton")
  })
  
  observeEvent(input$bouton2, {
    showNotification(
      "La valeur du slider a changé !",
      type = "message"
    )
    rv$df<-starwars |> filter(height > input$taille, gender==input$gender)
    rv$plot<-rv$df |> 
      
  })
  
  
    
  })
    
  
    output$nbtext<-renderText({paste(rv$df |> nrow()," personnages concernés")
    })
  
    output$StarWarsPlot <- renderPlot({
      rv$df |> 
        ggplot(aes(x = height)) +
        geom_histogram(
          binwidth = 10, 
          fill = "blue", 
          color = "white"
        ) +
        labs(title=paste("Taille pour les personnes ", input$gender,"mesurant plus de",input$taille, " cm" ))
    })
    
    output$StarwarsDT<-renderDT({
      rv$df|> select(name,height) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

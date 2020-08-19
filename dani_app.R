library(shiny)
library(ggiraph)
source("src/plots.R")
source("src/preprocesar.R")
source("src/utils.R")

## ------------------------------------------------
input_filename = "data/Covid19Casos.csv"
output_filename = "data/Covid19Casos-procesado.RDS"
## ------------------------------------------------

data = load_processed_data(input_filename, output_filename)

SIDEBARPANEL_WIDTH = 2

ui <- fluidPage(
    titlePanel('Positividad'),
    sidebarLayout(
        sidebarPanel(
            selectInput("distrito", "Distrito", choices = c("AMBA", "PAIS")),
            conditionalPanel(
                condition = "input.tabs == 'Mapa'",
                numericInput("semana", "Semana", value = 14)
            ),
            width = SIDEBARPANEL_WIDTH
        ),
        mainPanel(
            tabsetPanel(
                type="tabs",
                id = "tabs",
                tabPanel('Curvas', plotlyOutput("plot_semanas_todas")),
                tabPanel('Mapa', girafeOutput("plot_por_semana"))),
            width = 12 - SIDEBARPANEL_WIDTH)
    )
)

server <- function(input, output, session) {
    
    output$plot_semanas_todas <- renderPlotly({
        plot_curva_positividad(data, input$distrito)
    })
    
    output$plot_por_semana <- renderGirafe({
        grafico_por_semana(input$semana, data, input$distrito)
    })
}

shinyApp(ui = ui, server = server)

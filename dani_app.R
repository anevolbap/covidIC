library(shiny)

source("src/plots.R")
source("src/preprocesar.R")
source("src/utils.R")

## ------------------------------------------------
input_filename = "data/Covid19Casos.csv"
output_filename = "data/Covid19Casos-procesado.csv"
## ------------------------------------------------

data = load_processed_data(input_filename, output_filename)
    
ui <- fluidPage(
    titlePanel('DaniApp'),
    sidebarLayout(
        sidebarPanel(
            numericInput("semana", "Semana", value = 21)
        ),
        mainPanel(
            tabsetPanel(
                type="tabs",
                id = "tabs",
                tabPanel('Todas País', plotlyOutput("plot_semanas_todas")),
                tabPanel('País', plotlyOutput("plot_semana_PAIS")),
                tabPanel('Todas AMBA', plotlyOutput("plot_semanas_toda_AMBA")),
                tabPanel('AMBA', plotlyOutput("plot_semana_AMBA"))
            )
        )
    )
)

server <- function(input, output, session) {
    
    output$plot_semanas_todas <- renderPlotly({
        graficosemanas(data)
    })
    
    output$plot_semana_PAIS <- renderPlotly({
        graficosemanaPAIS(input$semana, data, argen)
    })

    output$plot_semanas_toda_AMBA <- renderPlotly({
        graficosemanasAMBA(data)
    })
    
    output$plot_semana_AMBA <- renderPlotly({
        graficosemanaAMBA(input$semana, data, mapaAMBA)
    })
}

shinyApp(ui = ui, server = server)

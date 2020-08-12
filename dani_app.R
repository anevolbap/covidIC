library(shiny)

source("src/plots.R")
source("src/preprocesar.R")

covdeter = preprocesar("data/Covid19Casos.csv")

ui <- fluidPage(
    titlePanel('DaniApp'),
    sidebarLayout(
        sidebarPanel(
            h4("Parameters"),
            numericInput("semana", "semana", value = 21)
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
        graficosemanas(covdeter)
    })
    
    output$plot_semana_PAIS <- renderPlotly({
        graficosemanaPAIS(input$semana, covdeter, argen)
    })

    output$plot_semanas_toda_AMBA <- renderPlotly({
        graficosemanasAMBA(covdeter)
    })
    
    output$plot_semana_AMBA <- renderPlotly({
        graficosemanaAMBA(input$semana, covdeter, mapaAMBA)
    })
}

shinyApp(ui = ui, server = server)

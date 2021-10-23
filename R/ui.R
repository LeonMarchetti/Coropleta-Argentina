library(leaflet)
library(shiny)

ui <- fillPage(
    sidebarLayout(
        sidebarPanel(width = 3,
                     style = "position:fixed;width:inherit;overflow-y:scroll;height:100vh",
            selectInput(inputId = "columna",
                        label = "Dato",
                        choices = c("personas", "hogares")),
            selectInput(inputId = "departamen",
                        label = "Partido/Departamento",
                        choices = c()),
            selectInput(inputId = "provincia",
                        label = "Filtrar por provincia",
                        choices = c()),
            sliderInput(inputId = "grosor",
                        label = "Grosor de línea",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.1),
            sliderInput(inputId = "opacidad",
                        label = "Opacidad de coloreo",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.1),
        ),
        mainPanel(width = 9,
            leafletOutput("mapa", height = "100vh")
        ),
        fluid = FALSE
    )
)

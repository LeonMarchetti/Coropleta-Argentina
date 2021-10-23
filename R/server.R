library(leaflet)
library(rgdal)


cat("\n● Coropleta-Argentina: server.R: ", format(Sys.time(), "%a %b %d %X %Y"), "\n")


#' Importa los datos de los departamentos de las provincias del país desde un
#' archivo.
#'
#' @return Un dataframe con los datos de los departamentos.
importar_datos <- function() {
    datos <- readOGR(dsn = "../data/codgeo/pxdptodatosok.shp", verbose = FALSE)

    # Convierto columnas con números a tipo numérico
    datos@data[,5:10] <- sapply(datos@data[,5:10], as.numeric)
    datos
}

#' Inicializa el mapa, creando la capa de los polígonos de los departamentos
#' con un fondo blanco y la capa con los bordes de los departamentos.
#'
#' @param output Parámetro `output` de la función `server` usada en `shinyApp`
#' @param grosor Grosor de las líneas de los bordes de los polígonos
#' @param opacidad Opacidad del color de los polígonos
inicializar_mapa <- function(output, grosor = 1, opacidad = 0.5) {
    l <- leaflet() %>%
        addTiles() %>%
        addMapPane("MainPane", zIndex=400) %>%
        addMapPane("BordesPane", zIndex=300) %>%
        addPolygons(group = "Main",
                    layerId = ~link,
                    options = pathOptions(pane = "MainPane"),
                    weight = 0,
                    fillColor = "white",
                    fillOpacity = opacidad,
                    data = df,
                    label = ~paste(departamen, ", ", provincia, sep = "")) %>%
        addPolygons(group = "Bordes",
                    options = pathOptions(pane = "BordesPane"),
                    color = "black", stroke = TRUE, opacity = grosor, weight = 1,
                    fillOpacity = 0,
                    data = df)

    output$mapa <- renderLeaflet(l)
}

#' Arma la capa de los polígonos de los departamentos, cada uno con su color
#' correspondiente de acuerdo a si el parámetro correspondiente es menor, mayor
#' o igual al parámetro del partido seleccionado.
#'
#' Actualiza la leyenda del mapa con el nombre de la columna seleccionada, si
#' no hay un partido seleccionado se quita de la vista.
#'
#' @param columna Nombre de la columna seleccionada del dataset
#' @param link Identificación del departamento seleccionado
#' @param opacidad Opacidad del color de los polígonos
armar_mapa <- function(columna, link, opacidad = 0.5) {
    objeto <- df[df$link == link,]

    l <- leafletProxy("mapa") %>%
        clearGroup("Main")

    if (nrow(objeto) <= 0) {
        df[TRUE, "color"] <- "white"
        l %>% removeControl("ColorLegend")

    } else {
        val <- objeto[[columna]]

        df[df[[columna]] > val, "color"] <- "#FF0000"
        df[df[[columna]] == val, "color"] <- "#FFFF00"
        df[df[[columna]] < val, "color"] <- "#00FF00"

        l %>% addLegend(layerId = "ColorLegend",
                        title = paste("Dato:", columna),
                        colors = c("red", "yellow", "green"),
                        labels = c("Mayor", "Igual/Seleccionado", "Menor"))
    }

    l %>% addPolygons(group = "Main",
                      layerId = ~link,
                      options = pathOptions(pane = "MainPane"),
                      weight = 0,
                      fillOpacity = opacidad,
                      data = df,
                      fillColor = ~color,
                      label = ~paste(departamen, ", ", provincia, ": ", get(columna), sep = ""))
}

#' Arma la capa de los bordes de los departamentos.
#'
#' @param grosor Grosor de las líneas de los bordes de los polígonos
armar_bordes <- function(grosor = 1) {
    leafletProxy("mapa") %>%
        clearGroup("Bordes") %>%
        addPolygons(group = "Bordes",
                    options = pathOptions(pane = "BordesPane"),
                    color = "black", stroke = TRUE, opacity = grosor, weight = 1,
                    fillOpacity = 0,
                    data = df)
}

#' Actualiza la capa del borde de un solo departamento para mostrar solamente
#' el borde del departamento correspondiente.
#'
#' @param link Identificación del departamento seleccionado
agregar_borde_simple <- function(link) {
    l <- leafletProxy("mapa") %>%
        clearGroup("BordeSimple")

    if (link != 0 && !is.null(link)) {
        l %>% addPolygons(data = df[df$link == link,],
                    group = "BordeSimple",
                    weight = 1, opacity = 1, color = "black", stroke = TRUE,
                    options = pathOptions(pane = "BordesPane"))
    }
}

df0 <- importar_datos()

# HACK Descomentar la linea siguiente para filtrar el dataset por las provincias seleccionadas.
# df0 <- df0[df0$provincia %in% c("Buenos Aires", "San Luis"),]

df0$label <- paste(df0$departamen, ", ", df0$provincia, sep="")
df <- df0

#' Función `server` para usar como parámetro en la llamada a la función
#' `shinyApp`.
#'
#' @param input Referencias los valores de los controles de la interfaz
#' @param output Referencias a los objetos de la interfaz
#' @param session Referencia a la sesión actual
server <- function(input, output, session) {
    df <<- df0

    currentLink <- reactiveVal(0)

    updateSelectInput(session,
                      inputId = "departamen",
                      choices = df$label,
                      selected = "NULL")

    updateSelectInput(session,
                      inputId = "provincia",
                      choices = c("Todas", unique(df$provincia)),
                      selected = "NULL")

    inicializar_mapa(output)

    observeEvent(input$mapa_shape_click, {
        if (input$mapa_shape_click$id != currentLink()) {
            currentLink(input$mapa_shape_click$id)
            armar_mapa(input$columna, currentLink(), input$opacidad)

            departamen <- df[df$link == currentLink(),]$label
            updateSelectInput(session,
                              inputId = "departamen",
                              selected = departamen)
        }
    })

    observeEvent(input$mapa_shape_mouseover, {
        agregar_borde_simple(input$mapa_shape_mouseover$id)
    }, ignoreInit = TRUE)

    observeEvent(input$departamen, {
        departamen <- df[df$label == input$departamen,]
        if (departamen$link != currentLink())  {
            currentLink(departamen$link)
            armar_mapa(input$columna, currentLink(), input$opacidad)
        }
    }, ignoreInit = TRUE)

    observeEvent(input$provincia, {
        if (input$provincia == "Todas") {
            df <<- df0
        } else {
            df <<- df0[df0$provincia == input$provincia,]
        }

        updateSelectInput(session, inputId = "departamen",
                          choices = df$label,
                          selected = "NULL")

        departamen <- df[df$link == currentLink(),]
        if (nrow(departamen) <= 0) {
            updateSelectInput(session, inputId = "departamen",
                              selected = "NULL")
            currentLink(0)
        } else {
            updateSelectInput(session, inputId = "departamen",
                              selected = departamen$label)
        }
        agregar_borde_simple(currentLink())

        armar_mapa(input$columna, currentLink(), input$opacidad)
        armar_bordes(input$grosor)
    }, ignoreInit = TRUE)

    observeEvent({ input$columna; input$opacidad }, {
        if (currentLink() != 0) {
            armar_mapa(input$columna, currentLink(), input$opacidad)
        }
    }, ignoreInit = TRUE)

    observeEvent(input$grosor, {
        armar_bordes(input$grosor)
    }, ignoreInit = TRUE)
}

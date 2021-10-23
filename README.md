---
output:
  html_document: default
  pdf_document: default
editor_options: 
  markdown: 
    wrap: 80
---

# Coropleta Argentina

Muestra un mapa interactivo de la República Argentina dividida según sus
departamentos o partidos. Los colorea de acuerdo a si uno de sus parámetros es
mayor, menor o igual que el mismo parámetro de un departamento seleccionado.

![Captura de ejemplo](examples/screenshot.png?raw=true "Captura de ejemplo")

## Ejecución

Desde RStudio, abrir cualquiera de los archivos `App.R`, `server.R` o `ui.R` y
hacer clic en **Run App**.

## Uso

Se puede hacer clic en el mapa en un departamento o partido determinado y el
mapa se modifica para colorear según el criterio correspondiente.

Colorea en verde si el parámetro del departamento es menor al del departamento
seleccionado, rojo si es mayor o amarillo si es igual. El departamento
seleccionado queda de color amarillo.

Controles:

-   **Dato**: Parámetro por el cual se comparan a los departamentos. Datos
    disponibles:
    -   Personas
    -   Hogares
-   **Partido/Departamento**: Alternativamente se puede elegir de una lista
    desplegable el departamento. Para cada uno se indica la provincia a la que
    pertenece.
-   **Filtrar por provincia**: Se puede mostrar solamente los partidos de la
    provincia seleccionada, o de todas.
-   **Grosor de línea**: Controla el grosor de los bordes de los departamentos,
    desde 0 (sin bordes) a 1 (con bordes opacos).
-   **Opacidad de coloreo**: Controla la opacidad del coloreo de los
    departamentos, de 0 (sin colorear) a 1 (opaco).

## Datos

Fuente de los datos: [Censo 2010 Argentina,
INDEC](https://sitioanterior.indec.gob.ar/codgeo.asp)

## Licencia

GNU General Public License v3.0 or posterior

Ver LICENSE para ver el texto completo.

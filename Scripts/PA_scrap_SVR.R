#
# OBJETIVO: Aplicación que devuelva datos scrapeados de link de busqueda en paginas amarillas

library(shiny)
library(reactable)
library(stringr)
library(purrr)
library(rvest)
library(dplyr)
library(rmarkdown)

rsconnect::setAccountInfo(name='imorosity',
                          token='A8594BCA98FF19E7A89D7B3DADB04C24',
                          secret='tXoVPFx7AKgqSVzDXSCWKc0lao7uJEeDrWFUq9SQ')

# Unificamos para tener todo el rango de URLs
#   Creamos una función para ello
unificarURL<- function(x){
  rango <- seq(1,20)
  a <- str_split_fixed(x, '[:digit:]', 2)
  full_url <- paste0(a[1], rango, a[2])
  full_url
}

# Creo una funcion para scrapear
scraperPaginasAmarillas <- function(x){
  page <- read_html(x)
  empresa <- page %>%
    html_nodes('h2') %>%
    html_text()
  direccion <- page %>%
    html_nodes('div.links') %>%
    html_text() 
  telf <- page %>%
    html_nodes('div.col-xs-4') %>%
    html_text()
  telf <-  str_subset(telf , 'Ver teléfono')
  
  #combine, name, and make it a tibble
  Directorio <-as.data.frame(cbind(head(empresa,-1), direccion, telf))
  names(Directorio) <- c("Empresa", "Dirección", "Telf")
  return(Directorio)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Paginas Amarillas Scrap Busquedas"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("link_to_scrap",
                "Link to scrap:",
                value = "https://www.paginasamarillas.es/search/mensajeria/all-ma/bizkaia/all-is/all-ci/all-ba/all-pu/all-nc/1?what=mensajeria&where=vizcaya&ub=false&qc=true"),
      actionButton("get_data", "Get data", class = "btn-primary"),
      br(),br(),
      downloadButton("download_data", "Download data")
    ),
    
    # Show results
    mainPanel(
      reactableOutput("Datos_Scrap")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  finalURL <- eventReactive(input$get_data, {
    unificarURL(input$link_to_scrap)
  })
  
  DF <- reactive ({
    req(finalURL())
    map_df(finalURL(), scraperPaginasAmarillas)
  })
  
  output$Datos_Scrap <- renderReactable({
    reactable::reactable(DF(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200)
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$link_to_scrap, "_", Sys.Date(), ".csv", sep = ";")
    },
    content = function(file) {
      write.csv2(DF(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)


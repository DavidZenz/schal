library(rvest)
library(xml2)
library(shiny)
library(data.table)
library(stringr)
library(dplyr)
library(leaflet)
library(geojsonio)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Ist es kalt genug sein Gesicht zu verhüllen?"),
  # withMathJax(helpText("Windchill formula $$\\vartheta_\\mathrm{WCT} = 13{,}12 + 0{,}6215 \\cdot \\vartheta_\\mathrm{a} + (0{,}3965 \\cdot \\vartheta_\\mathrm{a} - 11{,}37 ) \\cdot v^{0{,}16} \\!$$")),
  # withMathJax(helpText("Windchill-temperature $$\\vartheta_\\mathrm{WCT}$$")),
  # withMathJax(helpText("Air-temperature $$\\vartheta_\\mathrm{a}$$ in degree celsius")),
  # withMathJax(helpText("and Wind-speed $$v$$ in kmh")),
  # leafletOutput("myMap"),
  dataTableOutput("myTable")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read_html("https://www.zamg.ac.at/cms/de/wetter/wetterwerte-analysen") %>% html_nodes("table") %>% html_table
  data <- data.table(data[[1]])
  names(data)[1] <- "Ort"
  names(data)[2] <- "Höhe.m"
  names(data)[3] <- "Temp"
  names(data)[6] <- "Windspitzen.kmh"
  names(data)[7] <- "Niederschlag.mm"
  names(data)[8] <- "Sonne.%"
  names(data)[9] <- "Luftdruck.hpa"
  
  data$Höhe.m <- as.numeric(str_split(data$Höhe, pattern = "m", simplify = TRUE)[,1])
  data$Temp <- as.numeric(str_split(data$Temp, pattern = "°", simplify = TRUE)[,1])
  data$rel.Feuchte <- as.numeric(str_split(data$rel.Feuchte, pattern = " ", simplify = TRUE)[,1])
  data$Windspitzen.kmh <- as.numeric(str_split(data$Windspitzen, pattern = " ", simplify = TRUE)[,1])
  data$Niederschlag.mm <- as.numeric(str_split(data$Niederschlag.mm, pattern = " ", simplify = TRUE)[,1])
  data$`Sonne.%` <- as.numeric(str_split(data$`Sonne.%`, pattern = " ", simplify = TRUE)[,1])
  data$Luftdruck.hpa <- as.numeric(str_split(data$Luftdruck, pattern = " ", simplify = TRUE)[,1])
  
  wind <- data.table(str_split(data$Wind, pattern = " ", simplify = TRUE))
  wind <- select(wind, -3)
  names(wind) <- c("Windrichtung", "Windgeschwindigkeit")
  wind$Windrichtung <- str_split(wind$Windrichtung, pattern = ",", simplify = TRUE)[,1]
  wind$Windgeschwindigkeit <- as.numeric(wind$Windgeschwindigkeit)
  
  data <- cbind(select(data, Ort, Höhe.m, Temp, rel.Feuchte, Windspitzen.kmh, Niederschlag.mm, "Sonne.%", Luftdruck.hpa), wind)
  subset <- data[data$Windgeschwindigkeit > 5,]
  
  Windchill <- round(13.12 + 0.6214*subset$Temp + (0.3965*subset$Temp - 11.37)*(subset$Windgeschwindigkeit^0.16), digits = 2)
  Gehen <- round(13.12 + 0.6214*subset$Temp + (0.3965*subset$Temp - 11.37)*((subset$Windgeschwindigkeit+4)^0.16), digits = 2)
  Laufen <- round(13.12 + 0.6214*subset$Temp + (0.3965*subset$Temp - 11.37)*((subset$Windgeschwindigkeit+12)^0.16), digits = 2)
  Fahrrad <- round(13.12 + 0.6214*subset$Temp + (0.3965*subset$Temp - 11.37)*((subset$Windgeschwindigkeit+20)^0.16), digits = 2)
  
  bigger <- cbind(subset, Windchill, Gehen, Laufen, Fahrrad)
  smaller <- cbind(data[data$Windgeschwindigkeit <= 5,], 
                   Windchill = data[data$Windgeschwindigkeit <= 5,]$Temp, 
                   Gehen = data[data$Windgeschwindigkeit <= 5,]$Temp, 
                   Laufen = data[data$Windgeschwindigkeit <= 5,]$Temp, 
                   Fahrrad = data[data$Windgeschwindigkeit <= 5,]$Temp)
  
  
  data <- rbind(bigger, smaller)
  
  # karte <- geojson_read("laender_999_geo.json", what = "sp")
  # 
  # m <- leaflet(karte) %>%
  #   addProviderTiles("OpenStreetMap.DE") %>%
  #   addPolygons()
  # 
  # output$myMap <- renderLeaflet(m)
  
  # output$myTable <- renderDataTable(data) %>%
  #   formatStyle("Temp", target = "row", backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow')))
  
  output$myTable<- DT::renderDataTable({ 
    dat <- datatable(data) %>%
      formatStyle("Windchill", target = "row", backgroundColor = styleInterval(c(0), c('lightgreen', 'red')))
    return(dat)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
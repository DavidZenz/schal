library(xml2)
library(rvest)
library(shiny)
library(data.table)
library(stringr)
library(dplyr)
library(leaflet)
# library(geojsonio)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Ist es kalt genug sein Gesicht zu verhüllen?"),
  # withMathJax(helpText("Windchill formula $$\\vartheta_\\mathrm{WCT} = 13{,}12 + 0{,}6215 \\cdot \\vartheta_\\mathrm{a} + (0{,}3965 \\cdot \\vartheta_\\mathrm{a} - 11{,}37 ) \\cdot v^{0{,}16} \\!$$")),
  # withMathJax(helpText("Windchill-temperature $$\\vartheta_\\mathrm{WCT}$$")),
  # withMathJax(helpText("Air-temperature $$\\vartheta_\\mathrm{a}$$ in degree celsius")),
  # withMathJax(helpText("and Wind-speed $$v$$ in kmh")),
  leafletOutput("myMap"),
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
  
  coord <- data.table(
    rbind(
      c(48.2494574,16.3490232), # Hohe Warte, Wien
      c(48.2078783,16.2268039), # Mariabrunn, Wien
      c(47.8389073,16.4993557), # Eisenstadt
      c(48.1938732,15.5769755), # St. Pölten
      c(48.2951579,14.2573654), # Linz
      c(48.2370334,14.1885052), # Hörsching Flughafen, Linz
      c(47.7869873,13.0551723), # Schloss Freisaal, Salzburg
      c(47.7926502,13.0007387), # Flughafen, Salzburg
      c(47.7952162,13.0659847), # Salzburg-Aigen, Salzburg
      c(47.2576525,11.3491188), # Flughafen, Innsbruck
      c(47.5071327, 9.7030756), # Bregenz
      c(47.0777908,15.4477239), # Universität Graz, Graz
      c(46.9940491,15.4379428), # Flughafen, Graz
      c(46.8298034,12.7164207), # Lienz
      c(46.6500162,14.3226691), # Flughafen, Klagenfurt
      c(47.0541335,12.9485785), # Sonnblick
      c(47.8166802,13.7079119), # Feuerkogel
      c(46.6044982,13.6638532)  # Villacher Alpe
      )
  )
  
  data$Höhe.m <- as.numeric(str_split(data$Höhe, pattern = "m", simplify = TRUE)[,1])
  data$Temp <- as.numeric(str_split(data$Temp, pattern = "°", simplify = TRUE)[,1])
  data$rel.Feuchte <- as.numeric(str_split(data$rel.Feuchte, pattern = " ", simplify = TRUE)[,1])
  data$Windspitzen.kmh <- as.numeric(str_split(data$Windspitzen, pattern = " ", simplify = TRUE)[,1])
  data$Niederschlag.mm <- as.numeric(str_split(data$Niederschlag.mm, pattern = " ", simplify = TRUE)[,1])
  data$`Sonne.%` <- as.numeric(str_split(data$`Sonne.%`, pattern = " ", simplify = TRUE)[,1])
  data$Luftdruck.hpa <- as.numeric(str_split(data$Luftdruck, pattern = " ", simplify = TRUE)[,1])
  
  data <- data.table(data, lon = coord$V2, lat = coord$V1)
  
  wind <- data.table(str_split(data$Wind, pattern = " ", simplify = TRUE))
  wind <- select(wind, -3)
  names(wind) <- c("Windrichtung", "Windgeschwindigkeit")
  wind$Windrichtung <- str_split(wind$Windrichtung, pattern = ",", simplify = TRUE)[,1]
  wind$Windgeschwindigkeit <- as.numeric(wind$Windgeschwindigkeit)
  
  data <- cbind(select(data, Ort, lon, lat, Höhe.m, Temp, rel.Feuchte, Windspitzen.kmh, Niederschlag.mm, "Sonne.%", Luftdruck.hpa), wind)
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
  
  getColor <- function(data) {
    sapply(data$Windchill, function(Windchill) {
      if(Windchill <= 0) {
        "green"
      } else {
        "red"
      }
      })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(data)
  )
  
  # karte <- geojson_read("laender_999_geo.json", what = "sp")
  # 
  # m <- leaflet(karte) %>%
  #   addProviderTiles("OpenStreetMap.DE") %>%
  #   addPolygons()
  # 
  # output$myMap <- renderLeaflet(m)
  map <- leaflet(data = data) %>% addTiles() %>% setView(11.1031011, 47.6783193, zoom = 7) %>% addMarkers(~lon, ~lat, icon = icons)
  output$myMap = renderLeaflet(map)
  
  # output$myTable <- renderDataTable(data) %>%
  #   formatStyle("Temp", target = "row", backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow')))
  
  output$myTable<- DT::renderDataTable({ 
    dat <- datatable(data, options = list(paging = FALSE)) %>%
      formatStyle(c("Windchill", "Gehen", "Laufen", "Fahrrad"), target = "cell", backgroundColor = styleInterval(c(0), c('lightgreen', 'red')))
    return(dat)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
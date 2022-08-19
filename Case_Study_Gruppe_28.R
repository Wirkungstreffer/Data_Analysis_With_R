#### CASE STUDY 29 ####
  
### 1.Datenimport ### 
  
# Installieren und Laden aller notwendigen Pakete.
if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}

if(!require(readxl)) {
  install.packages("readxl")
  require(readxl)
}

if(!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}

if(!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

if(!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if(!require(shinyjs)) {
  install.packages("shinyjs")
  require(shinyjs)
}

if(!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if(!require(leaflet.extras)) {
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

if(!require(tidyr)) {
  install.packages("tidyr")
  require(tidyr)
}

if(!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

if(!require(plyr)) {
  install.packages("plyr")
  require(plyr)
}

if(!require(readr)) {
  install.packages("readr")
  require(readr)
}

if(!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}

# Pfad setzen.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Einzelteil_file <- list.files(path = "Data/Einzelteil/")
Einzelteil_path <- file.path("Data/Einzelteil", Einzelteil_file)

Fahrzeug_file <- list.files(path = "Data/Fahrzeug/")
Fahrzeug_path <- file.path("Data/Fahrzeug", Fahrzeug_file)

Geodaten_file <- list.files(path = "Data/Geodaten/")
Geodaten_path <- file.path("Data/Geodaten", Geodaten_file)

Komponente_file <- list.files(path = "Data/Komponente/")
Komponente_path <- file.path("Data/Komponente", Komponente_file)

Zulassungen_file <- list.files(path = "Data/Zulassungen/")
Zulassungen_path <- file.path("Data/Zulassungen", Zulassungen_file)

Additional_file <- list.files(path = "Additional_Files_Group_28")
Additional_path <- file.path(path = "Additional_Files_Group_28", Additional_file)

options(warn = -1)

# Einzelteil_T11 Data importieren und ordnen.
ET11 <- read_lines(Einzelteil_path[11], n_max = -1, skip = 0, na=character(), progress = show_progress())
summary(ET11)
New_ET11 <-str_split(ET11,'\f')
New_ET11 <- New_ET11[[1]][-1]
New1_ET11 <- gsub("\t",' ',New_ET11)
New2_ET11 <- strsplit(gsub('[\f"]', " ", New1_ET11), " +")
New3_ET11 <- do.call(rbind, New2_ET11)
DF_ET11<-as.data.frame(New3_ET11)
Einzelteil_T11 <- subset(DF_ET11, select = -c(V1))
colnames(Einzelteil_T11) <- c("Nummer", "X1","ID_T11", "Herstellernummer", "Werksnummer", "Fehlerhaft", 
                              "Fehlerhaft_Datum", "Fehlerhaft_Fahrleistung", "Produktionsdatum_Origin_01011970","Origin")
summary(Einzelteil_T11)

#nicht mehr benutzte Daten ausraeumen.
rm(ET11)
rm(New_ET11)
rm(New1_ET11)
rm(New2_ET11)
rm(New3_ET11)
rm(DF_ET11)

# Bestandteile_Fahrzeuge_OEM1_Typ11 und Bestandteile_Fahrzeuge_OEM1_Typ12 Datei importieren.
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read_csv2(Fahrzeug_path[1], col_names = TRUE, col_types = cols())
Bestandteile_Fahrzeuge_OEM1_Typ12 <- read_csv2(Fahrzeug_path[2], col_names = TRUE, col_types = cols())
summary(Bestandteile_Fahrzeuge_OEM1_Typ11)
summary(Bestandteile_Fahrzeuge_OEM1_Typ12)

# Fahrzeuge_OEM1_Typ11 und Fahrzeuge_OEM1_Typ12 Datei importieren.
Fahrzeuge_OEM1_Typ11 <- read_csv(Fahrzeug_path[5], col_names = TRUE, col_types = cols())
Fahrzeuge_OEM1_Typ12 <- read_csv2(Fahrzeug_path[6], col_names = TRUE, col_types = cols())
summary(Fahrzeuge_OEM1_Typ11)
summary(Fahrzeuge_OEM1_Typ12)
  
# Geodaten_Gemeinden_v1.2_2017-08-22_TrR Datei importieren und ordnen.
Geodaten_Gemeinden <- read_csv2(Geodaten_path[1], col_names = TRUE, col_types = cols())
setnames(Geodaten_Gemeinden, old="Gemeinde", new="Gemeinden")
summary(Geodaten_Gemeinden)

# Bestandteile_Komponente_K2LE1 und Bestandteile_Komponente_K2ST1  Dateien importieren
Bestandteile_Komponente_K2LE1 <- read_csv2(Komponente_path[5], col_names = TRUE, col_types = cols())
Bestandteile_Komponente_K2ST1 <- read_csv2(Komponente_path[7], col_names = TRUE, col_types = cols())
summary(Bestandteile_Komponente_K2LE1)
summary(Bestandteile_Komponente_K2ST1)

# Zulassungen_alle_Fahrzeuge importieren und ordnen
Zulassungen_alle_Fahrzeuge <- read_csv2(Zulassungen_path[1], col_names = TRUE, col_types = cols())
summary(Zulassungen_alle_Fahrzeuge)


### 2.Ordnung der Datei ###


# ID_T11 Spalte in Bestandteile_Komponente_K2LE1 ausw?hlen und in numerische Variable umwandeln
Bestandteile_Komponente_K2LE1$NrID_T11 <- gsub('-', '', Bestandteile_Komponente_K2LE1$ID_T11)
Bestandteile_Komponente_K2LE1$NrID_T11 <- as.numeric(Bestandteile_Komponente_K2LE1$NrID_T11)
CBK2LE1 <- data.frame(Bestandteile_Komponente_K2LE1$NrID_T11, Bestandteile_Komponente_K2LE1$ID_K2LE1)
colnames(CBK2LE1)<-c("T11ID", "ID_K")
  
# ID_T11 Spalte in Bestandteile_Komponente_K2ST1 ausw?hlen und in numerische Variable umwandeln
Bestandteile_Komponente_K2ST1$NrID_T11 <- gsub('-','', Bestandteile_Komponente_K2ST1$ID_T11)
Bestandteile_Komponente_K2ST1$NrID_T11 <- as.numeric(Bestandteile_Komponente_K2ST1$NrID_T11)
CBK2ST1 <- data.frame(Bestandteile_Komponente_K2ST1$NrID_T11,Bestandteile_Komponente_K2ST1$ID_K2ST1)
colnames(CBK2ST1)<-c("T11ID", "ID_K")

# ID_T11 Spalte in Einzelteil_T11 auswaehlen und in numerische Variable umwandeln
Einzelteil_T11$NrID_T11 <- gsub('-', '', Einzelteil_T11$ID_T11)
Einzelteil_T11$NrID_T11 <- as.numeric(Einzelteil_T11$NrID_T11)
Einzelteil_T11$NrID_T11 <- sort(Einzelteil_T11$NrID_T11, decreasing=FALSE)

# Aus der Aufgabe ergebenden Bereich von ID_T11 in Einzelteil_T11 Datei auswaelen
NewEinzelteil_T11 <- subset(Einzelteil_T11, NrID_T11>=11213213119000 & NrID_T11<=11213213130100 )
CT11 <- data.frame(NewEinzelteil_T11$NrID_T11, 0)
colnames(CT11)<-c("T11ID", "ID_K")

# Alle ID_Sitze im aufgetragenen Bereich von ID_T11 Kombinieren
T11Combo <- rbind(CBK2LE1, CBK2ST1, CT11)
T11Combo <- subset(T11Combo,duplicated(T11Combo$T11ID) | duplicated(T11Combo$T11ID, fromLast=TRUE))
NewT11Combo<- distinct(T11Combo, T11Combo$T11ID, .keep_all = TRUE)

# Checken ob es NA-Eintraege gibt?
any(is.na(NewT11Combo)) # FALSE

# Nicht mehr benutzte Daten ausraeumen.
rm(CBK2LE1)
rm(NewEinzelteil_T11)
rm(CBK2ST1)
rm(CT11)
rm(T11Combo)

# Aufgetragene ID_Sitze und entsprechebnde ID_Fahrzeug von Datei auswaehlen
IDSK <- data.frame(NewT11Combo$ID_K, 0)
colnames(IDSK) <-c("ID_Sitze","ID_Fahrzeug")
IDSF11 <-data.frame(Bestandteile_Fahrzeuge_OEM1_Typ11$ID_Sitze, Bestandteile_Fahrzeuge_OEM1_Typ11$ID_Fahrzeug)
colnames(IDSF11) <-c("ID_Sitze","ID_Fahrzeug")
IDSF12 <-data.frame(Bestandteile_Fahrzeuge_OEM1_Typ12$ID_Sitze, Bestandteile_Fahrzeuge_OEM1_Typ12$ID_Fahrzeug)
colnames(IDSF12) <-c("ID_Sitze","ID_Fahrzeug")

# Durch aufgetragene ID_Sitze waehlen entsprechende ID_Fahrzeug von Datei aus
IDSCombo <- rbind(IDSF11,IDSF12,IDSK)
IDSCombo <-subset(IDSCombo,duplicated(IDSCombo$ID_Sitze) | duplicated(IDSCombo$ID_Sitze, fromLast=TRUE))
NewIDSCombo<- distinct(IDSCombo,IDSCombo$ID_Sitze,.keep_all = TRUE)

# Checken ob es NA-Eintraege gibt?
any(is.na(NewIDSCombo)) # FALSE


# Nicht mehr benutzte Daten ausraeumen.
rm(IDSF11)
rm(NewT11Combo)
rm(IDSF12)
rm(IDSK)
rm(IDSCombo)

# Durch aufgetragene ID_Fahrzeug waehlen entsprechende Zulassungsdatum und entsprechende Zulassungsgemeinden
PreZula<- data.frame(0,NewIDSCombo$ID_Fahrzeug,"",as.Date("",format="%Y-%m-%d"))
colnames(PreZula)<- c("X1", "IDNummer","Gemeinden","Zulassung")
ZulaCombo <- rbind(Zulassungen_alle_Fahrzeuge,PreZula)
ZulaCombo<-subset(ZulaCombo,duplicated(ZulaCombo$IDNummer) | duplicated(ZulaCombo$IDNummer, fromLast=TRUE))
NewZulaCombo <- distinct(ZulaCombo,ZulaCombo$IDNummer,.keep_all = TRUE)

# Checken ob es NA-Eintraege gibt?
any(is.na(NewZulaCombo)) # FALSE

# Nicht mehr benutzte Daten ausraeumen.
rm(PreZula)
rm(ZulaCombo)

# ID_Fahrzeug, Zulassungsdatum, Zulassungsgemeinden, Produktionsdatum und Werksnummer zusammenbinden
New1zulaCombo <- NewZulaCombo[,c(1,2,3,4)]
New1zulaCombo <- dplyr::rename(New1zulaCombo, ID_Fahrzeug = IDNummer)
Fahrzeuge_Zusammen <- rbind(Fahrzeuge_OEM1_Typ11[,c(3,4,6)], Fahrzeuge_OEM1_Typ12[,c(3,4,6)])
New2ZulaCombo <- left_join(New1zulaCombo[,c(1,2,3,4)], Fahrzeuge_Zusammen, by = "ID_Fahrzeug")

# Checken ob es NA-Eintraege gibt?
any(is.na(New2ZulaCombo)) # FALSE

# nicht mehr benutzte Daten ausraeumen.
rm(New1zulaCombo)
rm(NewZulaCombo)

# Information ueber Bundeslaender integrieren
Bundeslaenderlist <- read_excel(Additional_path[1], col_names = TRUE)
Zusammenmerge <- merge(New2ZulaCombo, Geodaten_Gemeinden, by = "Gemeinden")
Newmerge <- merge(Zusammenmerge, Bundeslaenderlist, by = "Postleitzahl")

# Checken ob es NA-Eintraege gibt?
any(is.na(Newmerge)) # FALSE
  
# Nicht mehr benutzte Daten ausraeumen.
rm(Zusammenmerge)
rm(New2ZulaCombo)
  
# Alle grundlagende Information integrieren
Newdaten <- Newmerge[,c(1,2,4,5,6,7,10,11,12)]
New1daten <- left_join(Newdaten, NewIDSCombo[,c(1,2)], by = "ID_Fahrzeug")
colnames(Bestandteile_Komponente_K2LE1) <- c("X1", "ID_T11", "ID_T12/T14", "ID_T13/15", "ID_Sitze", "NrID_T11")
colnames(Bestandteile_Komponente_K2ST1) <- c("X1", "x", "ID_T11", "ID_T12/T14", "ID_T13/15", "ID_Sitze", "NrID_T11")
Bestandteile_Zusammen <- rbind(Bestandteile_Komponente_K2LE1, Bestandteile_Komponente_K2ST1[,c(1,3,4,5,6,7)])
Setdaten <- left_join(New1daten, Bestandteile_Zusammen[,c(2,5)], by = "ID_Sitze")
Alldaten <- Setdaten[c(11,10,3,4,2,1,9,5,6,7,8)]

# Checken ob es NA-Eintraege gibt?
any(is.na(Alldaten)) # FALSE

# Nicht mehr benutzte Daten ausraeumen.
rm(Newdaten)
rm(NewIDSCombo)
rm(Newmerge)
rm(New1daten)
rm(Bestandteile_Zusammen)
rm(Setdaten)

  
# Daten speichern im RData-Format.
save(Alldaten, file = "Final_Data_Group_28.RData")


### 3.Shiny App ###


# Alldaten fuer Analyse laden.
Alldaten <- get(load("Final_Data_Group_28.RData"))

# Struktur von Alldaten einlesen.
summary(Alldaten)

# Grenzung fuer zeitliche Slider setzen.
Firstday <- min(Alldaten$Zulassung)
Lastday <- max(Alldaten$Zulassung)

# Farben fuer Landkarte in Leaflet
Color <- 'yellow'

# icon bestimmen.
icon_settle <- awesomeIcons(
  icon = 'ion-android-car',
  iconColor = 'white',
  library = 'ion',
  markerColor = Color
)

# User Interface
ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(
    
    column(6,
           # Ueberschrift
           titlePanel(
             br(),
             title = "CASE_STUDY_Group_28" 
           ),
           h5("Introduction to Engineering Data Analytics with R"),
           h5("Technische Universitaet Berlin"),
           h5("SoSe 2019"),
           h3("Data-Analyse von Fahrzeugen mit unkorrekten Sitzen")
    )
  ),
  br(),
  
  fluidRow(
    
    # Titel fuer diagram
    h3("Zulassungen der Fahrzeug in gewaehlter Gemeinde", align = "center"),
    
    column(4,
           br(),
           br(),
           br(),
           
           # Seitenleiste fuer Auswahl der Datei
           wellPanel(
             
             # Auswahl fuer Gemeinden
             selectizeInput(
               inputId = "gemeinde",
               label = "Zulassungsgemeinden",
               choices = sort(unique(Alldaten$Gemeinden))
             ),
             
             # Auswahl fuer Zeitraum
             sliderInput(
               inputId = "zeitraum",
               label = "Zeitraum",
               min = as.Date(Firstday),
               max = as.Date(Lastday),
               value = c(as.Date(Firstday), as.Date(Lastday)),
               timeFormat = "%d %b %Y"
             ),
             
             # Aktualisierende Taste, um Eintraege des Main-Panel nach der Auswahl zu aktualiseren
             actionButton(
               inputId = "update",
               label = "Aktualisieren"
             )
           )
    ),
    
    # Output fuer Mainboard
    column(8,
           
           # Diagram ueber Zulassungen der Fahrzeug in ausgewaehlte Gemeinde
           plotlyOutput(
             outputId = "diagram"
           )
    ),
    
    br(),
    
    column(12,  
           tabsetPanel(
             
             # Tabelle 
             tabPanel("Grundlegende Daten",
                      dataTableOutput(outputId = "tabelle")
             ),
             
             # Daten gesamt setzen
             tabPanel("Gesamte Daten",
                      dataTableOutput(outputId = "daten_gesamt")
             )
           )
    ),
    
    br(),
    br(),
    
    # Karte
    column(12,
           # Titel fuer Karte
           br(),
           h3("Verteilung der Zulassungen von Fahrzeugen", align = "center"),
           br(),  
        
           # Die groesse von Landkarten anpassen.
           tags$style(type = "text/css", "#karte {height: calc(100vh - 120px) !important;}"),
           title = "Karte",
           leafletOutput(outputId = "karte"),
           br(),
           hr()
    )
  )
)


# Output durch Input von ui in server
server <- function(input, output, session) {
  
  # Aktualisierung Daten
  Inputdaten <- reactive({
    
    # Aktualisierungs Taste
    input$update
    isolate({
      
      # Die Daten nach ausgewaehlte Gemeinden filtern.
      daten <- filter(Alldaten, Gemeinden == input$gemeinde)
      
      # Die Frequenz der Zulassungen in gewaehlter Gemeinden rechnen .
      Freq_Gemeinden <- as.data.frame(table(daten$Zulassung)) %>%
        
        # Umbennenung fuer Daten beassere Zusammenbindung.
        dplyr::rename(Zulassung = Var1, Frequenze = Freq) %>%
        
        # Die From von die Datum der Zulassung aus die From der Datum veraendern.
        mutate(Zulassung = ymd(Zulassung))
      
      # Frequenze von Alldaten rechnen.
      daten_input <- left_join(daten, Freq_Gemeinden, by = "Zulassung") %>%
        
        # Zeitraums auswaehlen.
        filter(Zulassung %in% input$zeitraum[1]:input$zeitraum[2])
    })
  })
  
  # durch Slider die Alldaten bei Heatmap setzen.
  daten_filter <- reactive({
    input$update
    isolate({
      daten <- filter(Alldaten, Zulassung %in% input$zeitraum[1]:input$zeitraum[2])
    })
  })
  
  # Landkarte laden.
  output$karte <- renderLeaflet({
    
    leaflet(Alldaten) %>%
      addProviderTiles("CartoDB.Positron",
                       group = "Simpel (Standard)", 
                       options = providerTileOptions(minZoom = 2, maxZoom = 15)) %>%
      addTiles(group = "Open Street Map") %>%
      
      # Die Konzentration der Koordinaten.
      fitBounds(
        ~min(Laengengrad)-1, 
        ~min(Breitengrad)-1, 
        ~max(Laengengrad)+1, 
        ~max(Breitengrad)+1) 
  })
  
  # Output1 diagram
  output$diagram <- renderPlotly({
    
    # Aktualisierte Daten nach Wahl im Panel.
    Inputdaten() %>%
      
      # Output diagram fuer Zulassungen der Fahrzeuge
      plot_ly(x = ~Zulassung, 
              y = ~Frequenze,
              type = "scatter",
              mode = "markers",
              hoverinfo = "text",
              text = ~paste("Datum:", Zulassung, "<br>", "Fahrzeug:", ID_Fahrzeug),
              marker = list(color = "orange", size = 9)
      ) %>%
      
      # Beschreibung der Achsen.
      layout(xaxis = list(title = "Zeit"), 
             yaxis = list(title = "Anzahl von Zulassungen der Fahrzeuge"))
  })
  
  # Output2 Tabelle
  output$tabelle <- renderDataTable(
    
    # Aktualisierte Daten.
    select(Inputdaten(), 1:7),
    
    # Die Parameter fuer Seitenleiste setzen.
    options = list(
      pageLength = 4, 
      lengthMenu = c(4, 8, 12)
    )
  )
  
  # Output3 Tabelle
  output$daten_gesamt <- renderDataTable(
    
    Alldaten[order(Alldaten$Gemeinden),],
    
    options = list(
      pageLength = 4, 
      lengthMenu = c(4, 8, 12)
    )
  )
  
  # Output4 Heatmap
  observe({
    
    leafletProxy("karte", data = daten_filter()) %>%
      
      # Die Aenderung der Datei sollte vorher Ausgaben ausraeumen.
      clearShapes() %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearMarkerClusters %>%
      clearHeatmap %>%
      
      # Koordinaten von Alldaten auf Heatmap einsetzen.
      addHeatmap(lng = ~Laengengrad, 
                 lat = ~Breitengrad, 
                 max = .7,
                 group = "Heatmap",
                 blur = 70) %>%
      
      # Einstellungen der Marker.
      addAwesomeMarkers(
        lng = ~Laengengrad, 
        lat = ~Breitengrad,
        icon = icon_settle,
        
        # Popup informiert Zulassungsgemeinden, PLZ, Fahrzeug und Bundesland.
        popup = ~paste(
          "<b>Zulassungsgemeinden: </b>", Gemeinden, "<br>",
          "<b>PLZ: </b>", Postleitzahl, "<br>",
          "<b>ID_Fahrzeug: </b>", ID_Fahrzeug, "<br>",
          "<b>Bundesland: </b>", Bundesland, "<br>"),
        
        # die Marker in Cluster zusammensetzen
        clusterOptions = markerClusterOptions(),
        group = "Detailliert") %>%
      
      addLayersControl(
        baseGroups = c("Simpel (Standard)", "Open Street Map"),
        overlayGroups = c("Detailliert", "Heatmap"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = TRUE)) %>%
      
      # Heatmap ist zuerst verborgen.
      hideGroup(group = "Heatmap")
  })
  
}


# App aufrufen
shinyApp(ui = ui, server = server)


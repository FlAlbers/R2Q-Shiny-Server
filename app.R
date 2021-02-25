#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RSQLite) 
library(shinyMatrix)
library(rhandsontable)
library(readxl)
library(dplyr)
library(shinycssloaders)
library(shinybusy) 
library(writexl)
library(RMySQL)
library(DBI)
library(tidyverse)
#data structure
#conn <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
#Daten <- dbReadTable(conn, "Daten")
#dbDisconnect(conn)
#list_Massnahmen <- Daten$Name %>% unique()
#selected_row <- Daten[2,]

#options(shiny.maxRequestSize = 30*1024^2)

umsetzungbspBild = NA
systemskizzeBild = NA
umsetzungbspUptime = NA
systemskizzeUptime = NA


#the following vectors are the choice lists for the checkboxes
ressourcen = c("Niederschlagswasser", "Schmutzwasser", "Fläche", "Baustoffe", "Energie")

wifuNiederschlagswasser = c("Gewässerschutz", "Bodenschutz", "Überflutungsschutz", "Klimaanpassung")
wifuBaustoffe = c("BOM Bill of Material", "Monomaterial", "Einsparung von Primärmaterialien", "Nachwachsender Rohstoff", "Rohstofferhalt", "Rohstoffverfügbarkeit", "Rohstoffaufwand (gesamt)")
wifuFlaeche = c("Infrastrukturversorgung", "Nutzungsvielfalt", "Einsparung natürlicher Ressourcen", "Luftreinhaltung", "Biodiversität", "Aufenthalts-/ Freiraumqualität")
wifuSchmutzwasser = c("Gesundheitsvorsorge", "Gewässerschutz", "Trinwassereinsparung", "Nährstoffrückgewinnung")
wifuEnergie = c("Elektrizität", "Wärme", "Brennstoffe", "Erzeugung", "Verteilung", "Verbrauch")

anwendungsebenen = c("Gebäudeebene", "Grundstücksebene", "Quartiersebene")

entwicklungsstaende = c("Stand der Wissenschaft und Technik", "Stand der Technik", "Allgemein annerkannter Stand der Technik")

read_excel_allsheets <- function(filename, tibble = TRUE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(tibble) x <- lapply(x, as_tibble)
    names(x) <- sheets
    x
}
save1 <- read_excel_allsheets("R2Q_Datensatz_leer.xlsx")

getcon <- function(){
    dbConnect(MySQL(), user = "Flemming", password = "vo5Otei9", dbname = "r2q", host = "185.149.214.79")
}

con <- getcon()
list_Massnahmen <- dbGetQuery(con, "SELECT ressource, kategorieIndex, name, id FROM massnahmen ORDER BY ressource, kategorieIndex");
dbDisconnect(con)

# list_Massnahmen <- read_excel("Massnahmenliste.xlsx")
list_Massnahmen[["kategorieIndex"]] <- str_pad(list_Massnahmen[["kategorieIndex"]], 3, pad = "0")
list_Massnahmen <- unite(list_Massnahmen, Massnahmen, ressource:name)

massnahmen_dropdown <-append(list_Massnahmen$Massnahmen,NA)



# Start of the userinterface
ui <- fluidPage(
    
    # Application title
    titlePanel("R2Q Maßnahmenkatalog"),
    
    # Sidebar with dropdown list 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Massnahme",
                        label = "Maßnahme",
                        multiple = FALSE, 
                        choices = list_Massnahmen$Massnahmen,
            ),
            actionButton(inputId = "loaddata",label = "Daten laden"),
            actionButton(inputId = "SaveMassnahme",label = "Speichern"),
            
            
            br(),
            br(),
            br(),
            
            textOutput("message1")
        ),
        
        # main Panel
        mainPanel(
            tabsetPanel(
                id = "Eingabe",
                
                
                #################
                #TAB Kurzinformation
                tabPanel("Kurzinformation",
                         
                         br(),
                         HTML("<strong>Achtung: Am besten funktioniert die Anwendung maximiert, um überlappende Wörter vorzubeugen!</strong>"),
                         br(),
                         br(),
                         br(),
                         textInput("titel","Titelname"),
                         br(),
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         #Kurzbeschreibung
                         HTML("<strong>1. Kurzbeschreibung</strong>"),
                         br(),
                         br(),
                         
                         HTML("Folgende Befehle sind im Text möglich:"),
                         br(),
                         HTML("<strong>Kursiv:</strong> *text*"),
                         br(),
                         HTML("<strong>Fettgedruckt:</strong> **text**"),
                         br(),
                         HTML("<strong>Kursiv und Fettgedruckt:</strong> ***text***"),
                         br(),
                         HTML("<strong>Zeilenumbruch:</strong> hierfür muss <strong>zwei mal Enter</strong>, bzw zwei Zeilenumbrüche getätigt werden."),
                         br(),
                         HTML("Ergänzend sind noch weitere Markdown befehle möglich."),
                         br(),
                         
                         textAreaInput("kurzb", label = "",width = "200%"),
                         
                         
                         br(),
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Umsetzungsbeispiel (Foto)
                         
                         strong("2. Umsetzungsbeispiel"),
                         
                         textInput("beschrbsp","Beschriftung"),
                         
                         fileInput("bspfoto", "Upload Bild", accept = c('image/png','image/jpg')),
                         "Entsprechendes Bild im .png oder .jpg Format auswählen",
                         br(),
                         HTML("Letzter Upload:"),
                         textOutput("messageUploadtimebsp"),
                         
                         #textInput("bspfoto", "Umsetzungsbeispiel (Foto)"),
                         #HTML("Bitte <strong>Dateiname</strong> angeben mit der Endung <strong>...</strong>, ebenfalls stellen Sie bitte sicher, dass das Bild das entsprechende <strong>Format</strong> hat"),
                         br(),
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         #Ressource
                         checkboxGroupInput("cbgRessource", "3. Ressource", FALSE,
                                            choices = ressourcen,
                         ),
                         
                         
                         
                         
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         #Wirkung und Funktion
                         strong("4. Wirkung und Funktion"),
                         fluidRow(
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgniederschlagswasser", "Niederschlagswasser", 
                                                        choices = wifuNiederschlagswasser,
                                     )
                             ),
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgbaustoffe", "Baustoffe", 
                                                        choices = wifuBaustoffe,
                                                        
                                                        
                                     )
                                     
                             ),
                             
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgflaeche", "Fläche", 
                                                        choices = wifuFlaeche,
                                                        )
                                     
                             ),
                             
                             
                             
                         ),
                         fluidRow(
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgschmutzwasser", "Schmutzwasser", 
                                                        choices = wifuSchmutzwasser,
                                                        
                                     )
                             ),
                             
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgenergie", "Energie", 
                                                        choices = wifuEnergie,
                                                        
                                                        
                                     )
                                     
                             ),
                             
                         ),
                         
                         
                         
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         #Anwendungsebene
                         checkboxGroupInput("cbgAnwendungsebene", "5. Anwendungsebene", choices = anwendungsebenen),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Flächenbedarf
                         strong("6. Flächenbedarf"),
                         textInput("flaechenbedEW", "spezifische Fläche (m²/EW)"),
                         textInput("flaechenbedEinheit", "Einheit für den spezifischen Flächenbedarf (m²/XX)"),
                         as.character("Bitte nur eine Einheit für XX eingeben!"),
                         br(),
                         br(),
                         textInput("flaechenbedXX", "spezifische Fläche (m²/XX)"),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         #Nutzungsdauer
                         fluidRow(
                             column(width = 4,
                                    strong("7. Nutzungsdauer (Jahre)"),
                                    textInput("nutzdmin", "min")
                             ),
                             
                             column(width = 4,
                                    br(),
                                    textInput("nutzdmax", "max")
                             ),
                             
                             column(width = 4,
                                    br(),
                                    textInput("nutzdueblich", "üblich")
                             ),
                             
                             column(width = 4,
                                    br(),
                             )
                         ),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Entwicklungsstand
                         checkboxGroupInput("cbgentwicklungsstand", "8. Entwicklungsstand", choices = entwicklungsstaende),
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         
                         textInput("hinw1", "Hinweis für Anwendungsebene, Flächenbedarf, Nutzungsdauer und Entwicklungsstand"),
                         
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(), 
                         
                ),
                
                
                
                
                
                
                
                
                #################
                #TAB Detailinformation
                
                tabPanel("Detailinformation",
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),# br for Horizontal Space (empty line)
                         
                         #Funktionsbeschreibung und Aufbau
                         HTML("<strong>9. Funktionsbeschreibung und Aufbau</strong>"),
                         br(),
                         br(),
                         HTML("Folgende Befehle sind im Text möglich:"),
                         br(),
                         HTML("<strong>Kursiv:</strong> *text*"),
                         br(),
                         HTML("<strong>Fettgedruckt:</strong> **text**"),
                         br(),
                         HTML("<strong>Kursiv und Fettgedruckt:</strong> ***text***"),
                         br(),
                         HTML("<strong>Zeilenumbruch:</strong> hierfür muss <strong>zwei mal Enter</strong>, bzw zwei Zeilenumbrüche getätigt werden."),
                         br(),
                         HTML("Ergänzend sind noch weitere Markdown befehle möglich."),
                         br(),
                         
                         textAreaInput("funktiontxt", label = "", width = "200%"),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Systemskizze
                         
                         strong("10. Systemskizze"),
                         
                         textInput("beschrsys","Beschriftung"),
                         
                         fileInput("sysskizze", "Upload Bild", accept = c('image/png','image/jpg')),
                         "Entsprechendes Bild im .png oder .jpg Format auswählen",
                         br(),
                         HTML("Letzter Upload:"),
                         textOutput("messageUploadtimesys"),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Planung, Bemessung und rechtliche Aspekte
                         textAreaInput("planbemtxt", label = "11. Planung, Bemessung und rechtliche Aspekte", width = "200%"),
                         
                         br(),
                         
                         wellPanel(
                             rHandsontableOutput("planbemtab"),
                         ),
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         #Aufwand und Kosten
                         strong("12. Aufwand und Kosten"),
                         br(),
                         br(),
                         textAreaInput("aufwandtxt", label = "Fließtext", width = "200%"),
                         
                         br(),
                         strong("Investitionskosten 1 in €/XX"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("inv1e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("inv1min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("inv1max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("inv1ueblich", "Üblich")
                             )
                         ),
                         
                         strong("Investitionskosten 2 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("inv2e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("inv2min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("inv2max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("inv2ueblich", "Üblich")
                             )
                         ),
                         
                         strong("Investitionskosten 3 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("inv3e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("inv3min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("inv3max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("inv3ueblich", "Üblich")
                             )
                         ),
                         
                         
                         strong("Investitionskosten 4 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("inv4e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("inv4min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("inv4max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("inv4ueblich", "Üblich")
                             )
                         ),
                         
                         strong("Investitionskosten 5 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("inv5e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("inv5min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("inv5max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("inv5ueblich", "Üblich")
                             )
                         ),
                         
                         HTML("----------------------------------------------------------"),
                         br(),
                         
                         strong("Betriebskosten 1 in €/XX"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("bet1e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("bet1min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("bet1max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("bet1ueblich", "Üblich")
                             )
                         ),
                         
                         
                         strong("Betriebskosten 2 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("bet2e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("bet2min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("bet2max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("bet2ueblich", "Üblich")
                             )
                         ),
                         
                         
                         strong("Betriebskosten 3 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("bet3e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("bet3min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("bet3max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("bet3ueblich", "Üblich")
                             )
                         ),
                         
                         
                         strong("Betriebskosten 4 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("bet4e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("bet4min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("bet4max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("bet4ueblich", "Üblich")
                             )
                         ),
                         
                         strong("Betriebskosten 5 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    textInput("bet5e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    textInput("bet5min", "min")
                             ),
                             
                             column(width = 3,
                                    textInput("bet5max", "max")
                             ),
                             
                             column(width = 3,
                                    textInput("bet5ueblich", "Üblich")
                             )
                         ),
                         
                         br(),
                         br(),
                         textAreaInput("kosten_hinweis", label="Hinweis", width = "200%"),
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         #Weitergehende Hinweise
                         
                         
                         #Fließtext + freie anzahl von Tabellen Zeilen  mit Parameter Spalte und Werte Spalte
                         
                         strong("13. Weitergehende Hinweise"),
                         
                         
                         br(),
                         br(),
                         textAreaInput("whinwtxt", label="Fließtext", width = "200%"),
                         
                         wellPanel(
                             rHandsontableOutput("whinw"),
                         ),
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         
                         
                         
                         
                         #Ressourcenübergreifende Aspekte
                         strong("14. Ressourcenübergreifende Aspekte"),
                         fluidRow(
                             column( width = 4,
                                     br(),
                                     strong("Synergien:"),
                                     br(),
                                     br(),
                                     textAreaInput("synniederschlagswasser", "Niederschlagswasser", width = "100%", height = "70%" ),
                                     textAreaInput("synschmutzwasser", "Schmutzwasser", width = "100%", height = "70%" ),
                                     textAreaInput("synbaustoffe", "Baustoffe", width = "100%", height = "70%" ),
                                     textAreaInput("synenergie", "Energie", width = "100%", height = "70%" ),
                                     textAreaInput("synflaeche", "Fläche", width = "100%", height = "70%" ),
                                     textAreaInput("synoekobilanz", "Ökobilanz", width = "100%", height = "70%" ),
                                     
                                     #textInput("synniederschlagswasser", "Niederschlagswasser"),
                                     #textInput("synschmutzwasser", "Schmutzwasser"),
                                     #textInput("synbaustoffe", "Baustoffe"),
                                     #textInput("synenergie", "Energie"),
                                     #textInput("synflaeche", "Fläche"),
                                     #textInput("synoekobilanz", "Ökobilanz"),
                             ),
                             
                             column( width = 4,
                                     br(),
                                     strong("Zielkonflikte:"),
                                     br(),
                                     br(),
                                     textAreaInput("konfniederschlagswasser", "Niederschlagswasser", width = "100%", height = "70%" ),
                                     textAreaInput("konfschmutzwasser", "Schmutzwasser", width = "100%", height = "70%" ),
                                     textAreaInput("konfbaustoffe", "Baustoffe", width = "100%", height = "70%" ),
                                     textAreaInput("konfenergie", "Energie", width = "100%", height = "70%" ),
                                     textAreaInput("konfflaeche", "Fläche", width = "100%", height = "70%" ),
                                     textAreaInput("konfoekobilanz", "Ökobilanz", width = "100%", height = "70%" ),
                                     
                                     #textInput("konfniederschlagswasser", "Niederschlagswasser"),
                                     #textInput("konfschmutzwasser", "Schmutzwasser"),
                                     #textInput("konfbaustoffe", "Baustoffe"),
                                     #textInput("konfenergie", "Energie"),
                                     #textInput("konfflaeche", "Fläche"),
                                     #textInput("konfoekobilanz", "Ökobilanz"),
                             )
                             
                         ),
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         
                         
                         #Platzhalter für Kombinationsmöglichkeiten 
                         #mit Dropdownmenü auf andere Maßnahmen verweisen
                         br(),
                         
                         
                         strong("15. Kombinationsmöglichkeiten"),
                         
                         fluidRow(
                             column(width = 4,
                                 br(),
                         
                                 selectInput(inputId = "selkombi1",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi2",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi3",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi4",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi5",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi6",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi7",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi8",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi9",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 
                                 selectInput(inputId = "selkombi10",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                             ),
                            column(width = 4,
                                br(),
                         
                                 selectInput(inputId = "selkombi11",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi12",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi13",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi14",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi15",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi16",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi17",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi18",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi19",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                                 
                                 selectInput(inputId = "selkombi20",
                                             label = "",
                                             multiple = FALSE, 
                                             choices = massnahmen_dropdown,
                                             selected = NA),
                            ),
                         ),
                         
                         
                         #textAreaInput("kombis", label = "13. Kombinationsmöglichkeiten",value = selected_row$kombis ,width = "200%"),
                         #as.character("Aufzählung sinnvoller/möglicher Maßnahmenkombinationen"),
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         
                         
                         
                         #Vor- und Nachteile
                         
                         
                         strong("16. Vor- und Nachteile"),
                         br(),
                         
                         
                         wellPanel(
                             rHandsontableOutput("vornach"),
                         ),
                         
                         
                         
                         #fluidRow(
                         # column( width = 4,
                         #        br(),
                         #     strong("Vorteile:"),
                         #     br(),
                         #    br(),
                         #   textInput("vort1", "Vorteil 1"),
                         #  textInput("vort2", "Vorteil 2"),
                         # textInput("vort3", "Vorteil 3"),
                         # textInput("vort4", "Vorteil 4"),
                         #textInput("vort5", "Vorteil 5"), 
                         #   ),
                         #  
                         # column( width = 4,
                         #        br(),
                         #       strong("Nachteile:"),
                         #      br(),
                         #     br(),
                         #    textInput("nacht1", "Nachteil 1"),
                         #   textInput("nacht2", "Nachteil 2"),
                         #  textInput("nacht3", "Nachteil 3"),
                         # textInput("nacht4", "Nachteil 4"),
                         #textInput("nacht5", "Nachteil 5"), 
                         #),
                         
                         # ),
                         
                         
                         
                         
                         
                         HTML("_____________________________________________________________________________________________"),
                         br(),
                         
                         
                         #Fallbeispiele
                         
                         strong("17. Fallbeispiele"),
                         br(),
                         
                         
                         wellPanel(
                             rHandsontableOutput("fallbsp"),
                         ),
                         
                         
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                ),
                
                
                
                #################                
                
                
                #TAB Bewertung
                tabPanel("Bewertung"
                         
                ),
                
                
                #################                
                
                
                
                
                
                
                #TAB Vorschau
                tabPanel("Vorschau",
                         
                         br(),
                         br(),
                         #strong("Noch in Arbeit"),
                         htmlOutput("loading"),
                         br(),
                         actionButton(inputId = "viewpdf",label = "Vorschau anzeigen"),
                         br(),
                         br(),
                         add_busy_bar(color = "#52f32b"), #Hex Color Corde
                         br(),
                         br(),
                         
                         
                         htmlOutput("vorschaupdf"),
                         
                         
                         
                         
                         
                         #renderDataTable(expr = as.data.frame(Daten)),
                         
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         
                         #################
                         
                )
            )
        )
    )
)

#End of Userinterface
########################################################################################################################################################








server <- function(input, output, session) {
    
    
    
    
    
    #Vorschau / preview for the pdfs
    
    observeEvent(input$viewpdf, {
        
        output$vorschaupdf <-  renderText("...")
        
        namePDF <- stringi::stri_replace_all_fixed(
            input$Massnahme, 
            c("ä", "ö", "ü", "Ä", "Ö", "Ü"," "), 
            c("ae", "oe", "ue", "Ae", "Oe", "Ue", "_"), 
            vectorize_all = FALSE
        )
        #system("whoami >> /tmp/daschreibtderdashin.txt")
        pdfOutPath <- "/home/shiny/r2q_app/Katalog/"
        #damit PDF-Ausgabe mit Iframe funktioniert müssen Dateien in den www-Ordner vom Workingdirectory                
        syscommand <- paste0("java -jar /home/shiny/R2QPdfGen/target/r2q-pdf-gen-0.3.0-jar-with-dependencies.jar ",pdfOutPath,namePDF,".pdf ", as.character(subset(list_Massnahmen, Massnahmen == input$Massnahme)[1,2]))
        
        system(syscommand)
        file.copy(str_c(pdfOutPath,namePDF,".pdf"), str_c("./www/",namePDF,".pdf"),overwrite = TRUE)
        
        output$vorschaupdf <- renderText({
            return(paste('<iframe style="height:600px; width:100%" src="', str_c(namePDF,".pdf"), '"></iframe>', sep = ""))
        })
    })
    
    
    
    
    
    
    #load data process
    #The following observeEvent loads all data that is saved in the database for the selected technology to the UI
    
    observeEvent(input$loaddata, {
        
        mid <- subset(list_Massnahmen, Massnahmen == input$Massnahme)[1,2]
        
        con = getcon()
        
        #dbSendQuery(con, "SET CHARACTER SET utf8mb4;")
        #dbExecute(con, "SET CHARACTER SET utf8mb4;")
        loadedTable <- dbGetQuery(con, str_c("SELECT * FROM massnahmendaten WHERE massnahme_id = ", as.character(mid)))
        #Encoding(loadedTable) <- "UTF-8"
        dbDisconnect(con)
        
        save1 <- data.frame(loadedTable)
        save1 <- add_column(save1, id = "id", .after = 0)
        save1 <- add_column(save1, row = "bla", .after = 0)
        #save1 <- read.csv2(str_c("./Massnahmen/",input$Massnahme,".csv"))
        #massnahme <- as.character(input$Massnahme) 
        
        massnahme <- subset(list_Massnahmen, id == save1[1,3])[1,1]
        
        datamassnahme <- save1
        
        
        #Function for filling a TextArea  with a Value 
        
        wertInTextArea <- function(wert, box){
            if(is.na(wert)||wert=="NA"){
                updateTextInput(session, box, value = "")
            } else {
                updateTextAreaInput(session, box, value = as.character(wert))
            }
        }
        
        #Function for filling a TextInput  with a Value 
        wertInTextInput <- function(wert, box){
            if(is.na(wert)||wert=="NA"){
                updateTextInput(session, box, value = "")
            } else {
                updateTextInput(session, box, value = as.character(wert))
            }
        }
        
        #Function for replacing "NA" string with empty string
        clean <- function(s) {
            if(is.na(s)){
                ""
            }else if(s == "NA"){
                ""
            }
            s
        }
        
        #Function for filtering the Database. Overall the Database is structured in 3 layers to describe a position of a Value.
        # (Ebene = layer)
        # If just 1 layer is needed then the function "werte1" need to be used
        # If 2 layers are needed then the function "werte2" needs to be used
        # If 3 layers are needed then the function "werte3" needs to be used
        # The input for e1, e2, e3 is String
        werte1 <- function(e1){
            clean(subset(datamassnahme, ebene1 == e1)[1,7])
        }
        
        werte2 <- function(e1, e2){
            clean(subset(datamassnahme, ebene1 == e1 & ebene2 == e2)[1,7])
        }
        
        werte3 <- function(e1, e2, e3){
            clean(subset(datamassnahme, ebene1 == e1 & ebene2 == e2 & ebene3 == e3)[1,7])
        }

        #helping function that expands the werte1 2 and 3 function. It can be used universal for filtering the values without needing to change the function based on the amount of used layers.
        wertEn <- function(ebenen) {
            if(length(ebenen) == 1){
                werte1(ebenen[1])
            }else if(length(ebenen) == 2){
                werte2(ebenen[1], ebenen[2])
            }else if(length(ebenen) == 3){
                werte3(ebenen[1], ebenen[2], ebenen[3])
            }
        }
        
        #here starts the actual loading of the values into the textareas, textinputs, dropdown lists ...
        
        #Titel
        
        wertInTextInput(werte1("Titel"), "titel")
        
        #Kurzbeschreibung
        
        wertInTextArea(werte1("Kurzbeschreibung"), "kurzb")
        
        #Umsetzungsbeispiel
        
        umsetzungbspBild <<- werte2("Umsetzungsbeispiel", "Bild")
        umsetzungbspUptime <<- werte2("Umsetzungsbeispiel", "uptime")
        
        wertInTextInput(werte2("Umsetzungsbeispiel","Beschriftung"), "beschrbsp")
        
        namePNG <- stringi::stri_replace_all_fixed(
            input$Massnahme, 
            c("ä", "ö", "ü", "Ä", "Ö", "Ü"," "), 
            c("ae", "oe", "ue", "Ae", "Oe", "Ue", "_"), 
            vectorize_all = FALSE
        )
        
        # Function for giving information on when or if the last Image was updated
        if (is.na(werte2("Umsetzungsbeispiel","uptime"))||werte2("Umsetzungsbeispiel","uptime")=="NA") {
            output$messageUploadtimebsp <- renderText("keine Information")
        } else {output$messageUploadtimebsp <- renderText(werte2("Umsetzungsbeispiel","uptime"))}
        
        # Query if input for checkboxes is 0 , NA or empty -> then checkbox unckecked
        strIsFalse <- function(w){
            w != "0" && !is.na(w) && w != "NA" && w != ""
        }
        
        
        #function for setting the selected checkboxes on ckecked
        cbGroup = function(ebenen, choices, cbg){
            sel <- c() # Puffer für Wahrheitswerte der Ressourcen Checkboxen (cb) / buffer for truth value
            
            istCbAn <- function(cb){ # if the value for the checkbox is 1 then the name of the checkbox is appended to the list. 
                                    #If a name of a ckeckbox is appended, then this specific checkbox gets checked.
                w <- wertEn(append(ebenen, cb))
                if(strIsFalse(w)){
                    sel <<- append(sel, cb)
                }
            }
            
            for(c in choices){
                istCbAn(c)
            }
            if(length(sel) == 0){
                sel = ""
            }
            updateCheckboxGroupInput(session, cbg, selected = sel )
        }
        
        #Ressourcen
        
        cbGroup(c("Ressource"), ressourcen, "cbgRessource")
        
        #Wirkung und Funkion
        #Niederschlagswasser
        
        cbGroup(c("Wirkung/Funktion", "Niederschlagswasser"), wifuNiederschlagswasser, "cbgniederschlagswasser")
        
        #Baustoffe
        
        cbGroup(c("Wirkung/Funktion", "Baustoffe"), wifuBaustoffe, "cbgbaustoffe")
        
        #Fläche
        
        cbGroup(c("Wirkung/Funktion", "Fläche"), wifuFlaeche, "cbgflaeche")
        
        #Schmutzwasser
        
        cbGroup(c("Wirkung/Funktion", "Schmutzwasser"), wifuSchmutzwasser, "cbgschmutzwasser")
        
        #Energie
        
        cbGroup(c("Wirkung/Funktion", "Energie"), wifuEnergie, "cbgenergie")

        #Anwendungsebene
        
        cbGroup(c("Anwendungsebene"), anwendungsebenen, "cbgAnwendungsebene")
        
        
        #Flächenbedarf
        
        wertInTextInput(werte2("Flächenbedarf", "m²/EW"), "flaechenbedEW")
        wertInTextInput(werte2("Flächenbedarf", "XX"), "flaechenbedEinheit")
        wertInTextInput(werte2("Flächenbedarf", "m²/XX"), "flaechenbedXX")
        
        
        #Nutzungsdauer
        
        wertInTextInput(werte2("Nutzungsdauer", "min"), "nutzdmin")
        wertInTextInput(werte2("Nutzungsdauer", "max"), "nutzdmax")
        wertInTextInput(werte2("Nutzungsdauer", "üblich"), "nutzdueblich")
        
        
        #Entwicklungsstand
        
        cbGroup(c("Entwicklungsstand"), entwicklungsstaende, "cbgentwicklungsstand")
        
        
        #Hinweis für Anwendungsebene, Flächenbedarf, Nutzungsdauer und Entwicklungsstand
        
        wertInTextInput(werte2("Sammelhinweis", "Hinweis"), "hinw1")
        
        #Funktionsbeschreibung  
        
        wertInTextArea(werte1("Funktionsbeschreibung und Aufbau"), "funktiontxt")
        
        #Systemskizze
        
        systemskizzeBild <<- werte2("Systemskizze", "Bild")
        systemskizzeUptime <<- werte2("Systemskizze", "uptime")
        
        wertInTextInput(werte2("Systemskizze", "Beschriftung"), "beschrsys")
        
        namePNG <- stringi::stri_replace_all_fixed(
            input$Massnahme, 
            c("ä", "ö", "ü", "Ä", "Ö", "Ü"," "), 
            c("ae", "oe", "ue", "Ae", "Oe", "Ue","_"), 
            vectorize_all = FALSE
        )
        
        
        if (is.na(werte2("Systemskizze","uptime"))||werte2("Systemskizze","uptime")=="NA") {
            output$messageUploadtimesys <- renderText("keine Information")
        } else {output$messageUploadtimesys <- renderText(werte2("Systemskizze","uptime"))}
        
        
        #Planung, Bemessung und rechtliche Aspekte
        
        wertInTextInput(werte2("Planung, Bemessung und rechtliche Aspekte", "Fließtext"), "planbemtxt")
        
        #Tabelle Planbem
        
        
        DF4 <- tibble(Norm = c("","","","",""), Titel = c("","","","",""))
        
        DF4[1,1] = werte2("Planung, Bemessung und rechtliche Aspekte", "Normen/Regelwerke1")
        DF4[2,1] = werte2("Planung, Bemessung und rechtliche Aspekte", "Normen/Regelwerke2")
        DF4[3,1] = werte2("Planung, Bemessung und rechtliche Aspekte", "Normen/Regelwerke3")
        DF4[4,1] = werte2("Planung, Bemessung und rechtliche Aspekte", "Normen/Regelwerke4")
        DF4[5,1] = werte2("Planung, Bemessung und rechtliche Aspekte", "Normen/Regelwerke5")
        
        DF4[1,2] = werte2("Planung, Bemessung und rechtliche Aspekte", "Titel/Inhalt1")
        DF4[2,2] = werte2("Planung, Bemessung und rechtliche Aspekte", "Titel/Inhalt2")
        DF4[3,2] = werte2("Planung, Bemessung und rechtliche Aspekte", "Titel/Inhalt3")
        DF4[4,2] = werte2("Planung, Bemessung und rechtliche Aspekte", "Titel/Inhalt4")
        DF4[5,2] = werte2("Planung, Bemessung und rechtliche Aspekte", "Titel/Inhalt5")
        
        output$planbemtab <- renderRHandsontable({rhandsontable(DF4, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        #Aufwand und Kosten
        
        wertInTextInput(werte2("Aufwand und Kosten", "Fließtext"), "aufwandtxt")
        
        for(i in 1:5){
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Investitionskosten", as.character(i)), "Einheit"), paste0("inv", as.character(i) ,"e"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Investitionskosten", as.character(i)), "min"), paste0("inv", as.character(i) ,"min"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Investitionskosten", as.character(i)), "max"), paste0("inv", as.character(i) ,"max"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Investitionskosten", as.character(i)), "üblich"), paste0("inv", as.character(i) ,"ueblich"))
        }
        
        for(i in 1:5){
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Betriebskosten", as.character(i)), "Einheit"), paste0("bet", as.character(i) ,"e"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Betriebskosten", as.character(i)), "min"), paste0("bet", as.character(i) ,"min"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Betriebskosten", as.character(i)), "max"), paste0("bet", as.character(i) ,"max"))
            wertInTextInput(werte3("Aufwand und Kosten", paste0("Betriebskosten", as.character(i)), "üblich"), paste0("bet", as.character(i) ,"ueblich"))
        }
        
        wertInTextInput(werte2("Aufwand und Kosten", "Hinweis"), "kosten_hinweis")
        
        #Weitergehende Hinweise
        
        wertInTextInput(werte2("Weitergehende Hinweise", "Fließtext"), "whinwtxt")
        
        DF1 <- tibble(Parameter = c("","","","","","","","","","","","","","","","","","","",""), Wert = c("","","","","","","","","","","","","","","","","","","",""))
        
        for(i in 1:20){
            DF1[i,1] = werte3("Weitergehende Hinweise", "Parameter", as.character(i))
            DF1[i,2] = werte3("Weitergehende Hinweise", "Wert", as.character(i))
        }
        
        output$whinw <- renderRHandsontable({rhandsontable(DF1, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        #Ressourcenübergreifende Aspekte
        
        synergien <- function(e3, ti){
            wertInTextArea(werte3("Ressourcenübergreifende Aspekte", "Synergien", e3), ti)
        }
        
        synergien("Niederschlagswasser", "synniederschlagswasser")
        synergien("Schmutzwasser", "synschmutzwasser")
        synergien("Baustoffe", "synbaustoffe")
        synergien("Energie", "synenergie")
        synergien("Fläche", "synflaeche")
        synergien("Ökobilanz", "synoekobilanz")
        
        zielkonflikte <- function(e3, ti){
            wertInTextArea(werte3("Ressourcenübergreifende Aspekte", "Zielkonflikte", e3), ti)
        }
        
        zielkonflikte("Niederschlagswasser", "konfniederschlagswasser")
        zielkonflikte("Schmutzwasser", "konfschmutzwasser")
        zielkonflikte("Baustoffe", "konfbaustoffe")
        zielkonflikte("Energie", "konfenergie")
        zielkonflikte("Fläche", "konfflaeche")
        zielkonflikte("Ökobilanz", "konfoekobilanz")
        
        #Kombinationsmöglichkeiten

        wertInSelectInput <- function(wert, box){
            if(is.na(wert)||wert=="NA"){
                updateSelectInput(session, box, selected = "NA")
            } else {
                updateSelectInput(session, box, selected = as.character(wert))
            }
        }
        
        for(i in 1:20){
            wertInSelectInput(werte2("Kombinationsmöglichkeiten", as.character(i)), paste0("selkombi", as.character(i)))
        }
        
        #Vor- und Nachteile
        
        DF2 <- tibble(Vorteile = c("","","","","","","","","",""), Nachteile = c("","","","","","","","","",""))
        
        for(i in 1:10){
            DF2[i,1] = werte3("Vor- und Nachteile", "Vorteile", as.character(i))
            DF2[i,2] = werte3("Vor- und Nachteile", "Nachteile", as.character(i))
        }
        
        output$vornach <- renderRHandsontable({rhandsontable(DF2, useTypes = TRUE, stretchH = "all")
        })
        
        #Fallbeispiele
        
        
        DF3 <- tibble(Projektname = c("","",""), Stadt = c("","",""), Land = c("","",""), Erläuterung = c("","",""))
        
        for(i in 1:3){
            DF3[i,1] = werte3("Fallbeispiele", as.character(i), "Projektname")
            DF3[i,2] = werte3("Fallbeispiele", as.character(i), "Stadt")
            DF3[i,3] = werte3("Fallbeispiele", as.character(i), "Land")
            DF3[i,4] = werte3("Fallbeispiele", as.character(i), "Erläuterung")
        }
        
        output$fallbsp <- renderRHandsontable({rhandsontable(DF3, useTypes = TRUE, stretchH = "all")
        })
        output$message1 <- renderText({"Maßnahme wurde geladen."})
    })
    
    ###########################################################################################################  
    
    
    
    #Tabellen /tables
    
    #Tabelle für Planung, Bemessung und rechtliche Aspekte
    #the following code is for implementing the tables in the ui
    
    DF4 <- tibble(Norm_Regelwerk_Gesetz = c("","","","",""), Titel = c("","","","",""))
    
    observe({
        if (!is.null(input$planbemtab)) {  
            values[["previous"]] <- isolate(values[["DF4"]])
            DF4 = hot_to_r(input$planbemtab)
        } else {
            if (is.null(values[["DF4"]]))  
                DF4 <- DF4
            else
                DF4 <- values[["DF4"]]  
        }
        values[["DF4"]] <- DF4
    })
    
    
    output$planbemtab <- renderRHandsontable({
        DF4 <- values[["DF4"]]
        if (!is.null(DF4))
            rhandsontable(DF4, useTypes = TRUE, stretchH = "all")
    })
    
    
    #Tabelle für Weitergehende Hinweise
    
    values <- reactiveValues()
    
    DF1 <- tibble(Parameter = c("","","","","","","","","","","","","","","","","","","",""), Wert = c("","","","","","","","","","","","","","","","","","","",""))
    
    
    observe({
        if (!is.null(input$whinw)) {  #wenn schon Daten in der Tabelle sind werden diese unter values[["previous]] abgespeichert
            values[["previous"]] <- isolate(values[["DF1"]])
            DF1 = hot_to_r(input$whinw)
        } else {
            if (is.null(values[["DF1"]]))  #wenn keine Daten unter values DF vorhanden sind, dann bleibt DF gleich
                DF1 <- DF1
            else
                DF1 <- values[["DF1"]]  #wenn Daten unter values DF vorhanden sind, dann werden diese für DF genutzt
        }
        values[["DF1"]] <- DF1
    })
    
    
    output$whinw <- renderRHandsontable({
        DF1 <- values[["DF1"]]
        if (!is.null(DF1))
            rhandsontable(DF1, useTypes = TRUE, stretchH = "all")
        
        
    })
    
    #Tabelle Vor- und Nachteile
    
    DF2 <- tibble(Vorteile = c("","","","",""), Nachteile = c("","","","",""))
    
    observe({
        if (!is.null(input$vornach)) {  #wenn schon Daten in der Tabelle sind werden diese unter values[["previous]] abgespeichert
            values[["previous"]] <- isolate(values[["DF2"]])
            DF2 = hot_to_r(input$vornach)
        } else {
            if (is.null(values[["DF2"]]))  #wenn keine Daten unter values DF vorhanden sind, dann bleibt DF gleich
                DF2 <- DF2
            else
                DF2 <- values[["DF2"]]  #wenn Daten unter values DF vorhanden sind, dann werden diese für DF genutzt
        }
        values[["DF2"]] <- DF2
    })
    
    
    output$vornach <- renderRHandsontable({
        DF2 <- values[["DF2"]]
        if (!is.null(DF2))
            rhandsontable(DF2, useTypes = TRUE, stretchH = "all")
    })
    
    
    
    #Tabelle für Fallbeispiele
    
    
    DF3 <- tibble(Projektname = c("","",""), Stadt = c("","",""), Land = c("","",""), Erläuterung = c("","",""))
    
    observe({
        if (!is.null(input$fallbsp)) { 
            values[["previous"]] <- isolate(values[["DF3"]])
            DF3 = hot_to_r(input$fallbsp)
        } else {
            if (is.null(values[["DF3"]]))  
                DF3 <- DF3
            else
                DF3 <- values[["DF3"]]  
        }
        values[["DF3"]] <- DF3
    })
    
    
    output$fallbsp <- renderRHandsontable({
        DF3 <- values[["DF3"]]
        if (!is.null(DF3))
            rhandsontable(DF3, useTypes = TRUE, stretchH = "all")
    })
    
    
    
    
    
    
    
    
    
    
    #############################################################################################################
    #saving process
    #the following observeEvent is for saving the input of a technology
    observeEvent(input$SaveMassnahme, {
        
        ##Maßnahme auslesen
        massnahme <- as.character(input$Massnahme) 
        
        save1 <- read_excel("R2Q_Datensatz_leer.xlsx")
        save1$Massnahme <- massnahme
        datamassnahme <- save1
        
        if(nrow(isolate(values[["DF1"]])) > nrow(subset(datamassnahme, ebene1 == "Weitergehende Hinweise" & ebene2 == "Parameter"))) {output$message1 <- renderText({"Es konnte nicht gespeichert werden, da für Weitergehende Hinweise mehr als 10 Zeilen vorhanden sind!"})} else {
            
        
            
            
            TextInputToWert <- function(Box,e1,e2="NA",e3="NA") {
                rt <- renderText({Box})
                val <-  rt()
                if (e2=="NA") {
                    if(val=="NA"||is.na(val)){
                        datamassnahme[datamassnahme$ebene1==e1,7] <<- ""
                    } else {
                        datamassnahme[datamassnahme$ebene1==e1,7] <<- as.character(val)
                    }
                } else {
                    if (e3=="NA") {
                        if(val=="NA"||is.na(val)){
                            datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2 ,7] <<- ""
                        } else {
                            datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2 ,7] <<- as.character(val)
                        }
                    } else {
                        if(val=="NA"||is.na(val)){
                            datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2 & datamassnahme$ebene3==e3 ,7] <<- ""
                        } else {
                            datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2 & datamassnahme$ebene3==e3  ,7] <<- as.character(val)
                        }
                    }
                }
            }
            
            CbgInputToWert <- function(Box,e1,e2="NA",e3="NA") {
                InputWert <- data.frame(Box)
                if (e2=="NA") {
                    datamassnahme[datamassnahme$ebene1==e1 ,7] <<- "0"
                    if (nrow(InputWert) == 0 ){} else 
                    {
                        for (i in 1:as.numeric(nrow(InputWert))) {
                            E2Wert <- InputWert[i,1]
                            datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==E2Wert ,7] <<- "1"
                        }
                    }
                } else {
                    if (e3=="NA") {
                        datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2,7] <<- "0"
                        if (nrow(InputWert) == 0 ){} else 
                        {
                            for (i in 1:as.numeric(nrow(InputWert))) {
                                E3Wert <- InputWert[i,1]
                                datamassnahme[datamassnahme$ebene1==e1 & datamassnahme$ebene2==e2 & datamassnahme$ebene3==E3Wert ,7] <<- "1"
                            }
                        }
                    }
                }
                
                
                
            }
            
            #Titel
            
            input$titel %>% 
                TextInputToWert("Titel")
            
            #Kurzbeschreibung
            
            
            input$kurzb %>% 
                TextInputToWert("Kurzbeschreibung")
            
            ##Zeilennummer herausfiltern

            
            #Umsetzungsbeispiel(Foto)
            namePNG <- stringi::stri_replace_all_fixed(
                input$Massnahme, 
                c("ä", "ö", "ü", "Ä", "Ö", "Ü"," "), 
                c("ae", "oe", "ue", "Ae", "Oe", "Ue","_"), 
                vectorize_all = FALSE
            )
            
            param <- input$bspfoto
            
            if (is.null(param)) {TextInputToWert(umsetzungbspBild, "Umsetzungsbeispiel","Bild")} else {
            if (gsub("^.*\\.","",as.character(param$datapath))=="jpg") {
                imagepath <- str_c(file.path("Umsetzungsbeispiele", namePNG),"bsp.jpg")
                if (file.exists(imagepath)){file.remove(imagepath)}
            file.copy(param$datapath, str_c(file.path("./Umsetzungsbeispiele", namePNG),"bsp.jpg"), overwrite = TRUE )
             imagepath %>% 
                TextInputToWert("Umsetzungsbeispiel","Bild")
            } else {
                imagepath <- str_c(file.path("Umsetzungsbeispiele", namePNG),"bsp.PNG")
                if (file.exists(imagepath)){file.remove(imagepath)}
                file.copy(param$datapath, str_c(file.path("./Umsetzungsbeispiele", namePNG),"bsp.PNG"), overwrite = TRUE )
                imagepath %>% 
                    TextInputToWert("Umsetzungsbeispiel","Bild")
                }
            }
            
            input$beschrbsp %>% 
                TextInputToWert("Umsetzungsbeispiel","Beschriftung")
            
            if (is.null(param)) {TextInputToWert(umsetzungbspUptime, "Umsetzungsbeispiel", "uptime")} else {
                as.character(Sys.time()) %>% 
                    TextInputToWert("Umsetzungsbeispiel","uptime")
                
                output$messageUploadtimebsp <- renderText(as.character(Sys.time()))
            }
            
            
            
            
            
            
            
            
            
            #Ressource
            
            input$cbgRessource %>% 
                CbgInputToWert("Ressource")
            
            
            #Wirkung und Funktion
            
            ##Niederschlagswasser
            input$cbgniederschlagswasser %>% 
                CbgInputToWert("Wirkung/Funktion", "Niederschlagswasser")
            ##Baustoffe
            input$cbgbaustoffe %>% 
                CbgInputToWert("Wirkung/Funktion", "Baustoffe")
            ##Fläche
            input$cbgflaeche %>% 
                CbgInputToWert("Wirkung/Funktion", "Fläche")
            ##Schmutzwasser
            input$cbgschmutzwasser %>% 
                CbgInputToWert("Wirkung/Funktion", "Schmutzwasser")
            ##Energie
            input$cbgenergie %>% 
                CbgInputToWert("Wirkung/Funktion", "Energie")
           
            
            
            #Anwendungsebene
            
            input$cbgAnwendungsebene %>% 
                CbgInputToWert("Anwendungsebene")
            
            ##Hinweis
            
            #input$hinw1 %>% 
            #    TextInputToWert("Anwendungsebene", "Hinweis")
            
            #Flächenbedarf
            
            input$flaechenbedEW %>% 
                TextInputToWert("Flächenbedarf", "m²/EW")
            
            input$flaechenbedEinheit %>% 
                TextInputToWert("Flächenbedarf", "XX")
            
            input$flaechenbedXX %>% 
                TextInputToWert("Flächenbedarf", "m²/XX")
            
            
            #input$hinw2 %>% 
            #    TextInputToWert("Flächenbedarf", "Hinweis")
            
            #Nutzungsdauer
            
            input$nutzdmin %>% 
                TextInputToWert("Nutzungsdauer", "min")
            
            input$nutzdmax %>% 
                TextInputToWert("Nutzungsdauer", "max")
            
            input$nutzdueblich %>% 
                TextInputToWert("Nutzungsdauer", "üblich")
            
            
            #input$hinw3 %>% 
            #    TextInputToWert("Nutzungsdauer", "Hinweis")
            
            
            #Entwicklungsstand
            
            input$cbgentwicklungsstand %>% 
                CbgInputToWert("Entwicklungsstand")
            
            #Hinweis für Anwendungsebene, Flächenbedarf, Nutzungsdauer und Entwicklungsstand
            
            input$hinw1 %>% 
                TextInputToWert("Sammelhinweis", "Hinweis")
            
            #######################
            
            #Detailinformation
            #Funktionsbeschreibung und Aufbau
            
            input$funktiontxt %>% 
                TextInputToWert("Funktionsbeschreibung und Aufbau")
            
            #Systemskizze
            namePNG <- stringi::stri_replace_all_fixed(
                input$Massnahme, 
                c("ä", "ö", "ü", "Ä", "Ö", "Ü"," "), 
                c("ae", "oe", "ue", "Ae", "Oe", "Ue","_"), 
                vectorize_all = FALSE
            )
            
            param <- input$sysskizze
            
            if (is.null(param)) {TextInputToWert(systemskizzeBild, "Systemskizze","Bild")} else {
            if (gsub("^.*\\.","",param$datapath)=="jpg") {
                imagepath <- str_c(file.path("./Systemskizzen", namePNG),"sys.jpg")
                if (file.exists(imagepath)){file.remove(imagepath)}
                file.copy(param$datapath, imagepath, overwrite = TRUE )
                imagepath %>% 
                    TextInputToWert("Systemskizze","Bild")
            } else {
                imagepath <- str_c(file.path("./Systemskizzen", namePNG),"sys.PNG")
                if (file.exists(imagepath)){file.remove(imagepath)}
                file.copy(param$datapath, imagepath, overwrite = TRUE )
                imagepath %>% 
                    TextInputToWert("Systemskizze","Bild")
            }
            }
                
            input$beschrsys %>% 
                TextInputToWert("Systemskizze","Beschriftung")
            
            
            if (is.null(param)) {TextInputToWert(systemskizzeUptime, "Systemskizze", "uptime")} else {
                as.character(Sys.time()) %>% 
                    TextInputToWert("Systemskizze","uptime")
                
                output$messageUploadtimesys <- renderText(as.character(Sys.time()))
            }
            
            #param <- as.character(input$sysskizze)
            #nr <- as.numeric(subset(datamassnahme, ebene1 == "Systemskizze")[1,1])
            #if (param == "") {
            #  datamassnahme[nr,7] <- as.character("NA")
            #} else
            #{datamassnahme[nr,7] <- param}
            
            
            #Planung, Bemessung und rechtliche Aspekte
            
            input$planbemtxt %>% 
                TextInputToWert("Planung, Bemessung und rechtliche Aspekte","Fließtext")
            
            
            param <- isolate(values[["DF4"]])
            
            for (i in 1:5) {
                TextInputToWert("NA","Planung, Bemessung und rechtliche Aspekte",str_c("Normen/Regelwerke",as.character(i)))
                TextInputToWert("NA","Planung, Bemessung und rechtliche Aspekte",str_c("Titel/Inhalt",as.character(i)))
            }
            
            for (i in 1:nrow(param)) {
                as.character(param[i,1]) %>%
                    TextInputToWert("Planung, Bemessung und rechtliche Aspekte",str_c("Normen/Regelwerke",as.character(i)))
                as.character(param[i,2]) %>% 
                    TextInputToWert("Planung, Bemessung und rechtliche Aspekte",str_c("Titel/Inhalt",as.character(i)))
            }
            
            
            #Aufwand und Kosten
            
            input$aufwandtxt %>% 
                TextInputToWert("Aufwand und Kosten","Fließtext")
            
            input$inv1e %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten1","Einheit")
            input$inv1min %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten1","min")
            input$inv1max %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten1","max")
            input$inv1ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten1","üblich")
            
            input$inv2e %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten2","Einheit")
            input$inv2min %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten2","min")
            input$inv2max %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten2","max")
            input$inv2ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten2","üblich")
            
            input$inv3e %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten3","Einheit")
            input$inv3min %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten3","min")
            input$inv3max %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten3","max")
            input$inv3ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten3","üblich")
            
            input$inv4e %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten4","Einheit")
            input$inv4min %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten4","min")
            input$inv4max %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten4","max")
            input$inv4ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten4","üblich")
            
            input$inv5e %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten5","Einheit")
            input$inv5min %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten5","min")
            input$inv5max %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten5","max")
            input$inv5ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Investitionskosten5","üblich")
            
            
            input$bet1e %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten1","Einheit")
            input$bet1min %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten1","min")
            input$bet1max %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten1","max")
            input$bet1ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten1","üblich")
            
            input$bet2e %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten2","Einheit")
            input$bet2min %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten2","min")
            input$bet2max %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten2","max")
            input$bet2ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten2","üblich")
            
            input$bet3e %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten3","Einheit")
            input$bet3min %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten3","min")
            input$bet3max %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten3","max")
            input$bet3ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten3","üblich")
            
            input$bet4e %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten4","Einheit")
            input$bet4min %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten4","min")
            input$bet4max %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten4","max")
            input$bet4ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten4","üblich")
            
            input$bet5e %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten5","Einheit")
            input$bet5min %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten5","min")
            input$bet5max %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten5","max")
            input$bet5ueblich %>% 
                TextInputToWert("Aufwand und Kosten","Betriebskosten5","üblich")
            
            input$kosten_hinweis %>%
                TextInputToWert("Aufwand und Kosten", "Hinweis")
            
            
            #Weitergehende Hinweise
            
            input$whinwtxt %>% 
                TextInputToWert("Weitergehende Hinweise","Fließtext")
            
            
            param <- isolate(values[["DF1"]])
            
            for (i in 1:20) {
                TextInputToWert("NA","Weitergehende Hinweise","Parameter",as.character(i))
                TextInputToWert("NA","Weitergehende Hinweise","Wert",as.character(i))
            }
            
            for (i in 1:nrow(param)) {
                as.character(param[i,1]) %>% 
                    TextInputToWert("Weitergehende Hinweise","Parameter",as.character(i))
                as.character(param[i,2]) %>% 
                    TextInputToWert("Weitergehende Hinweise","Wert",as.character(i))
            }
            
            
            #Ressourcenübergreifende Aspekte
            ##Synergien
            
            input$synniederschlagswasser %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Niederschlagswasser")
            input$synschmutzwasser %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Schmutzwasser")
            input$synbaustoffe %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Baustoffe")
            input$synenergie %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Energie")
            input$synflaeche %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Fläche")
            input$synoekobilanz %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Synergien","Ökobilanz")
            
            ##Zielkonflikte
            input$konfniederschlagswasser %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Niederschlagswasser")
            input$konfschmutzwasser %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Schmutzwasser")
            input$konfbaustoffe %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Baustoffe")
            input$konfenergie %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Energie")
            input$konfflaeche %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Fläche")
            input$konfoekobilanz %>% 
                TextInputToWert("Ressourcenübergreifende Aspekte","Zielkonflikte","Ökobilanz")
            
            
            
            
            #Kombinationsmöglichkeiten
            
            input$selkombi1 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","1")
            input$selkombi2 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","2")
            input$selkombi3 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","3")
            input$selkombi4 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","4")
            input$selkombi5 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","5")
            input$selkombi6 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","6")
            input$selkombi7 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","7")
            input$selkombi8 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","8")
            input$selkombi9 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","9")
            input$selkombi10 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","10")
            input$selkombi11 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","11")
            input$selkombi12 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","12")
            input$selkombi13 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","13")
            input$selkombi14 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","14")
            input$selkombi15 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","15")
            input$selkombi16 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","16")
            input$selkombi17 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","17")
            input$selkombi18 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","18")
            input$selkombi19 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","19")
            input$selkombi20 %>% 
                TextInputToWert("Kombinationsmöglichkeiten","20")
            
            
            #Vor- und Nachteile
            
            param <- isolate(values[["DF2"]])
            
            for (i in 1:10) {
                TextInputToWert("NA","Vor- und Nachteile","Vorteile",as.character(i))
                TextInputToWert("NA","Vor- und Nachteile","Nachteile",as.character(i))
            }
            
            for (i in 1:nrow(param)) {
                as.character(param[i,1]) %>% 
                    TextInputToWert("Vor- und Nachteile","Vorteile",as.character(i))
                as.character(param[i,2]) %>% 
                    TextInputToWert("Vor- und Nachteile","Nachteile",as.character(i))
            }
            
            #Fallbeispiele
            
            
            param <- isolate(values[["DF3"]])
            
            for (i in 1:3) {
                TextInputToWert("NA","Fallbeispiele",as.character(i),"Projektname")
                TextInputToWert("NA","Fallbeispiele",as.character(i),"Stadt")
                TextInputToWert("NA","Fallbeispiele",as.character(i),"Land")
                TextInputToWert("NA","Fallbeispiele",as.character(i),"Erläuterung")
            }
            
            for (i in 1:nrow(param)) {
                as.character(param[i,1]) %>% 
                    TextInputToWert("Fallbeispiele",as.character(i),"Projektname")
                as.character(param[i,2]) %>% 
                    TextInputToWert("Fallbeispiele",as.character(i),"Stadt")
                as.character(param[i,3]) %>% 
                    TextInputToWert("Fallbeispiele",as.character(i),"Land")
                as.character(param[i,4]) %>% 
                    TextInputToWert("Fallbeispiele",as.character(i),"Erläuterung")
            }
            
            
            ###################################################
            
            #getting the primary key of the selected technology
            mid <- subset(list_Massnahmen, Massnahmen == input$Massnahme)[1,2]
            
            #filling the primary key into each column of the data frame
            for (i in 1:nrow(datamassnahme)) {
                datamassnahme[i,2] <- as.character(mid) 
            }
            
            save1 <- datamassnahme
            
            
            #connecting to MySQL
            con = getcon()
            dbExecute(con, "SET CHARACTER SET utf8mb4;")
            
            s <- function(wert){
                if(is.na(wert)){
                    "NULL"
                }
                else{
                    ret = gsub("'", "''", as.character(wert))
                    Encoding(ret) <- "UTF-8"
                    ret
                }
            }
            #transferring the data frame to the database
            for(i in 1:nrow(save1)){
                query <- paste0("INSERT INTO massnahmendaten (massnahme_id, ebene1, ebene2, ebene3, wert, werttyp) VALUES (", s(save1[i,2]), ", '", s(save1[i,4]), "', '", s(save1[i,5]), "', '", s(save1[i,6]), "', '", s(save1[i,7]), "', '", s(save1[i,8]),"') ON DUPLICATE KEY UPDATE wert = '", s(save1[i,7]), "';")
                print(query)
                dbExecute(con, query);
            }
            #disconnecting from MySQL
            dbDisconnect(con)
            #Status message for saving
            output$message1 <- renderText({"Maßnahme wurde gespeichert."})
            
        }})
}

# Run the application 
shinyApp(ui = ui, server = server)


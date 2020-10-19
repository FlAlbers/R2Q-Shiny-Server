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
library(rmarkdown)
library(shinycssloaders)
library(shinybusy) 
library(writexl)
#data structure
#conn <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
#Daten <- dbReadTable(conn, "Daten")
#dbDisconnect(conn)
#list_Massnahmen <- Daten$Name %>% unique()
#selected_row <- Daten[2,]


read_excel_allsheets <- function(filename, tibble = TRUE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(tibble) x <- lapply(x, as_tibble)
    names(x) <- sheets
    x
}
save1 <- read_excel_allsheets("R2Q_Datensatz_leer.xlsx")

list_Massnahmen <- read_excel("Massnahmenliste.xlsx")
list_Massnahmen[["Index"]] <- str_pad(list_Massnahmen[["Index"]], 3, pad = "0")
list_Massnahmen <- unite(list_Massnahmen, Massnahmen, Ressource:Name)

massnahmen_dropdown <-append(list_Massnahmen,NA)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("R2Q Maßnahmenkatalog"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Massnahme",
                        label = "Maßnahme",
                        multiple = FALSE, 
                        choices = list_Massnahmen,
            ),
            actionButton(inputId = "loaddata",label = "Daten laden"),
            actionButton(inputId = "SaveMassnahme",label = "Speichern"),
            
            
            br(),
            br(),
            br(),
            
            textOutput("message1")
        ),
        
        # Show a plot of the generated distribution
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
                         
                         
                         #Kurzbeschreibung
                         HTML("<strong>1. Kurzbeschreibung</strong>"),
                         br(),
                         br(),
                         
                         HTML("Folgende Befehle sind im Text möglich:"),
                         br(),
                         HTML("<strong>Fettgedruckt:</strong> **...**"),
                         br(),
                         HTML("<strong>Zeilenumbruch:</strong> hierfür müssen <strong>2 Leerzeichen</strong> an das Zeilenende gesetzt werden! <br> Dabei ist wichtig den Zeilenumbruch auch hier in der Eingabe mit Enter einzufügen."),
                         br(),
                         
                         textAreaInput("kurzb", label = "",width = "200%"),
                         
                         
                         br(),
                         br(),
                         br(),
                         
                         
                         
                         #Umsetzungsbeispiel (Foto)
                         
                         fileInput("bspfoto", "2. Umsetzungsbeispiel (Bild)", accept = c('image/png')),
                         "Entsprechendes Bild im .png Format auswählen",
                         
                         
                         #textInput("bspfoto", "Umsetzungsbeispiel (Foto)"),
                         #HTML("Bitte <strong>Dateiname</strong> angeben mit der Endung <strong>...</strong>, ebenfalls stellen Sie bitte sicher, dass das Bild das entsprechende <strong>Format</strong> hat"),
                         br(),
                         br(),
                         br(),
                         
                         #Ressource
                         checkboxGroupInput("cbgRessource", "3. Ressource", FALSE,
                                            choices = list("Niederschlagswasser", "Schmutzwasser", "Fläche", "Baustoffe", "Energie"),
                         ),
                         
                         
                         br(), 
                         
                         
                         
                         
                         
                         
                         
                         #Wirkung und Funktion
                         strong("4. Wirkung und Funktion"),
                         fluidRow(
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgniederschlagswasser", "Niederschlagswasser", 
                                                        choices = list("Gewässerschutz", "Bodenschutz", "Überflutungsschutz", "Klimaanpassung"),
                                     )
                             ),
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgbaustoffe", "Baustoffe", 
                                                        choices = list("BOM Bill of Material", "Monomaterial", "Einsparung von Primärmaterialien", "Nachwachsender Rohstoff", "Rohstofferhalt", "Rohstoffverfügbarkeit", "Rohstoffaufwand (gesamt)"),
                                                        
                                                        
                                     )
                                     
                             ),
                             
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgflaeche", "Fläche", 
                                                        choices = list("Infrastrukturversorgung", "Nutzungsvielfalt", "Einsparung natürlicher Ressourcen", "Luftreinhaltung", "Biodiversität", "Aufenthalts-/ Freiraumqualität"),                            
                                     )
                                     
                             ),
                             
                             
                             
                         ),
                         fluidRow(
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgschmutzwasser", "Schmutzwasser", 
                                                        choices = list("Gesundheitsvorsorge", "Gewässerschutz", "Trinwassereinsparung", "Nährstoffgewinnung"),
                                                        
                                     )
                             ),
                             
                             column( width = 3,
                                     br(),
                                     checkboxGroupInput("cbgenergie", "Energie", 
                                                        choices = list("Elektrizität", "Wärme", "Brennstoffe", "Erzeugung", "Verteilung", "Verbrauch"),
                                                        
                                                        
                                     )
                                     
                             ),
                             
                         ),
                         
                         br(),
                         
                         
                         
                         
                         
                         
                         
                         
                         #Anwendungsebene
                         checkboxGroupInput("cbgAnwendungsebene", "5. Anwendungsebene", choices = list("Gebäudeebene", "Grundstücksebene", "Quartiersebene")),
                         textInput("hinw1", "Hinweis:"),
                         
                         br(),
                         
                         
                         
                         
                         
                         
                         
                         
                         #Flächenbedarf
                         strong("6. Flächenbedarf"),
                         textInput("flaechenbedEW", "spezifische Fläche (m²/EW)"),
                         textInput("flaechenbedEinheit", "Einheit für den spezifischen Flächenbedarf (m²/XX)"),
                         as.character("Bitte nur eine Einheit für XX eingeben!"),
                         br(),
                         br(),
                         textInput("flaechenbedXX", "spezifische Fläche (m²/XX)"),
                         textInput("hinw2", "Hinweis:"),
                         
                         
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
                                    textInput("hinw3", "Hinweis")
                             )
                         ),
                         
                         
                         br(),
                         
                         
                         
                         
                         
                         #Entwicklungsstand
                         checkboxGroupInput("cbgentwicklungsstand", "8. Entwicklungsstand", choices = list("Stand der Wissenschaft und Technik", "Stand der Technik", "Allgemein annerkannter Stand der Technik")),
                         textInput("hinw4", "Hinweis"),
                         
                         textOutput("test"),
                         
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
                         
                         br(),
                         
                         
                         
                         #Funktionsbeschreibung und Aufbau
                         HTML("<strong>9. Funktionsbeschreibung und Aufbau</strong>"),
                         br(),
                         br(),
                         HTML("Folgende Befehle sind im Text möglich:"),
                         br(),
                         HTML("<strong>Fettgedruckt:</strong> **...**"),
                         br(),
                         HTML("<strong>Zeilenumbruch:</strong> hierfür müssen <strong>2 Leerzeichen</strong> an das Zeilenende gesetzt werden! <br> Dabei ist wichtig den Zeilenumbruch auch hier in der Eingabe mit Enter einzufügen."),
                         br(),
                         
                         
                         textAreaInput("funktiontxt", label = "", width = "200%"),
                         
                         br(), # br for Horizontal Space (empty line)
                         br(),
                         
                         
                         
                         
                         #Systemskizze
                         #textInput("sysskizze", "10. Systemskizze Dateipfad"),
                         #HTML("Bitte <strong>Dateiname</strong> angeben mit der Endung <strong>...</strong>, ebenfalls stellen Sie bitte sicher, dass das Bild das entsprechende <strong>Format</strong> hat"),
                         
                         fileInput("sysskizze", "10. Systemskizze (Bild)", accept = c('image/png')),
                         "Entsprechendes Bild im .png Format auswählen",
                         
                         br(),
                         br(),
                         br(),
                         
                         
                         
                         
                         #Planung, Bemessung und rechtliche Aspekte
                         textAreaInput("planbemtxt", label = "11. Planung, Bemessung und rechtliche Aspekte", width = "200%"),
                         
                         br(),
                         
                         wellPanel(
                             rHandsontableOutput("planbemtab"),
                         ),
                         
                         
                         
                         #Aufwand und Kosten
                         strong("12. Aufwand und Kosten"),
                         br(),
                         br(),
                         textAreaInput("aufwandtxt", label = "Fließtext", width = "200%"),
                         
                         br(),
                         strong("Investitionskosten in €/XX"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    br(),
                                    textInput("inve", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("invmin", "min")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("invmax", "max")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("invueblich", "Üblich")
                             )
                         ),
                         
                         br(),
                         br(),
                         
                         
                         strong("Betriebskosten 1 in €/XX"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    br(),
                                    textInput("bet1e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet1min", "min")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet1max", "max")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet1ueblich", "Üblich")
                             )
                         ),
                         
                         br(),
                         br(),
                         
                         
                         strong("Betriebskosten 2 in €/XX (optional!!!)"),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    br(),
                                    textInput("bet2e", "Einheit für XX")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet2min", "min")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet2max", "max")
                             ),
                             
                             column(width = 3,
                                    br(),
                                    textInput("bet2ueblich", "Üblich")
                             )
                         ),
                         
                         br(),
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
                         
                         
                         
                         br(),
                         br(),
                         
                         
                         
                         
                         
                         
                         #Ressourcenübergreifende Aspekte
                         strong("14. Ressourcenübergreifende Aspekte"),
                         fluidRow(
                             column( width = 4,
                                     br(),
                                     strong("Synergien:"),
                                     br(),
                                     br(),
                                     textInput("synniederschlagswasser", "Niederschlagswasser"),
                                     textInput("synschmutzwasser", "Schmutzwasser"),
                                     textInput("synbaustoffe", "Baustoffe"),
                                     textInput("synenergie", "Energie"),
                                     textInput("synflaeche", "Fläche"),
                                     textInput("synoekobilanz", "Ökobilanz"),
                             ),
                             
                             column( width = 4,
                                     br(),
                                     strong("Zielkonflikte:"),
                                     br(),
                                     br(),
                                     textInput("konfniederschlagswasser", "Niederschlagswasser"),
                                     textInput("konfschmutzwasser", "Schmutzwasser"),
                                     textInput("konfbaustoffe", "Baustoffe"),
                                     textInput("konfenergie", "Energie"),
                                     textInput("konfflaeche", "Fläche"),
                                     textInput("konfoekobilanz", "Ökobilanz"),
                             )
                             
                         ),
                         
                         
                         br(),
                         
                         
                         
                         
                         
                         #Platzhalter für Kombinationsmöglichkeiten 
                         #mit Dropdownmenü auf andere Maßnahmen verweisen
                         br(),
                         
                         
                         strong("15. Kombinationsmöglichkeiten"),
                         
                         
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
                         
                         
                         
                         
                         
                         
                         #textAreaInput("kombis", label = "13. Kombinationsmöglichkeiten",value = selected_row$kombis ,width = "200%"),
                         #as.character("Aufzählung sinnvoller/möglicher Maßnahmenkombinationen"),
                         br(),
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
                         
                         
                         
                         
                         
                         
                         
                         #Fallbeispiele
                         br(),
                         
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
                         strong("Noch in Arbeit"),
                         htmlOutput("loading"),
                         br(),
                         #actionButton(inputId = "viewpdf",label = "Vorschau anzeigen"),
                         br(),
                         br(),
                         add_busy_bar(color = "#52f32b"), #Hex Color Corde
                         br(),
                         br(),
                         
                         
                         #htmlOutput("vorschaupdf"),
                         
                         
                         
                         
                         
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


########################################################################################################################################################





# Define server logic required to draw a histogram


server <- function(input, output, session) {
    
    
    
    
    
    #Vorschau
    
    observeEvent(input$viewpdf, {
        
        #damit PDF-Ausgabe mit Iframe funktioniert müssen Dateien in den www-Ordner vom Workingdirectory                
        outputname <- str_c("../Katalog/",input$Massnahme,".html")
        rmdsource <- str_c("./Markdown/","R2Q_Test.Rmd")
        render(rmdsource, output_file = outputname)
        
        file.copy(str_c("./Katalog/",input$Massnahme,".html"), str_c("./www/",input$Massnahme,".html"))
        
        output$vorschaupdf <- renderText({
            return(paste('<iframe style="height:600px; width:100%" src="', str_c(input$Massnahme,".html"), '"></iframe>', sep = ""))
        })
    })
    
    
    
    
    
    
    #Daten laden
    
    observeEvent(input$loaddata, {
        
        read_excel_allsheets <- function(filename, tibble = TRUE) {
            sheets <- readxl::excel_sheets(filename)
            x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
            if(tibble) x <- lapply(x, as_tibble)
            names(x) <- sheets
            x
        }
        save1 <- read.csv2(str_c("./Massnahmen/",input$Massnahme,".csv"))
        massnahme <- as.character(input$Massnahme) 
        
        datamassnahme <- save1
        
        #Kurzbeschreibung
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kurzbeschreibung" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextAreaInput(session, "kurzb", value = "")
        } else {
            updateTextAreaInput(session, "kurzb", value = as.character(datamassnahme[nr,7]))
        }
        
        
        #Ressourcen
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressource")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Ressource"))) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,5]))
            }
        }
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgRessource", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgRessource", selected = "" )
        }
        
        
        #Wirkung und Funkion
        #Niederschlagswasser
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Niederschlagswasser")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Niederschlagswasser"))) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,6]))
            }
        }
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgniederschlagswasser", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgniederschlagswasser", selected = "" )
        }
        
        
        #Baustoffe
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Baustoffe")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Baustoffe"))) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,6]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgbaustoffe", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgbaustoffe", selected = "" )
        }
        
        
        #Fläche
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Fläche")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Fläche"))) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,6]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgflaeche", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgflaeche", selected = "" )
        }
        
        
        #Schmutzwasser
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Schmutzwasser")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Schmutzwasser"))) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,6]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgschmutzwasser", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgschmutzwasser", selected = "" )
        }
        
        #Energie
        
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Energie")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Energie"))) {
            if(datamassnahme[nr+i-1,7] == "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,6]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgenergie", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgenergie", selected = "" )
        }
        
        
        #Anwendungsebene
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene")[1,1])
        
        for (i in 1:(nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene"))-1)) {
            if(datamassnahme[nr+i-1,7]== "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,5]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgAnwendungsebene", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgAnwendungsebene", selected = "" )
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "hinw1", value = "")
        } else {
            updateTextInput(session, "hinw1", value = as.character(datamassnahme[nr,7]))}
        
        
        #Flächenbedarf
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "m²/EW" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "flaechenbedEW", value = "")
        } else {
            updateTextInput(session, "flaechenbedEW", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "XX" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "flaechenbedEinheit", value = "")
        } else {
            updateTextInput(session, "flaechenbedEinheit", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "m²/XX" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "flaechenbedXX", value = "")
        } else {
            updateTextInput(session, "flaechenbedXX", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "hinw2", value = "")
        } else {
            updateTextInput(session, "hinw2", value = as.character(datamassnahme[nr,7]))}
        
        
        #Nutzungsdauer
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "min" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "nutzdmin", value = "")
        } else {
            updateTextInput(session, "nutzdmin", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "max" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "nutzdmax", value = "")
        } else {
            updateTextInput(session, "nutzdmax", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "üblich" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "nutzdueblich", value = "")
        } else {
            updateTextInput(session, "nutzdueblich", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "hinw3", value = "")
        } else {
            updateTextInput(session, "hinw3", value = as.character(datamassnahme[nr,7]))}
        
        
        #Entwicklungsstand
        
        
        sel <- c()
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand")[1,1])
        
        for (i in 1:(nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand"))-1)) {
            if(datamassnahme[nr+i-1,7] == "1"){
                sel <- append(sel,as.character(datamassnahme[nr+i-1,5]))
            }
        }
        
        if(length(sel != 0)) {
            updateCheckboxGroupInput(session, "cbgentwicklungsstand", selected = sel )
        } else {
            updateCheckboxGroupInput(session, "cbgentwicklungsstand", selected = "" )
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "hinw4", value = "")
        } else {
            updateTextInput(session, "hinw4", value = as.character(datamassnahme[nr,7]))}
        
        
        
        #Funktionsbeschreibung
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Funktionsbeschreibung")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "funktiontxt", value = "")
        } else {
            updateTextInput(session, "funktiontxt", value = as.character(datamassnahme[nr,7]))}
        
        
        
        #Planung, Bemessung und rechtliche Aspekte
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Fließtext")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "planbemtxt", value = "")
        } else {
            updateTextInput(session, "planbemtxt", value = as.character(datamassnahme[nr,7]))}
        
        #Tabelle Planbem
        
        
        DF4 <- tibble(Norm = c("","","","",""), Titel = c("","","","",""))
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Normen/Regelwerke")[1,1])
        
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Normen/Regelwerke"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF4[i,1] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Titel/Inhalt")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Titel/Inhalt"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF4[i,2] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        output$planbemtab <- renderRHandsontable({rhandsontable(DF4, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        #Aufwand und Kosten
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Fließtext")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "aufwandtxt", value = "")
        } else {
            updateTextInput(session, "aufwandtxt", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Investitionskosten" & datamassnahme$Ebene3 == "Einheit" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "inve", value = "")
        } else {
            updateTextInput(session, "inve", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Investitionskosten" & datamassnahme$Ebene3 == "min" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "invmin", value = "")
        } else {
            updateTextInput(session, "invmin", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Investitionskosten" & datamassnahme$Ebene3 == "max" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "invmax", value = "")
        } else {
            updateTextInput(session, "invmax", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Investitionskosten" & datamassnahme$Ebene3 == "üblich" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "invueblich", value = "")
        } else {
            updateTextInput(session, "invueblich", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten1" & datamassnahme$Ebene3 == "Einheit" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet1e", value = "")
        } else {
            updateTextInput(session, "bet1e", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten1" & datamassnahme$Ebene3 == "min" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet1min", value = "")
        } else {
            updateTextInput(session, "bet1min", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten1" & datamassnahme$Ebene3 == "max" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet1max", value = "")
        } else {
            updateTextInput(session, "bet1max", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten1" & datamassnahme$Ebene3 == "üblich" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet1ueblich", value = "")
        } else {
            updateTextInput(session, "bet1ueblich", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten2" & datamassnahme$Ebene3 == "Einheit" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet2e", value = "")
        } else {
            updateTextInput(session, "bet2e", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten2" & datamassnahme$Ebene3 == "min" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet2min", value = "")
        } else {
            updateTextInput(session, "bet2min", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten2" & datamassnahme$Ebene3 == "max" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet2max", value = "")
        } else {
            updateTextInput(session, "bet2max", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Betriebskosten2" & datamassnahme$Ebene3 == "üblich" )[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "bet2ueblich", value = "")
        } else {
            updateTextInput(session, "bet2ueblich", value = as.character(datamassnahme[nr,7]))}
        
        
        
        #Weitergehende Hinweise
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Fließtext")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "whinwtxt", value = "")
        } else {
            updateTextInput(session, "whinwtxt", value = as.character(datamassnahme[nr,7]))}
        
        
        DF1 <- tibble(Parameter = c("","","","","","","","","",""), Wert = c("","","","","","","","","",""))
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF1[i,1] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Wert")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Wert"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF1[i,2] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        output$whinw <- renderRHandsontable({rhandsontable(DF1, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        #Ressourcenübergreifende Aspekte
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Niederschlagswasser")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synniederschlagswasser", value = "")
        } else {
            updateTextInput(session, "synniederschlagswasser", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Schmutzwasser")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synschmutzwasser", value = "")
        } else {
            updateTextInput(session, "synschmutzwasser", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Baustoffe")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synbaustoffe", value = "")
        } else {
            updateTextInput(session, "synbaustoffe", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Energie")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synenergie", value = "")
        } else {
            updateTextInput(session, "synenergie", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Fläche")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synflaeche", value = "")
        } else {
            updateTextInput(session, "synflaeche", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Synergien" & datamassnahme$Ebene3 == "Ökobilanz")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "synoekobilanz", value = "")
        } else {
            updateTextInput(session, "synoekobilanz", value = as.character(datamassnahme[nr,7]))}
        
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Niederschlagswasser")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfniederschlagswasser", value = "")
        } else {
            updateTextInput(session, "konfniederschlagswasser", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Schmutzwasser")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfschmutzwasser", value = "")
        } else {
            updateTextInput(session, "konfschmutzwasser", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Baustoffe")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfbaustoffe", value = "")
        } else {
            updateTextInput(session, "konfbaustoffe", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Energie")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfenergie", value = "")
        } else {
            updateTextInput(session, "konfenergie", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Fläche")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfflaeche", value = "")
        } else {
            updateTextInput(session, "konfflaeche", value = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 == "Zielkonflikte" & datamassnahme$Ebene3 == "Ökobilanz")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateTextInput(session, "konfoekobilanz", value = "")
        } else {
            updateTextInput(session, "konfoekobilanz", value = as.character(datamassnahme[nr,7]))}
        
        
        #Kombinationsmöglichkeiten
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "1")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi1", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi1", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "2")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi2", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi2", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "3")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi3", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi3", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "4")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi4", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi4", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "5")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi5", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi5", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "6")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi6", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi6", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "7")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi7", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi7", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "8")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi8", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi8", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "9")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi9", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi9", selected = as.character(datamassnahme[nr,7]))}
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "10")[1,1])
        if(is.na(datamassnahme[nr,7])){
            updateSelectInput(session, "selkombi10", selected = "NA")
        } else {
            updateSelectInput(session, "selkombi10", selected = as.character(datamassnahme[nr,7]))}
        
        
        
        #Vor- und Nachteile
        
        DF2 <- tibble(Vorteile = c("","","","","","","","","",""), Nachteile = c("","","","","","","","","",""))
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Vorteile")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Vorteile"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF2[i,1] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Nachteile")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Nachteile"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF2[i,2] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        output$vornach <- renderRHandsontable({rhandsontable(DF2, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        #Fallbeispiele
        
        
        DF3 <- tibble(Projektname = c("","",""), Stadt = c("","",""), Land = c("","",""), Erläuterung = c("","",""))
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF3[1,i] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF3[2,i] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF3[3,i] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "4")[1,1])
        for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "4"))) {
            if(is.na(datamassnahme[nr+i-1,7])){} else {
                DF3[4,i] <- as.character(datamassnahme[nr+i-1,7])
                
            }
        }
        
        output$fallbsp <- renderRHandsontable({rhandsontable(DF3, useTypes = TRUE, stretchH = "all")
        })
        
        
        
        
    })
    
    ###########################################################################################################  
    
    
    
    #Tabellen
    
    #Tabelle für Planung, Bemessung und rechtliche Aspekte
    
    
    DF4 <- tibble(Norm = c("","","","",""), Titel = c("","","","",""))
    
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
    
    DF1 <- tibble(Parameter = c("","","","",""), Wert = c("","","","",""))
    
    
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
    #Saving Process
    
    observeEvent(input$SaveMassnahme, {
        
        ##Maßnahme auslesen
        massnahme <- as.character(input$Massnahme) 
        
        save1 <- read_excel("R2Q_Datensatz_leer.xlsx")
        save1$Massnahme <- massnahme
        datamassnahme <- save1
        
        if(nrow(isolate(values[["DF1"]])) > nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter"))) {output$message1 <- renderText({"Es konnte nicht gespeichert werden, da für Weitergehende Hinweise mehr als 10 Zeilen vorhanden sind!"})} else {
            
            
            read_excel_allsheets <- function(filename, tibble = TRUE) {
                sheets <- readxl::excel_sheets(filename)
                x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
                if(tibble) x <- lapply(x, as_tibble)
                names(x) <- sheets
                x
            }
            
            
            
            
            #Kurzbeschreibung
            
            
            
            ##Zeilennummer herausfiltern
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kurzbeschreibung" )[1,1])
            
            ##Wert abspeichern
            param <- as.character(input$kurzb)
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {
                datamassnahme[nr,7] <- param
            }
            
            #Umsetzungsbeispiel(Foto)
            namePNG <- stringi::stri_replace_all_fixed(
                input$Massnahme, 
                c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
                c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
                vectorize_all = FALSE
            )
            param <- input$bspfoto
            if (is.null(param)) {} else {}
            file.copy(param$datapath, str_c(file.path("./Umsetzungsbeispiele", namePNG),"bsp.PNG") )
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Umsetzungsbeispiel" )[1,1])
            
            datamassnahme[nr,7] <- str_c(file.path("../Umsetzungsbeispiele", namePNG),"bsp.PNG")
            
            
            
            #Ressource
            param <- data.frame(input$cbgRessource)
            
            for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Ressource"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressource")[1,1])
                datamassnahme[nr+i-1,7] <- "0"
            }
            
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressource" & datamassnahme$Ebene2 == as.character(param[i,1]) )[1,1])
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            #Wirkung und Funktion
            
            param <- data.frame(input$cbgniederschlagswasser)
            
            for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion")[1,1])
                datamassnahme[nr+i-1,7] <- "0"
            }
            
            
            ##Niederschlagswasser
            
            param <- data.frame(input$cbgniederschlagswasser)
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Niederschlagswasser" & datamassnahme$Ebene3 == as.character(param[i,1]) )[1,1])
                    
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            ##Baustoffe
            param <- data.frame(input$cbgbaustoffe)
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Baustoffe" & datamassnahme$Ebene3 == as.character(param[i,1]) )[1,1])
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            ##Fläche
            param <- data.frame(input$cbgflaeche)
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Fläche" & datamassnahme$Ebene3 == as.character(param[i,1]) )[1,1])
                    
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            
            ##Schmutzwasser
            param <- data.frame(input$cbgschmutzwasser)
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Schmutzwasser" & datamassnahme$Ebene3 == as.character(param[i,1]) )[1,1])
                    
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            
            ##Energie
            
            param <- data.frame(input$cbgenergie)
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Wirkung/Funktion" & datamassnahme$Ebene2 == "Energie" & datamassnahme$Ebene3 == as.character(param[i,1]) )[1,1])
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            
            #Anwendungsebene
            
            param <- data.frame(input$cbgAnwendungsebene)
            
            for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene")[1,1])
                datamassnahme[nr+i-1,7] <- "0"
            }
            
            
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene" & datamassnahme$Ebene2 == as.character(param[i,1]) )[1,1])
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            
            
            
            ##Hinweis
            
            param <- as.character(input$hinw1)
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Anwendungsebene" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
            if (param == ""){datamassnahme[nr,7] <- as.character("NA")} else {
                datamassnahme[nr,7] <- param
            }
            
            
            
            #Flächenbedarf
            param <- as.character(input$flaechenbedEW)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "m²/EW" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            param <- as.character(input$flaechenbedEinheit)  
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "XX" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            param <- as.character(input$flaechenbedXX)  #####XX Variabel lassen
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "m²/XX" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            
            param <- as.character(input$hinw2)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Flächenbedarf" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            
            
            #Nutzungsdauer
            param <- as.character(input$nutzdmin)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "min" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            param <- as.character(input$nutzdmax)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "max" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            param <- as.character(input$nutzdueblich)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "üblich" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            param <- as.character(input$hinw3)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Nutzungsdauer" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            #Entwicklungsstand
            
            param <- data.frame(input$cbgentwicklungsstand)
            for (i in 1:nrow(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand")[1,1])
                datamassnahme[nr+i-1,7] <- "0"
            }
            
            
            if (nrow(param) == 0 ){} else 
            {
                for (i in 1:as.numeric(nrow(param))) {
                    nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand" & datamassnahme$Ebene2 == as.character(param[i,1]) )[1,1])
                    datamassnahme[nr,7] <- "1"
                }
            }
            
            
            
            
            
            
            
            
            
            param <- as.character(input$hinw4)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Entwicklungsstand" & datamassnahme$Ebene2 == "Hinweis" )[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            #######################
            
            
            
            #Detailinformation
            #Funktionsbeschreibung und Aufbau
            
            param <- as.character(input$funktiontxt)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Funktionsbeschreibung und Aufbau")[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            #Systemskizze
            namePNG <- stringi::stri_replace_all_fixed(
                input$Massnahme, 
                c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
                c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
                vectorize_all = FALSE
            )
            param <- input$sysskizze
            if (is.null(param)) {} else {}
            file.copy(param$datapath, str_c(file.path("./Systemskizzen", namePNG),"sys.PNG") )
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Systemskizze" )[1,1])
            
            datamassnahme[nr,7] <- str_c(file.path("./Systemskizzen", namePNG),"sys.PNG")
            
            
            
            
            #param <- as.character(input$sysskizze)
            #nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Systemskizze")[1,1])
            #if (param == "") {
            #  datamassnahme[nr,7] <- as.character("NA")
            #} else
            #{datamassnahme[nr,7] <- param}
            
            
            #Planung, Bemessung und rechtliche Aspekte
            
            param <- as.character(input$planbemtxt)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Fließtext")[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            
            param <- isolate(values[["DF4"]])
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Normen/Regelwerke"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Normen/Regelwerke")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Normen/Regelwerke")[1,1])
                if (param[i,1] == "") {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,1]
                }  
            }
            
            
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Titel/Inhalt"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Titel/Inhalt")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Planung, Bemessung und rechtliche Aspekte" & datamassnahme$Ebene2 == "Titel/Inhalt")[1,1])
                if (param[i,1] == "") {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,2]
                }  
            }
            
            
            
            
            
            
            #Aufwand und Kosten
            
            param <- as.character(input$aufwandtxt)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 == "Fließtext")[1,1])
            if (param == "") {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            param <- as.character(input$inve)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Investitionskosten" & datamassnahme$Ebene3 == "Einheit" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$invmin)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Investitionskosten" & datamassnahme$Ebene3 == "min" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$invmax)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Investitionskosten" & datamassnahme$Ebene3 == "max" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$invueblich)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Investitionskosten" & datamassnahme$Ebene3 == "üblich" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            
            
            param <- as.character(input$bet1e)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten1" & datamassnahme$Ebene3 == "Einheit" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet1min)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten1" & datamassnahme$Ebene3 == "min" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet1max)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten1" & datamassnahme$Ebene3 == "max" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet1ueblich)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten1" & datamassnahme$Ebene3 == "üblich" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            
            
            param <- as.character(input$bet2e)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten2" & datamassnahme$Ebene3 == "Einheit" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet2min)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten2" & datamassnahme$Ebene3 == "min" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet2max)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten2" & datamassnahme$Ebene3 == "max" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$bet2ueblich)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Aufwand und Kosten" & datamassnahme$Ebene2 =="Betriebskosten2" & datamassnahme$Ebene3 == "üblich" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            
            
            
            
            #Weitergehende Hinweise
            
            param <- as.character(input$whinwtxt)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Fließtext")[1,1])
            if (param == "" ) {
                datamassnahme[nr,7] <- as.character("NA")
            } else
            {datamassnahme[nr,7] <- param}
            
            
            
            param <- isolate(values[["DF1"]])
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Parameter")[1,1])
                if (param[i,1] == "") {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,1]
                }  
            }
            
            
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Wert"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Wert")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Weitergehende Hinweise" & datamassnahme$Ebene2 == "Wert")[1,1])
                if (param[i,1] == "") {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,2]
                }  
            }
            
            
            #Ressourcenübergreifende Aspekte
            ##Synergien
            
            param <- as.character(input$synniederschlagswasser)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Niederschlagswasser" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$synschmutzwasser)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Schmutzwasser" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$synbaustoffe)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Baustoffe" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$synenergie)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Energie" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$synflaeche)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Fläche" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$synoekobilanz)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Synergien" & datamassnahme$Ebene3 == "Ökobilanz" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            ##Zielkonflikte
            
            param <- as.character(input$konfniederschlagswasser)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Niederschlagswasser" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$konfschmutzwasser)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Schmutzwasser" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$konfbaustoffe)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Baustoffe" )[1,1])
            if (param == "") {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$konfenergie)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Energie" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$konfflaeche)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Fläche" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            param <- as.character(input$konfoekobilanz)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Ressourcenübergreifende Aspekte" & datamassnahme$Ebene2 =="Zielkonflikte" & datamassnahme$Ebene3 == "Ökobilanz" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            #Kombinationsmöglichkeiten
            
            
            param <- as.character(input$selkombi1)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "1" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi2)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "2" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi3)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "3" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi4)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "4" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi5)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "5" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi6)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "6" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi7)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "7" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi8)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "8" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi9)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "9" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            param <- as.character(input$selkombi10)
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Kombinationsmöglichkeiten" & datamassnahme$Ebene2 == "10" )[1,1])
            if (param == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param
            }
            
            
            
            
            #Vor- und Nachteile
            
            
            param <- isolate(values[["DF2"]])
            
            
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Vorteile"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Vorteile")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Vorteile")[1,1])
                if (param[i,1] == "" ) {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,1]
                }  
            }
            
            
            
            for (i in 1:nrow(subset(datamassnahme,datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Nachteile"))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Nachteile")[1,1])
                datamassnahme[nr+i-1,7] <- as.character("NA")
            }
            
            for (i in 1:as.numeric(nrow(param))) {
                nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Vor- und Nachteile" & datamassnahme$Ebene2 == "Nachteile")[1,1])
                if (param[i,1] == "" ) {datamassnahme[nr+i-1,7] <- as.character("NA")} else
                {
                    datamassnahme[nr+i-1,7] <- param[i,2]
                }  
            }
            
            
            #Fallbeispiele
            
            param <- isolate(values[["DF3"]])
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1" & datamassnahme$Ebene3 == "Projektname" )[1,1])
            if (param[1,1] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[1,1]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1" & datamassnahme$Ebene3 == "Stadt" )[1,1])
            if (param[1,2] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[1,2]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1" & datamassnahme$Ebene3 == "Land" )[1,1])
            if (param[1,3] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[1,3]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "1" & datamassnahme$Ebene3 == "Erläuterung" )[1,1])
            if (param[1,4] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[1,4]
            }
            
            
            
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2" & datamassnahme$Ebene3 == "Projektname" )[1,1])
            if (param[2,1] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[2,1]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2" & datamassnahme$Ebene3 == "Stadt" )[1,1])
            if (param[2,2] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[2,2]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2" & datamassnahme$Ebene3 == "Land" )[1,1])
            if (param[2,3] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[2,3]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "2" & datamassnahme$Ebene3 == "Erläuterung" )[1,1])
            if (param[2,4] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[2,4]
            }
            
            
            
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3" & datamassnahme$Ebene3 == "Projektname" )[1,1])
            if (param[3,1] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[3,1]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3" & datamassnahme$Ebene3 == "Stadt" )[1,1])
            if (param[3,2] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[3,2]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3" & datamassnahme$Ebene3 == "Land" )[1,1])
            if (param[3,3] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[3,3]
            }
            
            
            nr <- as.numeric(subset(datamassnahme, datamassnahme$Ebene1 == "Fallbeispiele" & datamassnahme$Ebene2 == "3" & datamassnahme$Ebene3 == "Erläuterung" )[1,1])
            if (param[3,4] == "" ) {datamassnahme[nr,7] <- as.character("NA")} else
            {
                datamassnahme[nr,7] <- param[3,4]
            }
            
            
            
            
            
            
            
            ############
            
            save1[[massnahme]] <- datamassnahme
            write.csv2(save1[[massnahme]], str_c("./Massnahmen/",input$Massnahme,".csv"), row.names = FALSE)
            #write_xlsx(save1[[massnahme]], str_c("./Massnahmen/",input$Massnahme,".xlsx"))
            output$message1 <- renderText({"Maßnahme wurde gespeichert."})
            
        }})
    
    #####################################
    
    #observeEvent(input$SaveMassnahme, {
    # massnahme <- input$Massnahme
    # finalDF1 <- isolate(values[["DF1"]])
    # write.csv(finalDF1, file="TabelleHinweise.CSV")
    #}
    #)
    
    
    #  DF_NA <- data.frame(Parameter = c("NA", "NA", "NA", "NA", "NA"),
    #                    Wert = c("NA", "NA", "NA", "NA", "NA"),
    #                    stringsAsFactors = FALSE)
    #
    #DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
    #                small = letters[1:10],
    #                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
    #                stringsAsFactors = FALSE)
    #
    #observeEvent(input$SaveMassnahme, {
    #output$test <- renderPrint({ input$Massnahme })
    #})
    #  
    #  
    #  
    #  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)


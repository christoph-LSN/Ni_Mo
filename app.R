#install.packages("DT")
#install.packages("markdown")


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(ggplot2)
#library(plotly)
library(markdown)



ui <- dashboardPage(
  dashboardHeader(title = "Ni-Mo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("NDS-Monitor", tabName = "NDS-Monitor"),
      menuItem("Bevölkerung", tabName = "bevoelkerung",
               menuSubItem("Bevölkerungsentwicklung", tabName = "bevoelkerungsentwicklung"),
               menuSubItem("Bevölkerungsstand", tabName = "bevoelkerungsstand")
      ),
      menuItem("Familie und Beruf", tabName = "familieberuf",
               menuSubItem("Kinderbetreuung", tabName = "kinderbetreuung"),
               menuSubItem("Alleinerziehende", tabName = "alleinerziehende")
      )
      # Weitere Menüpunkte für die restlichen Unterseiten hier hinzufügen
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "NDS-Monitor",
              div(
                h2("Das ist der Niedersachsen-Monitor"),
              #img(src = "NiMo.jpg"),
                h3("Hier ist der zusätzliche Text unter dem Bild.")
              )
      ),
      tabItem(tabName = "bevoelkerungsentwicklung",
              h2("Bevölkerungsentwicklung"),
              fluidRow(
                column(width = 12,
                       tabBox(
                         tabPanel("Tabelle", dataTableOutput("tabelle_bevoelkerung")),
                         tabPanel("Diagramm", plotOutput("diagramm_bevoelkerung")),
                         tabPanel("Karte", leafletOutput("karte_bevoelkerung"))
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("Erläuterungen"),
                       uiOutput("erl_bevoelkerungsentwicklung")
                )
              )
      ),
      tabItem(tabName = "bevoelkerungsstand",
              tabBox(
                tabPanel("Tabelle", dataTableOutput("tabelle_unterseite1")),
                tabPanel("Diagramm", plotOutput("diagramm_unterseite1")),
                tabPanel("Karte", leafletOutput("karte_unterseite1"))
              ),
              h3("Erläuterungen"),
              verbatimTextOutput("erl_bevoelkerungsstand")
      ),
      tabItem(tabName = "kinderbetreuung",
              tabBox(
                tabPanel("Tabelle", dataTableOutput("tabelle_unterseite2")),
                tabPanel("Diagramm", plotOutput("diagramm_unterseite2")),
                tabPanel("Karte", leafletOutput("karte_unterseite2"))
              ),
              h3("Erläuterungen"),
              verbatimTextOutput("erl_kinderbetreuung")
      )
      # Weitere Unterseiten für die restlichen Menüpunkte hier hinzufügen
    ),
    fluidRow(
      tags$style(".footer { margin-top: auto; }"),
      dashboardFooter(
        tags$div(
          class = "footer-text",
          tags$a("Impressum", href = "impressum.html"),
          tags$a("Datenschutz", href = "datenschutz.html"),
          tags$a("Cookie-Einstellungen", href = "cookie-einstellungen.html")
        )
        #includeHTML("impressum.html")  # Einbinden der Impressum-Seite
      )
    )
  )
)





server <- function(input, output, session) {
 
#  output$erl_bevoelkerungsentwicklung <- renderUI({
#    markdown_text <- readLines("erl_bevoelkerungsentwicklung.md")  # Pfad zur Markdown-Datei anpassen
#    html_code <- markdownToHTML(paste(markdown_text, collapse = "\n"))
#    div(HTML(html_code))
#  })
  
  output$erl_bevoelkerungsentwicklung <- renderText({
   bevoelkerung_erl <- readLines("erl_bevoelkerungsentwicklung.txt")
   paste(bevoelkerung_erl, collapse = "\n")
  })
  
  output$erl_bevoelkerungsstand <- renderText({
    bevoelkerung_erl <- readLines("erl_bevoelkerungsstand.txt")
    paste(bevoelkerung_erl, collapse = "\n")
  })
  
  output$erl_kinderbetreuung <- renderText({
    bevoelkerung_erl <- readLines("erl_kinderbetreuung.txt")
    paste(bevoelkerung_erl, collapse = "\n")
  })
  
  # Funktionen für die Bevölkerungsseite
  output$tabelle_bevoelkerung <- renderDataTable({
    data <- read.csv("bevoelkerung_tabelle.csv")  # Pfad zur CSV-Datei anpassen
    
    # Zeilen formatieren
    datatable(data) %>%
      formatStyle(columns = 1:ncol(data),
                  backgroundColor = styleEqual(c("lightblue", "white"), c("Niedersachsen", "Deutschland")),
                  fontWeight = styleEqual(c("bold", "normal"), c("Deutschland", NA)))
  })
  
  output$diagramm_bevoelkerung <- renderPlot({
    data <- read.csv("bevoelkerung_tabelle.csv")  # Pfad zur CSV-Datei anpassen
    
    # Daten auswählen
    data_subset <- subset(data, select = c("Land", "Einwohner"))
    
    # Balkendiagramm erstellen
    ggplot(data_subset, aes(x = reorder(Land, Einwohner), y = Einwohner)) +
      geom_bar(stat = "identity", fill = ifelse(data_subset$Land == "Niedersachsen", "lightblue", ifelse(data_subset$Land == "Deutschland", "black", "steelblue"))) +
      labs(x = "Land", y = "Einwohner") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  })
  
  # Weitere Server-Funktionen für die restlichen Tabs hier hinzufügen
}

shinyApp(ui, server)

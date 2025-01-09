library(shiny)
library(bslib)
library(ggplot2)
library(data.table)
library(httr)
library(rjson)
library(plotly)
library(DT)

ui <- page_sidebar(
  title = "Dane energetyczne",
  theme = bs_theme(preset = "sandstone"),
  
  # panel boczny 
  sidebar = sidebar(
    width = 325,

    # dodanie selektora zakresu czasowego danych wejsciowych      
    card(
      card_header("Ramy czasowe danych"),
      helpText("Wybierz zakres danych energetycznych"),
      card_body(
        dateRangeInput(
          "date_range",
          "Zakres dat",
          start = as.character(Sys.Date() - 9),
          end = as.character(Sys.Date() - 2),
          min = as.character("2024-06-14"),
          max = as.character(Sys.Date() - 2)
        )
      )
    ),

    # dodanie selektora wyboru zmiennej   
    card(
      card_header("Wybór zmiennej"),
      helpText("Wybierz zmienną, którą chcesz analizować."),
      card_body(
        height = 250,
        selectInput(
          "selected_variable",
          "Zmienna:",
          choices = c("Ceny energii" = "cen_rozl",
                      "Zapotrzebowanie na moc" = "zapotrzebowanie_na_moc",
                      "Fotowoltaika (PV)" = "pv",
                      "Wiatr" = "wiatr",
                      "Magazyny energii" = "sumatyczna_moc_ladowania_magazynow_energii"),
          selected = "cena_rozl"
        )
      )
    ),

    # zapis danych do pliku/ downloadButton    
    card(
      card_header("Eksport danych"),
      helpText("Możesz wyeksportować całość danych do pliku csv"),
      card_body(downloadButton("write_data", "Zapisz dane"))
    )
  ),
  
  # panel centralny 
  card(
    navset_card_underline(
      
  # wykres liniowy
      nav_panel(
        card_title("Wykres szeregu czasowego"),
        card_body(
          class = "align-items-center",
          plotlyOutput("price_plot"),
          radioButtons(
            "plot_radio",
            "Typ wykresu",
            choices = list("+ geom_line()" = 1, "+ geom_point()" = 2),
            inline = TRUE,
            selected = 1
          ),
          actionButton("plus_geom_smooth", " + geom_smooth(method='lm')", width = 325)
        )
      ),

    # histogram      
      nav_panel(
        card_title("Histogram"),
        card_body(
          class = "align-items-center",
          plotlyOutput("price_hist"),
          selectInput(
            "hist_fill_select",
            "fill",
            choices = list("yellow/red" = 1, "red/yellow" = 2, "blue/red" = 3),
            selected = 2
          ),
          sliderInput(
            "hist_bins",
            "geom_histogram::bins",
            min = 10,
            max = 100,
            value = 25,
            width = 325
          )
        )
      ),
      
    # boxplot
      nav_panel(
        card_title("Boxplot"),
        card_body(
          class = "align-items-center",
          plotlyOutput("price_boxplot")
        )
      )
      
      ,
    # tabela      
      nav_panel(
        card_title("Tabela"),
        card_body(
          class = "align-items-center",
          DT::dataTableOutput("price_tab_dt")
        )
      ),
    # dane diagnostyczne   
      nav_panel(
        card_title("Dane diagnostyczne"),
        card_body(verbatimTextOutput("text_demo"))
      )
    )
  )
)
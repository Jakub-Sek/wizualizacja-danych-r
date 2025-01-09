# Server
source("utils.r")

server <- function(input, output) {
  
  # dane aplikacji
  dataInput <- reactive({
    ret_df_crb <- data.frame()
    ret_df_his <- data.frame()
    
    try({
      from <- input$date_range[1]
      to <- input$date_range[2]
      days <- as.character(seq(from = as.Date(from), to = as.Date(to), by = "1 day"))
      
      # dane z plików CRB_ROZLP 
      ret_df_crb <- as.data.frame(rbindlist(lapply(days, function(day) {
        return(get_CENY_ROZL(day))
      }), fill = TRUE))
      
      # dane z plików HIS_WLK_CAL 
      ret_df_his <- as.data.frame(rbindlist(lapply(days, function(day) {
        return(get_HIS_WLK_CAL(day))
      }), fill = TRUE))
      
      
    })
    
    # Złączenie danych z plików: HIS_WLK_CAL i CRB_ROZLP 
    if (nrow(ret_df_his) > 0 && nrow(ret_df_crb) > 0) {
      merged_df <- merge(ret_df_his, ret_df_crb, by = "timestamp", all = TRUE)
    } else {
      merged_df <- rbind(ret_df_his, ret_df_crb)
    }
    
    return(merged_df)
  })
  
  # Obsługa wyboru zmiennej
  selectedData <- reactive({
    df <- dataInput()
    if (!is.null(df) && input$selected_variable %in% colnames(df)) {
      df <- df[, c("timestamp", input$selected_variable), drop = FALSE]
      colnames(df) <- c("timestamp", "value")
      df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    } else {
      df <- data.frame()
    }
    return(df)
  })
  
  # dane diagnostyczne
  output$text_demo <- renderPrint({
    df <- dataInput()
    print(head(df))
    str(df)
  })
  
  # zmienna logiczna T/F - w zależności od stanu actionButton 
  v <- reactiveValues(if_geom_smooth = FALSE)
  
  # powiązanie zmiennej logicznej ze stanem przycisku 
  observeEvent(input$plus_geom_smooth, {
    v$if_geom_smooth <- !v$if_geom_smooth
  })
  
  # funkcja generujaca wykres szeregu czasowego 
  output$price_plot <- renderPlotly({
    df <- selectedData()
    img <- ggplot()
    if (nrow(df)) {
      df$timestamp <- as.POSIXct(df$timestamp)
      img <- ggplot(df, aes(x = timestamp, y = value)) +
        xlab("Czas") + ylab(input$selected_variable)
      
      # obsługa plot_radio
      if (input$plot_radio == 1) {
        img <- img + geom_line(color = "red")
      } else if (input$plot_radio == 2) {
        img <- img + geom_point(color = "blue", size = 0.5)
      }
      
      # dodawanie akcji dla actionButton 
      if (v$if_geom_smooth) {
        img <- img + geom_smooth(method = "lm", se = FALSE)
      }
    }
    ggplotly(img)
  })
  

  
  # histogram
  output$price_hist <- renderPlotly({
    df <- selectedData()
    img <- ggplot()
    if (nrow(df)) {
      
      # 24 - wartości zwracane przez hist_fill_select
      if (input$hist_fill_select == 1) {
        fill_col <- "yellow"
        border_col <- "red"
      } else if (input$hist_fill_select == 2) {
        fill_col <- "red"
        border_col <- "yellow"
      } else if (input$hist_fill_select == 3) {
        fill_col <- "blue"
        border_col <- "red"
      }
      
      img <- ggplot(df, aes(x = value)) +
        geom_histogram(bins = input$hist_bins, fill = fill_col, color = border_col) +
        xlab(input$selected_variable) + ylab("Liczba wystąpień")
    }
    ggplotly(img)
  })
  
  #boxplot 
  output$price_boxplot <- renderPlotly({
    df <- selectedData()
    img <- ggplot()
    
    if(nrow(df)) {
      colnames(df) <- c("timestamp", "value")
      df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      df$hour <- as.POSIXlt(df$timestamp)$hour
      df$hour[df$hour < 10] <- paste0(0, df$hour[df$hour < 10])
      df$wday <- as.POSIXlt(df$timestamp)$wday
      df$variable <- paste0(df$wday, "_", df$hour)
      df$variable <- factor(df$variable, levels = sort(unique(df$variable)))
      days_names_vec <- c("niedziela", "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota")
      df$wday_names <- days_names_vec[df$wday + 1]  
      df$wday_names <- factor(df$wday_names, levels = days_names_vec)
      
      img <- (
        ggplot(df, aes(x = variable, y = value, fill = factor(wday_names))) +
          geom_polygon(
            data = data.frame(
              x = c(25, 6*24, 6*24, 25),  
              y = c(min(df$value), min(df$value), max(df$value), max(df$value))
            ), 
            aes(x = x, y = y), fill = "darkgray", alpha = 0.25
          ) + 
          geom_boxplot() +
          xlab("") +
          scale_x_discrete(breaks = sort(unique(df$variable))[seq(from = 1, to = 168, by = 6)]) +
          theme(
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),    
            axis.text.y = element_text(size = 8),    
            axis.title.y = element_text(size = 16, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(size = 18, face = "bold"),
            legend.text = element_text(size = 12, face = "italic")
          ) + 
          guides(fill = guide_legend(nrow = 2)) +
          labs(fill = "Dzień tygodnia")
      )
    }
    
    return(ggplotly(img))
  })
  
  # przygotowanie sformatowanej tabeli z biblioteką DT
  output$price_tab_dt <- DT::renderDataTable({
    df <- selectedData()
    DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 8))
  })
  
  # obsługa downloadButton/write_data
  output$write_data <- downloadHandler(
    filename = function() { "dane_energetyczne.csv" },
    content = function(file) {
      write.table(dataInput(), file, sep = ";", dec = ",", row.names = FALSE)
    }
  )
}
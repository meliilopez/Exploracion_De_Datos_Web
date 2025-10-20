# Shiny dashboard leyendo desde ClickHouse por HTTP
# ==> Solo consulta, no crea tablas ni vistas

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(treemapify)
library(data.table)
library(httr)

options(shiny.launch.browser = TRUE)

# -----------------------------
# Config ClickHouse
# -----------------------------
ch_host   <- "http://localhost:8123"
ch_user   <- "default"
ch_pass   <- "secret"
db_name   <- "bi"
tbl_name  <- "access_log_parsed"   # ya creada en ingesta
full_table <- paste0(db_name, ".", tbl_name)

http_close <- httr::add_headers(Connection = "close")

# -----------------------------
# Helpers HTTP a ClickHouse
# -----------------------------
ch_query_df <- function(sql) {
  q <- paste(sql, "FORMAT TabSeparatedWithNames")
  r <- try(httr::GET(
    ch_host,
    query = list(query = q),
    authenticate(ch_user, ch_pass, type = "basic"),
    http_close,
    httr::timeout(120)
  ), silent = TRUE)
  
  if (inherits(r, "try-error")) {
    message("HTTP error: ", conditionMessage(attr(r, "condition")))
    return(data.frame())
  }
  if (httr::http_error(r)) {
    msg <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) "")
    message("ClickHouse HTTP ", httr::status_code(r), ": ", msg)
    return(data.frame())
  }
  txt <- httr::content(r, "text", encoding = "UTF-8")
  if (!nzchar(txt)) return(data.frame())
  data.table::fread(txt, sep = "\t", header = TRUE, na.strings = "\\N", data.table = FALSE)
}

# =======================================
# UI
# =======================================
ui <- fluidPage(
  titlePanel(sprintf("Dashboard de Logs (ClickHouse: %s.%s)", db_name, tbl_name)),
  fluidRow(
    column(4, div(tags$h5("Filas leídas"),        tags$h2(textOutput("kpi_rows")))),
    column(4, div(tags$h5("Tasa de errores (%)"), tags$h2(textOutput("kpi_errores")))),
    column(4, div(tags$h5("Promedio de Bytes"),   tags$h2(textOutput("kpi_bytes"))))
  ),
  hr(),
  fluidRow(
    column(6, plotlyOutput("graf_top_endpoints", height="350px")),
    column(6, plotlyOutput("graf_top_metodos",   height="350px"))
  ),
  fluidRow(
    column(6, plotOutput("treemap_browser", height="350px")),
    column(6, plotlyOutput("graf_status_family", height="350px"))
  )
)

# =======================================
# Server
# =======================================
server <- function(input, output, session) {
  cat("Usando tabla/vista: ", full_table, "\n")
  
  # KPI: filas totales
  output$kpi_rows <- renderText({
    df <- ch_query_df(sprintf("SELECT count() AS rows FROM %s", full_table))
    if (!nrow(df)) return("0")
    format(df$rows[1], big.mark = ",", scientific = FALSE)
  })
  
  # KPI: tasa de errores (>=400)
  output$kpi_errores <- renderText({
    df <- ch_query_df(sprintf(
      "SELECT ifNull(round(100.0 * countIf(status >= 400) / nullIf(count(),0), 2), 0) AS tasa
       FROM %s", full_table))
    if (!nrow(df)) return("0.00 %")
    sprintf("%.2f %%", df$tasa[1])
  })
  
  # KPI: promedio de bytes
  output$kpi_bytes <- renderText({
    df <- ch_query_df(sprintf(
      "SELECT ifNull(round(avg(bytes), 2), 0) AS avg_bytes
       FROM %s", full_table))
    if (!nrow(df)) return("0.00 bytes")
    sprintf("%.2f bytes", df$avg_bytes[1])
  })
  
  # Top 5 Endpoints (normaliza slashes y agrupa por primer segmento)
  output$graf_top_endpoints <- renderPlotly({
    df <- ch_query_df(sprintf(
      "WITH
       replaceRegexpOne(replaceRegexpAll(url_simple, '/{2,}', '/'), '/+$', '') AS endpoint0,
       multiIf(endpoint0 = '' OR endpoint0 = '/', '/', endpoint0) AS endpoint1,
       if(match(endpoint1, '^/[^/?]+'),
          extract(endpoint1, '^(/[^/?]+)'),
          endpoint1) AS first_seg
     SELECT first_seg AS url_simple, count() AS n
     FROM %s
     WHERE notEmpty(first_seg) AND first_seg <> '/'
     GROUP BY first_seg
     ORDER BY n DESC
     LIMIT 5", full_table))
    
    validate(need(nrow(df) > 0, "Sin datos para endpoints"))
    
    p <- ggplot(df, aes(x = reorder(url_simple, n), y = n, fill = url_simple,
                        text = paste0('URL: ', url_simple, '<br>Requests: ', n))) +
      geom_bar(stat = 'identity') +
      scale_y_log10() +
      coord_flip() +
      xlab("Endpoint (primer segmento)") +
      ylab("Requests (escala log)") +
      ggtitle("Top 5 Endpoints (agrupados por primer segmento)") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Top 5 Métodos
  output$graf_top_metodos <- renderPlotly({
    df <- ch_query_df(sprintf(
      "SELECT method, count() AS n
       FROM %s
       WHERE method <> 'UNKNOWN' AND notEmpty(method)
       GROUP BY method
       ORDER BY n DESC
       LIMIT 5", full_table))
    validate(need(nrow(df) > 0, "Sin datos para métodos"))
    p <- ggplot(df, aes(x=reorder(method, n), y=n, fill=method,
                        text=paste0('Método: ', method, '<br>Requests: ', n))) +
      geom_bar(stat="identity") +
      scale_y_log10() +
      ylab("Cantidad de requests") + xlab("Método") +
      ggtitle("Top 5 Métodos HTTP (log scale)") +
      theme(legend.position="none")
    ggplotly(p, tooltip="text")
  })
  
  # Treemap Navegador
  output$treemap_browser <- renderPlot({
    df <- ch_query_df(sprintf(
      "SELECT browser_family, n
     FROM (
       SELECT
         multiIf(
           browser = '' OR browser IS NULL, 'Desconocido',
           positionCaseInsensitive(browser,'bot')>0 OR positionCaseInsensitive(browser,'spider')>0 OR positionCaseInsensitive(browser,'crawler')>0, 'Bots',
           positionCaseInsensitive(browser,'Edg')>0,      'Edge',
           positionCaseInsensitive(browser,'OPR')>0 OR positionCaseInsensitive(browser,'Opera')>0, 'Opera',
           positionCaseInsensitive(browser,'Firefox')>0,  'Firefox',
           positionCaseInsensitive(browser,'Chrome')>0 AND positionCaseInsensitive(browser,'Chromium')=0, 'Chrome',
           positionCaseInsensitive(browser,'Safari')>0 AND positionCaseInsensitive(browser,'Chrome')=0,  'Safari',
           positionCaseInsensitive(browser,'MSIE')>0 OR positionCaseInsensitive(browser,'Trident')>0, 'Otros',
           'Otros'
         ) AS browser_family,
         count() AS n
       FROM %s
       GROUP BY browser_family
     )
     WHERE browser_family <> 'Desconocido'
     ORDER BY n DESC", full_table))
    
    validate(need(nrow(df) > 0, "Sin datos para navegadores"))
    
    ggplot(df, aes(area = n, fill = browser_family, label = paste(browser_family, "\n", n))) +
      geom_treemap() +
      geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
      ggtitle("Distribución por Navegador (familias)")
  })
  
  # Status Family
  output$graf_status_family <- renderPlotly({
    df <- ch_query_df(sprintf(
      "WITH intDiv(status, 100) * 100 AS sf
       SELECT sf AS status_family, count() AS n
       FROM %s
       GROUP BY status_family
       ORDER BY status_family", full_table))
    validate(need(nrow(df) > 0, "Sin datos para status family"))
    p <- ggplot(df, aes(x=factor(status_family), y=n, fill=factor(status_family),
                        text=paste0('Status Family: ', status_family, '<br>Requests: ', n))) +
      geom_bar(stat="identity") +
      scale_y_log10() +
      ylab("Cantidad de requests") + xlab("Status Family") +
      ggtitle("Requests por Status Family (log scale)") +
      theme(legend.position="none")
    ggplotly(p, tooltip="text")
  })
}

shinyApp(ui=ui, server=server)






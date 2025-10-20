# ============================
# Ingesta .log -> ClickHouse (R)
# Detecta estructura, crea tabla y carga por chunks
# Si el log NO es "combined":
#   - crea bi.access_log_auto (cruda)
#   - crea bi.access_log_parsed (parseada)
#   - crea MATERIALIZED VIEW bi.mv_access_log_auto_to_parsed
#   - SIN backfill (la MV pobla en vivo durante la ingesta)
# ============================

suppressPackageStartupMessages({
  library(readr); library(stringr); library(httr); library(data.table)
})

# -------- CONFIG --------
log_path  <- "C:/Users/guill/OneDrive/Escritorio/04-Archivos de datos/access_ssl_20230404.log"

# ClickHouse
ch_host   <- "http://localhost:8123"
ch_user   <- "default"
ch_pass   <- "secret"
db_name   <- "bi"

chunk_size <- 150000L 
try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)

# -------- helpers HTTP (con cierre de conexión y timeout) --------
http_close <- httr::add_headers(Connection = "close")

run_sql <- function(sql) {
  r <- httr::POST(
    url   = ch_host,
    query = list(query = sql),
    authenticate(ch_user, ch_pass, type = "basic"),
    http_close,
    httr::timeout(60)
  )
  if (httr::http_error(r)) {
    stop(paste("ClickHouse:", httr::content(r, "text", encoding = "UTF-8")))
  }
  invisible(TRUE)
}

ch_query_text <- function(sql) {
  r <- httr::GET(
    ch_host,
    query = list(query = paste(sql, "FORMAT TabSeparatedWithNames")),
    authenticate(ch_user, ch_pass, type = "basic"),
    http_close,
    httr::timeout(60)
  )
  stop_for_status(r)
  content(r, "text", encoding = "UTF-8")
}

ch_query_df <- function(sql) {
  txt <- ch_query_text(sql)
  if (!nzchar(txt)) return(data.frame())
  data.table::fread(txt, sep = "\t", header = TRUE, data.table = FALSE)
}

ingesta_run <- function() {
  on.exit({
    # cierre defensivo de conexiones
    try(closeAllConnections(), silent = TRUE)
    flush.console()
    gc()
  }, add = TRUE)
  
  # Probar conexión
  res_check <- GET(paste0(ch_host, "/?query=SELECT%201"),
                   authenticate(ch_user, ch_pass, type = "basic"),
                   http_close, timeout(10))
  stop_for_status(res_check)
  cat("CH OK:", content(res_check, "text"), "\n")
  
  # -------- 1) HUSMEAR MUESTRA --------
  sample_lines <- readr::read_lines(log_path, n_max = 4000)
  sample_trim  <- trimws(sample_lines)
  
  # patrón Apache/Nginx combined
  pat_combined <- '^(\\S+) (\\S+) (\\S+) \\[([^\\]]+)\\] "([^"]*)" (\\d{3}) (\\S+)(?: "([^"]*)" "([^"]*)")?$'
  looks_combined <- if (length(sample_trim)) mean(grepl(pat_combined, sample_trim)) > 0.5 else FALSE
  
  # detectar JSONL
  jsonish  <- sample_trim[substr(sample_trim,1,1)=="{"]
  is_jsonl <- length(jsonish) > 50
  
  # ---- decidir nombre de tabla según modo ----
  if (looks_combined) {
    mode <- "combined"
    table_name <- "access_log"       # parseada
  } else if (is_jsonl) {
    mode <- "jsonl"
    table_name <- "access_log_auto"  # cruda
  } else {
    mode <- "raw"
    table_name <- "access_log_auto"  # cruda
  }
  
  # -------- 2) DDL SEGÚN DETECCIÓN --------
  if (mode == "combined") {
    create_db_sql <- sprintf("CREATE DATABASE IF NOT EXISTS %s", db_name)
    create_tbl_sql <- sprintf("
CREATE TABLE IF NOT EXISTS %s.%s
(
  ts             Nullable(DateTime),
  method         LowCardinality(String),
  url            String,
  status         UInt16,
  status_family  UInt16,
  bytes          UInt64,
  user_agent     String,
  browser        String,
  url_simple     LowCardinality(String)
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(ifNull(ts, toDateTime(0)))
ORDER BY (ifNull(ts, toDateTime(0)), url_simple, status_family)
", db_name, table_name)
  } else {
    create_db_sql <- sprintf("CREATE DATABASE IF NOT EXISTS %s", db_name)
    create_tbl_sql <- sprintf("
CREATE TABLE IF NOT EXISTS %s.%s
(
  ts        Nullable(DateTime),
  line_raw  String
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(ifNull(ts, toDateTime(0)))
ORDER BY (ifNull(ts, toDateTime(0)))
", db_name, table_name)
  }
  
  cat("\nDDL (modo:", mode, ")\n", create_db_sql, "\n", create_tbl_sql, "\n", sep = "")
  
  # -------- 3) CREAR DB Y TABLA --------
  run_sql(create_db_sql)
  run_sql(create_tbl_sql)
  cat("✅ Tabla lista: ", db_name, ".", table_name, "\n", sep="")
  
  # -------- 4) (SOLO NO-COMBINED) crear TABLA PARSEADA + MATERIALIZED VIEW ANTES DE INGESTAR --------
  if (mode != "combined") {
    cat("\n-- Preparando MATERIALIZED VIEW (antes de la ingesta) --\n")
    
    # borrar artefactos viejos, por las dudas
    run_sql("DROP VIEW IF EXISTS bi.v_access_log_parsed")
    run_sql(sprintf("DROP VIEW IF EXISTS %s.mv_access_log_auto_to_parsed", db_name))
    
    # asegurar tabla destino parseada
    run_sql(sprintf("
CREATE TABLE IF NOT EXISTS %s.access_log_parsed
(
  ts             Nullable(DateTime),
  method         LowCardinality(String),
  url            String,
  status         UInt16,
  status_family  UInt16,
  bytes          UInt64,
  user_agent     String,
  browser        String,
  url_simple     LowCardinality(String)
)
ENGINE = MergeTree
PARTITION BY toYYYYMM(ifNull(ts, toDateTime(0)))
ORDER BY (ifNull(ts, toDateTime(0)), url_simple, status_family)
", db_name))
    
    # crear MV que escriba en access_log_parsed
    sql_mv <- sprintf("
CREATE MATERIALIZED VIEW %s.mv_access_log_auto_to_parsed
TO %s.access_log_parsed AS
WITH
  extract(line_raw, '\"([^\"]*)\"') AS req,
  arrayElement(splitByChar(' ', req), 2) AS url_path
SELECT
  parseDateTimeBestEffortOrNull(extract(line_raw, '\\\\[([^\\\\]]+)\\\\]'))                     AS ts,
  arrayElement(splitByChar(' ', req), 1)                                                       AS method,
  url_path                                                                                     AS url,
  toUInt16OrZero(extract(line_raw, '\"\\\\s(\\\\d{3})\\\\s'))                                  AS status,
  intDiv(status, 100) * 100                                                                    AS status_family,
  toUInt64OrZero(replaceOne(extract(line_raw, '\"\\\\s\\\\d{3}\\\\s(\\\\S+)\\\\s\"'), '-', '0')) AS bytes,
  extract(line_raw, '\"[^\"]*\"\\\\s\"([^\"]*)\"$')                                            AS user_agent,
  user_agent                                                                                   AS browser,
  if(empty(extract(url_path, '^/[^/?]+')), url_path, extract(url_path, '^/[^/?]+'))            AS url_simple
FROM %s.access_log_auto
WHERE length(line_raw) > 0
", db_name, db_name, db_name)
    run_sql(sql_mv)
    cat("✅ MATERIALIZED VIEW creada: ", db_name, ".mv_access_log_auto_to_parsed\n", sep = "")
  }
  
  # -------- 5) ABRIR ARCHIVO (robusto) --------
  path_norm <- normalizePath(log_path, winslash = "/", mustWork = TRUE)
  cat("Archivo:", path_norm, "\n")
  
  open_con <- function(path) {
    if (grepl("\\.gz$", path, ignore.case = TRUE)) {
      gzfile(path, open = "rb")
    } else {
      file(path, open = "r", encoding = "UTF-8")
    }
  }
  con_in <- open_con(path_norm)
  on.exit(try(close(con_in), silent = TRUE), add = TRUE)
  
  if (!isOpen(con_in)) open(con_in)
  tmp <- try(readLines(con_in, n = 3, warn = FALSE), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    close(con_in); stop("No pude leer el archivo (¿bloqueado por OneDrive o ruta incorrecta?).")
  }
  close(con_in); con_in <- open_con(path_norm); if (!isOpen(con_in)) open(con_in)
  
  # -------- 6) CARGA POR CHUNKS --------
  insert_url <- paste0(
    ch_host, "/?query=",
    URLencode(sprintf("INSERT INTO %s.%s FORMAT CSV", db_name, table_name), reserved = TRUE)
  )
  
  i <- 1L
  repeat {
    lines <- readLines(con_in, n = chunk_size, warn = FALSE)
    if (!length(lines)) break
    lines <- trimws(lines); lines <- lines[nchar(lines) > 0]
    
    if (mode == "combined") {
      m <- stringr::str_match(lines, pat_combined)
      ok <- which(!is.na(m[,1])); if (!length(ok)) { i <- i + 1L; next }
      m <- m[ok,,drop=FALSE]
      
      req    <- stringr::str_match(m[,5], '^([A-Z]+)\\s+([^\\s]+)\\s+([^\\s]+)$')
      method <- req[,2]
      url    <- req[,3]
      status <- suppressWarnings(as.integer(m[,6]))
      bytes  <- suppressWarnings(as.numeric(ifelse(m[,7] %in% c("-", "/", ""), NA, m[,7])))
      ua     <- if (ncol(m) >= 9) m[,9] else ""
      
      # timestamp [10/Oct/2000:13:55:36 -0700]
      tstr <- gsub("^\\[|\\]$", "", m[,4])
      ts   <- strptime(tstr, "%d/%b/%Y:%H:%M:%S %z", tz = "UTC")
      
      # filtros mínimos
      keep <- !is.na(status) & !is.na(bytes) & !is.na(url) & url != "/"
      if (!any(keep)) { i <- i + 1L; next }
      method <- method[keep]; url <- url[keep]
      status <- status[keep]; bytes <- bytes[keep]
      ua     <- ua[keep];     ts    <- ts[keep]
      
      status_family <- (status %/% 100) * 100
      url_simple <- ifelse(grepl("^/[^/?]+", url),
                           sub("^((/[^/?]+)).*$", "\\1", url),
                           url)
      browser <- ua
      
      ts_dt  <- as.POSIXct(ts, tz="UTC")
      ts_str <- ifelse(is.na(ts_dt), NA, format(ts_dt, "%Y-%m-%d %H:%M:%S"))
      
      df <- data.frame(
        ts = ts_str,
        method = ifelse(is.na(method), "", method),
        url = ifelse(is.na(url), "", url),
        status = status,
        status_family = status_family,
        bytes = as.character(as.double(bytes)),
        user_agent = ifelse(is.na(ua), "", ua),
        browser = ifelse(is.na(browser), "", browser),
        url_simple = ifelse(is.na(url_simple), "", url_simple),
        stringsAsFactors = FALSE
      )
      
    } else {
      # jsonl/raw: guardar línea completa; ts = NULL -> \N
      df <- data.frame(
        ts = NA,
        line_raw = lines,
        stringsAsFactors = FALSE
      )
    }
    
    tf <- tempfile(fileext = ".csv")
    write.table(df, tf, sep = ",", row.names = FALSE, col.names = FALSE,
                qmethod = "double", na = "\\N")  # \N = NULL en CSV de CH
    
    r <- httr::POST(
      url = insert_url,
      authenticate(ch_user, ch_pass, type = "basic"),
      body = upload_file(tf),
      encode = "multipart",
      http_close,
      httr::timeout(300)
    )
    stop_for_status(r)
    
    cat("✅ chunk", i, "filas:", nrow(df), "\n")
    i <- i + 1L
  }
  
  cat("✅ carga completa en ", db_name, ".", table_name, "\n", sep = "")
  try(close(con_in), silent = TRUE)
  
  # -------- 7) Verificaciones rápidas --------
  if (mode != "combined") {
    cat("\n-- Filas crudas --\n")
    cat(ch_query_text(sprintf("SELECT count() AS rows FROM %s.access_log_auto", db_name)), "\n")
    
    cat("\n-- Filas parseadas (tabla) --\n")
    cat(ch_query_text(sprintf("SELECT count() AS rows FROM %s.access_log_parsed", db_name)), "\n")
    
    cat("\n-- Muestra (5 filas) --\n")
    print(ch_query_df(sprintf("
      SELECT ts, method, url, status, bytes
      FROM %s.access_log_parsed
      ORDER BY ts DESC
      LIMIT 5
    ", db_name)))
    
    cat('\n👉 En el Shiny, usá:  tbl_name <- "access_log_parsed"\n')
  } else {
    cat("👉 Tabla parseada lista para Shiny: bi.access_log\n")
  }
}

# ---- Ejecutar ----
ingesta_run()
cat("\n✅ Ingesta finalizada.\n")


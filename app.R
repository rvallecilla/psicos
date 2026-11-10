# =============================================================================
# Chaos NetAct — Sector Power Control (Shiny)
# UI operativa NOC: conectar, operar, monitorear estado e historial.
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(plotly)

# Cargar cliente API (cwd = directorio de la app al usar shiny::runApp)
api_path <- file.path("R", "api.R")
if (!file.exists(api_path)) {
  stop("No se encontró R/api.R. Ejecute shiny::runApp() desde el directorio del proyecto.")
}
source(api_path, local = FALSE)

# Defaults desde entorno (sin secretos hardcodeados)
env_or <- function(key, default = "") {
  val <- Sys.getenv(key, unset = "")
  if (!nzchar(val)) default else val
}

# Cargar .Renviron del proyecto si existe (Bearer / URL guardados localmente)
LOCAL_RENVIRON <- normalizePath(file.path(".", ".Renviron"), mustWork = FALSE)
if (file.exists(LOCAL_RENVIRON)) {
  try(readRenviron(LOCAL_RENVIRON), silent = TRUE)
}

DEFAULT_BASE_URL <- env_or("CHAOS_BASE_URL", "http://172.22.129.126:8001")
DEFAULT_TOKEN    <- env_or("CHAOS_BEARER_TOKEN", "")
DEFAULT_USER     <- env_or("CHAOS_USERNAME", "")
DEFAULT_PASS     <- env_or("CHAOS_PASSWORD", "")
DEFAULT_PORT     <- 8001L

#' Separa host / puerto / esquema de una Base URL.
parse_base_url <- function(url, default_port = DEFAULT_PORT) {
  url <- trimws(as.character(url %||% ""))
  if (!nzchar(url)) {
    return(list(scheme = "http", host = "172.22.129.126", port = as.integer(default_port)))
  }
  # Aceptar host:puerto sin esquema
  if (!grepl("://", url, fixed = TRUE)) {
    url <- paste0("http://", url)
  }
  m <- regexec("^([a-zA-Z][a-zA-Z0-9+.-]*)://([^/:]+)(?::([0-9]+))?/?.*$", url, perl = TRUE)
  parts <- regmatches(url, m)[[1]]
  if (length(parts) < 3) {
    return(list(scheme = "http", host = url, port = as.integer(default_port)))
  }
  scheme <- parts[[2]]
  host <- parts[[3]]
  port <- if (length(parts) >= 4 && nzchar(parts[[4]])) {
    as.integer(parts[[4]])
  } else if (identical(tolower(scheme), "https")) {
    443L
  } else {
    as.integer(default_port)
  }
  list(scheme = scheme, host = host, port = port)
}

#' Construye Base URL a partir de host + puerto (+ esquema).
build_base_url <- function(host, port, scheme = "http") {
  host <- trimws(as.character(host %||% ""))
  host <- sub("^https?://", "", host, ignore.case = TRUE)
  host <- sub("/+$", "", host)
  host <- sub(":\\d+$", "", host)
  scheme <- tolower(trimws(as.character(scheme %||% "http")))
  if (!scheme %in% c("http", "https")) scheme <- "http"
  port_n <- suppressWarnings(as.integer(port))
  if (is.na(port_n) || port_n < 1L || port_n > 65535L) {
    port_n <- DEFAULT_PORT
  }
  sprintf("%s://%s:%d", scheme, host, port_n)
}

parsed_default <- parse_base_url(DEFAULT_BASE_URL, DEFAULT_PORT)
DEFAULT_HOST   <- parsed_default$host
DEFAULT_SCHEME <- parsed_default$scheme
DEFAULT_PORT_N <- if (is.na(parsed_default$port) || is.null(parsed_default$port)) {
  DEFAULT_PORT
} else {
  as.integer(parsed_default$port)
}
# Preferir CHAOS_PORT del entorno si está definido
env_port <- suppressWarnings(as.integer(env_or("CHAOS_PORT", "")))
if (!is.na(env_port) && env_port >= 1L && env_port <= 65535L) {
  DEFAULT_PORT_N <- env_port
}

#' Actualiza o inserta claves KEY=VALUE en un archivo .Renviron (sin tocar otras).
upsert_renviron <- function(path, values) {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  keys <- names(values)
  kept <- lines
  for (k in keys) {
    kept <- kept[!grepl(paste0("^\\s*", k, "\\s*="), kept)]
  }
  kept <- kept[!grepl("^# Generado/actualizado por la app Shiny", kept)]
  while (length(kept) && !nzchar(kept[[length(kept)]])) {
    kept <- kept[-length(kept)]
  }
  new_lines <- vapply(keys, function(k) {
    val <- as.character(values[[k]] %||% "")
    val <- gsub("\\", "\\\\", val, fixed = TRUE)
    val <- gsub("'", "\\'", val, fixed = TRUE)
    paste0(k, "='", val, "'")
  }, character(1), USE.NAMES = FALSE)
  out <- c(
    "# Generado/actualizado por la app Shiny Sector Power (local; no versionar).",
    kept,
    new_lines
  )
  writeLines(out, path, useBytes = FALSE)
  invisible(path)
}

HISTORY_MAX <- 20L

redact_json <- function(obj) {
  raw <- tryCatch(
    toJSON(obj, pretty = TRUE, auto_unbox = TRUE, null = "null"),
    error = function(e) "{}"
  )
  gsub(
    '"(password|token|bearer|Authorization)"\\s*:\\s*"[^"]*"',
    '"\\1":"***"',
    raw,
    ignore.case = TRUE
  )
}

# -----------------------------------------------------------------------------
# Tema sobrio NOC (slate / teal operativo — sin plantilla morada genérica)
# -----------------------------------------------------------------------------
noc_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1a5f7a",
  secondary = "#34495e",
  success = "#1e7a46",
  danger = "#c0392b",
  warning = "#d68910",
  info = "#2471a3",
  light = "#eef2f5",
  dark = "#1c2833",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("IBM Plex Sans"),
  "border-radius" = "0.25rem",
  "font-size-base" = "0.8rem"
) |>
  bs_add_rules("
    :root {
      --noc-panel: #ffffff;
      --noc-ink: #1c2833;
      --noc-muted: #5d6d7e;
      --noc-line: #d5dde5;
      --noc-on: #1e7a46;
      --noc-off: #c0392b;
      --noc-accent: #1a5f7a;
      --noc-unknown: #7f8c8d;
    }
    body {
      background: #e8edf1;
      color: var(--noc-ink); min-height: 100vh;
    }
    .container-fluid {
      padding-top: .2rem !important; padding-bottom: .25rem !important;
      max-width: 1600px;
    }
    .bslib-gap-spacing { gap: .35rem !important; }
    .app-header {
      display: flex; align-items: center; justify-content: space-between; gap: .5rem; flex-wrap: wrap;
      background: linear-gradient(90deg, #1c2833 0%, #1a5f7a 100%);
      color: #f4f7f9; padding: .3rem .65rem; margin: 0 0 .3rem;
      border-bottom: 2px solid #148f77; border-radius: .25rem;
    }
    .app-header h1 { font-size: .95rem; margin: 0; font-weight: 600; letter-spacing: .02em; }
    .app-header .subtitle { font-size: .68rem; opacity: .8; margin: 0; }
    .noc-card {
      background: var(--noc-panel); border: 1px solid var(--noc-line);
      border-radius: .3rem; padding: .4rem .55rem; margin-bottom: .3rem;
      box-shadow: none; height: 100%;
    }
    .noc-card h2 {
      font-size: .65rem; font-weight: 700; text-transform: uppercase;
      letter-spacing: .06em; color: var(--noc-accent); margin: 0 0 .3rem;
      padding-bottom: .15rem; border-bottom: 1px solid var(--noc-line);
    }
    .noc-card .form-label, .noc-card label.control-label {
      font-size: .68rem; margin-bottom: .05rem; font-weight: 600;
    }
    .noc-card .form-group, .noc-card .mb-3, .noc-card .shiny-input-container {
      margin-bottom: .25rem !important;
    }
    .noc-card .form-control, .noc-card .form-select {
      min-height: 1.7rem; padding: .1rem .35rem; font-size: .78rem;
    }
    .noc-card .form-check { min-height: auto; margin-bottom: 0; padding-top: 0; }
    .noc-card .form-text, .noc-card .help-block { display: none; }
    .grid-top {
      display: grid;
      grid-template-columns: 4.2rem minmax(7rem,1fr) 4.2rem minmax(8rem,1.1fr) minmax(6rem,.9fr) minmax(6rem,.9fr) auto;
      gap: .25rem .35rem; align-items: end;
    }
    .grid-top > .shiny-input-container { margin-bottom: 0 !important; width: 100%; }
    .grid-sector {
      display: grid; grid-template-columns: 1fr 5.5rem; gap: .25rem .35rem; align-items: end;
    }
    .conn-actions {
      display: flex; flex-direction: column; justify-content: flex-end;
      min-width: 0; margin-bottom: 0 !important;
    }
    .conn-actions > .form-label {
      font-size: .68rem; margin-bottom: .05rem; font-weight: 600;
      line-height: 1.2; white-space: nowrap;
    }
    .conn-actions .row-actions {
      display: flex; align-items: center; gap: .3rem; flex-wrap: nowrap;
      min-height: 1.7rem;
    }
    .conn-actions .btn {
      height: 1.7rem; min-height: 1.7rem; padding: .1rem .45rem !important;
      font-size: .72rem; line-height: 1.2; display: inline-flex; align-items: center;
    }
    .row-actions {
      display: flex; align-items: center; gap: .3rem; flex-wrap: wrap;
    }
    .url-chip {
      font-size: .65rem; color: var(--noc-muted); margin: .1rem 0 0;
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    }
    .status-pill {
      display: inline-flex; align-items: center; gap: .25rem;
      font-size: .65rem; font-weight: 600; padding: .1rem .4rem;
      border-radius: 999px; border: 1px solid transparent;
    }
    .status-pill.ok { background: #d5f5e3; color: #145a32; border-color: #82e0aa; }
    .status-pill.bad { background: #fadbd8; color: #7b241c; border-color: #f1948a; }
    .status-pill.idle { background: #eaecee; color: #566573; border-color: #ccd1d1; }
    .status-dot { width: .4rem; height: .4rem; border-radius: 50%; background: currentColor; }
    .btn-power-on {
      background: var(--noc-on) !important; border-color: #196f3d !important;
      color: #fff !important; font-weight: 600; min-height: 1.85rem; font-size: .8rem;
      padding: .15rem .4rem !important;
    }
    .btn-power-on:hover { background: #196f3d !important; }
    .btn-power-off {
      background: #d68910 !important; border-color: #b9770e !important;
      color: #fff !important; font-weight: 600; min-height: 1.85rem; font-size: .8rem;
      padding: .15rem .4rem !important;
    }
    .btn-power-off:hover { background: #b9770e !important; }
    .btn-ops {
      display: grid; grid-template-columns: 1fr 1fr; gap: .25rem; margin-top: .2rem;
    }
    .btn-sm, .btn-sm.btn { padding: .15rem .45rem !important; font-size: .72rem !important; }
    .result-ok { border-left: 3px solid var(--noc-on); background: #eafaf1; padding: .3rem .45rem; font-size: .75rem; }
    .result-err { border-left: 3px solid var(--noc-off); background: #fdedec; padding: .3rem .45rem; font-size: .75rem; }
    .result-idle { border-left: 3px solid #95a5a6; background: #f4f6f7; padding: .3rem .45rem; color: var(--noc-muted); font-size: .75rem; }
    .field-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(100px, 1fr)); gap: .2rem .3rem; }
    .field-item { background: #f8fafb; border: 1px solid var(--noc-line); padding: .2rem .3rem; border-radius: .2rem; }
    .field-item .lbl { font-size: .6rem; text-transform: uppercase; color: var(--noc-muted); letter-spacing: .03em; }
    .field-item .val { font-size: .72rem; font-weight: 600; word-break: break-all; }
    .history-wrap { max-height: 110px; overflow: auto; }
    .history-table { font-size: .7rem; margin-bottom: 0; }
    .history-table th { background: #1c2833; color: #fff; position: sticky; top: 0; }
    .history-table td, .history-table th { padding: .15rem .3rem !important; }
    details.raw-json { margin-top: .2rem; font-size: .7rem; }
    details.raw-json summary { cursor: pointer; color: var(--noc-accent); font-weight: 600; }
    details.raw-json pre {
      background: #1c2833; color: #d5dbdb; padding: .35rem; border-radius: .2rem;
      max-height: 120px; overflow: auto; margin-top: .15rem; font-size: .65rem;
    }
    .busy-banner {
      display: flex; align-items: center; gap: .4rem; padding: .25rem .5rem;
      background: #d4e6f1; border: 1px solid #7fb3d5; border-radius: .25rem;
      color: #1a5276; font-weight: 600; margin-bottom: .25rem; font-size: .75rem;
    }
    .confirm-warn { color: #7b241c; font-weight: 600; }
    .semaforo {
      display: flex; flex-direction: row; align-items: center; justify-content: flex-start;
      gap: .45rem; padding: .15rem 0; margin: 0 0 .2rem;
    }
    .semaforo-light {
      width: 2rem; height: 2rem; border-radius: 50%; flex-shrink: 0;
      border: 2px solid rgba(0,0,0,.1);
    }
    .semaforo-light.on { background: radial-gradient(circle at 35% 30%, #82e0aa, #1e7a46); }
    .semaforo-light.off { background: radial-gradient(circle at 35% 30%, #f5b041, #c0392b); }
    .semaforo-light.unknown { background: radial-gradient(circle at 35% 30%, #d5d8dc, #7f8c8d); }
    .semaforo-light.idle { background: radial-gradient(circle at 35% 30%, #eaecee, #b0b7bd); }
    .semaforo-label {
      font-size: .9rem; font-weight: 700; letter-spacing: .05em; text-transform: uppercase; line-height: 1.1;
    }
    .semaforo-label.on { color: var(--noc-on); }
    .semaforo-label.off { color: var(--noc-off); }
    .semaforo-label.unknown, .semaforo-label.idle { color: var(--noc-unknown); }
    .live-warn {
      background: #fef5e7; border: 1px solid #f0b27a; color: #6e2c00;
      padding: .2rem .4rem; border-radius: .2rem; font-size: .68rem; margin-bottom: .25rem;
    }
    .status-toolbar {
      display: flex; align-items: center; gap: .3rem; flex-wrap: wrap;
      margin: .15rem 0 0;
    }
    .status-toolbar .shiny-input-container { margin-bottom: 0 !important; }
    .pill-toggles {
      display: inline-flex; align-items: center; gap: .25rem; flex-wrap: wrap;
    }
    .pill-toggles .form-group,
    .pill-toggles .shiny-input-container { margin-bottom: 0 !important; }
    .pill-toggles .form-check {
      padding-left: 0; margin: 0; min-height: 0; display: inline-flex;
    }
    .pill-toggles .form-check-input {
      position: absolute; opacity: 0; width: 0; height: 0; pointer-events: none;
    }
    .pill-toggles .form-check-label {
      display: inline-flex; align-items: center; gap: .2rem;
      margin: 0; cursor: pointer; user-select: none;
      font-size: .62rem; font-weight: 700; letter-spacing: .02em;
      text-transform: uppercase;
      padding: .15rem .45rem; border-radius: 999px;
      border: 1px solid #c5d0da; background: #eef2f5; color: #566573;
    }
    .pill-toggles .form-check-label::before {
      content: ''; width: .35rem; height: .35rem; border-radius: 50%;
      background: #95a5a6; flex-shrink: 0;
    }
    .pill-toggles .form-check-input:checked + .form-check-label {
      background: #1a5f7a; border-color: #154b61; color: #fff;
    }
    .pill-toggles .form-check-input:checked + .form-check-label::before {
      background: #82e0aa;
    }
    .panel-scroll { max-height: 140px; overflow: auto; }
    .prb-grid-controls {
      display: grid; grid-template-columns: 1fr 1fr auto auto;
      gap: .25rem .35rem; align-items: end; margin-bottom: .25rem;
    }
    .prb-cards {
      display: grid; grid-template-columns: repeat(3, 1fr); gap: .3rem; margin-bottom: .25rem;
    }
    .prb-card {
      border: 1px solid var(--noc-line); border-radius: .25rem; padding: .3rem .45rem;
      background: #f8fafb; border-left: 4px solid #95a5a6;
      text-align: center;
    }
    .prb-card.ok { border-left-color: var(--noc-on); background: #eafaf1; }
    .prb-card.warn { border-left-color: #d68910; background: #fef9e7; }
    .prb-card.crit { border-left-color: var(--noc-off); background: #fdedec; }
    .prb-card.na { border-left-color: #95a5a6; }
    .prb-card.users { border-left-color: #1a5f7a; background: #eaf2f5; }
    .prb-card .prb-title { font-size: .6rem; text-transform: uppercase; letter-spacing: .04em; color: var(--noc-muted); font-weight: 700; }
    .prb-card .prb-value {
      font-size: 1.55rem; font-weight: 800; line-height: 1.05;
      font-variant-numeric: tabular-nums; margin: .05rem 0;
    }
    .prb-card.ok .prb-value { color: var(--noc-on); }
    .prb-card.warn .prb-value { color: #b9770e; }
    .prb-card.crit .prb-value { color: var(--noc-off); }
    .prb-card.na .prb-value { color: var(--noc-unknown); }
    .prb-card.users .prb-value { color: #1a5f7a; }
    .prb-card .prb-unit { font-size: .75rem; font-weight: 700; margin-left: .12rem; }
    .prb-card .prb-meta { font-size: .62rem; color: var(--noc-muted); margin-top: 0; }
    .prb-card .prb-stats { font-size: .65rem; margin-top: .1rem; font-weight: 600; }
    .prb-source { font-size: .62rem; color: var(--noc-muted); margin: .1rem 0 .15rem; }
    .hist-details summary {
      cursor: pointer; font-size: .65rem; font-weight: 700; text-transform: uppercase;
      letter-spacing: .06em; color: var(--noc-accent);
    }
    @media (max-width: 1100px) {
      .grid-top { grid-template-columns: 1fr 1fr 1fr; }
      .prb-grid-controls { grid-template-columns: 1fr 1fr; }
      .prb-cards { grid-template-columns: 1fr; }
    }
  ")

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_fluid(
  theme = noc_theme,
  tags$head(tags$title("Psicos - Sector Power")),
  div(
    class = "app-header",
    h1("Psicos - Sector Power"),
    p(class = "subtitle", "NOC · NetAct")
  ),
  uiOutput("busy_banner"),

  # --- Conexión en una sola franja ---
  div(
    class = "noc-card",
    h2("Conexión"),
    div(
      class = "grid-top",
      selectInput(
        "url_scheme", "Esquema",
        choices = c("http" = "http", "https" = "https"),
        selected = DEFAULT_SCHEME,
        width = "100%"
      ),
      textInput(
        "api_host", "Host",
        value = DEFAULT_HOST,
        placeholder = "172.22.129.126",
        width = "100%"
      ),
      numericInput(
        "api_port", "Puerto",
        value = DEFAULT_PORT_N,
        min = 1, max = 65535, step = 1,
        width = "100%"
      ),
      passwordInput("bearer_token", "Bearer", value = DEFAULT_TOKEN, width = "100%"),
      textInput("chaos_user", "Usuario", value = DEFAULT_USER, width = "100%"),
      passwordInput("chaos_pass", "Password", value = DEFAULT_PASS, width = "100%"),
      div(
        class = "conn-actions",
        tags$label(class = "form-label", `for` = "btn_ping", "Acciones"),
        div(
          class = "row-actions",
          actionButton("btn_ping", "Conectar", class = "btn-secondary btn-sm", icon = icon("plug")),
          actionButton("btn_save_token", "Guardar", class = "btn-outline-secondary btn-sm", icon = icon("floppy-disk")),
          uiOutput("ping_indicator")
        )
      )
    ),
    div(style = "display:flex; gap:.5rem; flex-wrap:wrap; align-items:center;",
      uiOutput("effective_url"),
      uiOutput("ping_detail")
    )
  ),

  layout_columns(
    col_widths = c(4, 4, 4),
    div(
      class = "noc-card",
      h2("Operación"),
      div(
        class = "grid-sector",
        textInput(
          "cell_name", "cell_name",
          value = "",
          placeholder = "CAL.Ingenio_S1",
          width = "100%"
        ),
        selectInput(
          "tech", "Tech",
          choices = c("Auto" = "auto", "4G" = "4G", "5G" = "5G"),
          selected = "auto",
          width = "100%"
        )
      ),
      div(
        class = "btn-ops",
        actionButton("btn_on", "Encender", class = "btn-power-on", icon = icon("bolt")),
        actionButton("btn_off", "Apagar", class = "btn-power-off", icon = icon("power-off"))
      ),
      div(
        class = "status-toolbar",
        div(
          class = "pill-toggles",
          checkboxInput("live_netact", "Live", value = TRUE),
          checkboxInput("auto_refresh", "Auto", value = TRUE)
        ),
        conditionalPanel(
          condition = "input.auto_refresh == true",
          selectInput(
            "refresh_interval", NULL,
            choices = c("15s" = 15, "30s" = 30, "60s" = 60),
            selected = 15,
            width = "72px"
          )
        ),
        actionButton("btn_status", "Estado", class = "btn-primary btn-sm", icon = icon("satellite-dish"))
      )
    ),
    div(
      class = "noc-card",
      h2("Estado"),
      uiOutput("status_panel")
    ),
    div(
      class = "noc-card",
      h2("Resultado"),
      div(class = "panel-scroll", uiOutput("result_panel"))
    )
  ),

  # --- KPI unificado ---
  div(
    class = "noc-card",
    h2("KPI LTE · Usuarios + PRB"),
    {
      win0 <- default_kpi_window_24h()
      div(
        class = "prb-grid-controls",
        textInput("kpi_date_from", "from", value = win0$date_from, width = "100%"),
        textInput("kpi_date_to", "to", value = win0$date_to, width = "100%"),
        actionButton(
          "btn_kpi", "Consultar KPI",
          class = "btn-primary btn-sm", icon = icon("chart-line")
        ),
        div(
          class = "status-toolbar",
          div(
            class = "pill-toggles",
            checkboxInput("kpi_auto_refresh", "Auto KPI", value = TRUE)
          ),
          conditionalPanel(
            condition = "input.kpi_auto_refresh == true",
            selectInput(
              "kpi_refresh_interval", NULL,
              choices = c("15s" = 15, "30s" = 30, "60s" = 60),
              selected = 15,
              width = "72px"
            )
          )
        )
      )
    },
    uiOutput("kpi_cards_ui"),
    plotlyOutput("kpi_plot", height = "240px"),
    uiOutput("kpi_detail_ui")
  ),

  tags$details(
    class = "noc-card hist-details",
    tags$summary("Historial de sesión"),
    div(class = "history-wrap", style = "margin-top:.25rem;", uiOutput("history_panel"))
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  ping_state <- reactiveVal(list(status = "idle", message = "Sin conectar"))
  last_result <- reactiveVal(NULL)
  last_status <- reactiveVal(NULL)
  last_prb <- reactiveVal(NULL)
  last_prb_fetch_ts <- reactiveVal(0)
  last_ue <- reactiveVal(NULL)
  last_ue_fetch_ts <- reactiveVal(0)
  busy <- reactiveVal(FALSE)
  busy_msg <- reactiveVal("")
  history <- reactiveVal(data.frame(
    hora = character(),
    cell_name = character(),
    accion = character(),
    exito = logical(),
    mensaje = character(),
    stringsAsFactors = FALSE
  ))

  pending_action <- reactiveVal(NULL)

  current_base_url <- reactive({
    build_base_url(input$api_host, input$api_port, input$url_scheme)
  })

  output$effective_url <- renderUI({
    tags$p(
      class = "url-chip",
      "URL: ", tags$code(current_base_url())
    )
  })

  tech_arg <- function() {
    if (identical(input$tech, "auto")) NULL else input$tech
  }

  append_history <- function(cell_name, accion, exito, mensaje) {
    row <- data.frame(
      hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      cell_name = as.character(cell_name),
      accion = as.character(accion),
      exito = isTRUE(exito),
      mensaje = substr(as.character(mensaje %||% ""), 1, 120),
      stringsAsFactors = FALSE
    )
    hist <- rbind(row, history())
    if (nrow(hist) > HISTORY_MAX) hist <- hist[seq_len(HISTORY_MAX), , drop = FALSE]
    history(hist)
  }

  credentials_ok <- function(notify = TRUE) {
    if (!nzchar(trimws(input$bearer_token %||% ""))) {
      if (notify) showNotification("Configure el Bearer token.", type = "error", duration = 5)
      return(FALSE)
    }
    if (!nzchar(trimws(input$chaos_user %||% "")) || !nzchar(input$chaos_pass %||% "")) {
      if (notify) showNotification("Configure usuario y contraseña Chaos.", type = "error", duration = 5)
      return(FALSE)
    }
    TRUE
  }

  run_status_query <- function(quiet = FALSE, record_history = TRUE) {
    cell <- trimws(input$cell_name %||% "")
    v <- validate_cell_name(cell)
    if (!v$ok) {
      if (!quiet) showNotification(v$message, type = "error", duration = 6)
      return(invisible(NULL))
    }
    if (!credentials_ok(notify = !quiet)) return(invisible(NULL))

    live <- isTRUE(input$live_netact)
    timeout <- if (live) 120 else 30
    if (!quiet) {
      busy(TRUE)
      busy_msg(if (live) {
        "Consultando estado Live NetAct… (hasta 120 s)"
      } else {
        "Consultando estado del sector…"
      })
    }

    res <- tryCatch(
      sector_status(
        base_url = current_base_url(),
        token = input$bearer_token,
        username = input$chaos_user,
        password = input$chaos_pass,
        cell_name = v$cell_name,
        tech = tech_arg(),
        live = live,
        timeout = timeout
      ),
      error = function(e) {
        list(
          success = FALSE, status_code = NA_integer_, body = NULL,
          message = paste("Error:", conditionMessage(e)),
          elapsed_sec = NA_real_,
          request_meta = list(cell_name = v$cell_name, live = live)
        )
      }
    )

    if (!quiet) {
      busy(FALSE)
      busy_msg("")
    }

    last_status(res)
    if (isTRUE(record_history)) {
      append_history(
        v$cell_name,
        if (live) "Estado (live)" else "Estado",
        isTRUE(res$success),
        res$message
      )
    }

    if (!quiet) {
      showNotification(
        if (isTRUE(res$success)) (res$message %||% "Estado actualizado.") else paste("Estado:", res$message),
        type = if (isTRUE(res$success)) "message" else "error",
        duration = 6
      )
    }
    invisible(res)
  }

  output$busy_banner <- renderUI({
    if (!isTRUE(busy())) return(NULL)
    div(
      class = "busy-banner",
      icon("spinner", class = "fa-spin"),
      span(busy_msg() %||% "Procesando…")
    )
  })

  output$ping_indicator <- renderUI({
    st <- ping_state()
    cls <- switch(st$status, ok = "ok", bad = "bad", "idle")
    label <- switch(st$status, ok = "Conectado", bad = "Sin conexión", "Sin conectar")
    span(class = paste("status-pill", cls), span(class = "status-dot"), label)
  })

  output$ping_detail <- renderUI({
    st <- ping_state()
    if (identical(st$status, "idle")) return(NULL)
    tags$span(
      style = "font-size:.72rem; color:#5d6d7e;",
      st$message
    )
  })

  observeEvent(input$btn_save_token, {
    token <- as.character(input$bearer_token %||% "")
    user <- trimws(input$chaos_user %||% "")
    pass <- as.character(input$chaos_pass %||% "")
    if (!nzchar(trimws(token))) {
      showNotification("Ingrese un Bearer token antes de guardar.", type = "error", duration = 5)
      return()
    }
    if (!nzchar(user) || !nzchar(pass)) {
      showNotification("Ingrese usuario y contraseña Chaos antes de guardar.", type = "error", duration = 5)
      return()
    }
    host <- trimws(input$api_host %||% "")
    if (!nzchar(host)) {
      showNotification("Host / IP vacío.", type = "error", duration = 5)
      return()
    }
    base <- current_base_url()
    port_s <- as.character(as.integer(input$api_port %||% DEFAULT_PORT))
    tryCatch(
      {
        upsert_renviron(LOCAL_RENVIRON, list(
          CHAOS_BASE_URL = base,
          CHAOS_PORT = port_s,
          CHAOS_BEARER_TOKEN = token,
          CHAOS_USERNAME = user,
          CHAOS_PASSWORD = pass
        ))
        # Disponible en la sesión actual sin reiniciar
        Sys.setenv(
          CHAOS_BASE_URL = base,
          CHAOS_PORT = port_s,
          CHAOS_BEARER_TOKEN = token,
          CHAOS_USERNAME = user,
          CHAOS_PASSWORD = pass
        )
        showNotification(
          "Credenciales guardadas en .Renviron local (Bearer, usuario y contraseña Chaos).",
          type = "message",
          duration = 6
        )
      },
      error = function(e) {
        showNotification(
          paste("No se pudo guardar:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      }
    )
  })

  observeEvent(input$btn_ping, {
    busy(TRUE)
    busy_msg("Conectando…")
    on.exit({
      busy(FALSE)
      busy_msg("")
    }, add = TRUE)
    res <- ping_api(current_base_url(), input$bearer_token, timeout = 15)
    ping_state(list(
      status = if (isTRUE(res$success)) "ok" else "bad",
      message = res$message %||% ""
    ))
  })

  # Solicitud de acción → modal de confirmación
  request_operation <- function(desired_on) {
    cell <- trimws(input$cell_name %||% "")
    v <- validate_cell_name(cell)
    if (!v$ok) {
      showNotification(v$message, type = "error", duration = 6)
      return()
    }
    if (!credentials_ok()) return()

    pending_action(list(desired_on = desired_on, cell_name = v$cell_name))
    accion <- if (desired_on) "ENCENDER" else "APAGAR"
    warn_extra <- if (!desired_on) {
      tags$p(class = "confirm-warn", "Atención: apagar el sector puede afectar el servicio de radio.")
    } else {
      NULL
    }

    showModal(modalDialog(
      title = paste("Confirmar:", accion, "sector"),
      tags$p(
        "¿Confirma ", tags$strong(accion), " el sector ",
        tags$code(v$cell_name), "?"
      ),
      tags$p(
        style = "font-size:.9rem; color:#5d6d7e;",
        "Tecnología: ", tags$strong(input$tech),
        " · desired_on = ", tags$code(tolower(as.character(desired_on)))
      ),
      warn_extra,
      footer = tagList(
        modalButton("Cancelar"),
        actionButton(
          "btn_confirm",
          if (desired_on) "Sí, encender" else "Sí, apagar",
          class = if (desired_on) "btn-success" else "btn-danger"
        )
      ),
      easyClose = FALSE
    ))
  }

  observeEvent(input$btn_on,  { request_operation(TRUE) })
  observeEvent(input$btn_off, { request_operation(FALSE) })

  observeEvent(input$btn_confirm, {
    act <- pending_action()
    removeModal()
    if (is.null(act)) return()

    busy(TRUE)
    busy_msg("Enviando a NetAct… espere (hasta 120 s).")
    withProgress(message = "Enviando a NetAct…", value = 0.3, {
      res <- sector_power(
        base_url = current_base_url(),
        token = input$bearer_token,
        username = input$chaos_user,
        password = input$chaos_pass,
        cell_name = act$cell_name,
        desired_on = act$desired_on,
        tech = tech_arg(),
        timeout = 120
      )
      setProgress(1)
    })
    busy(FALSE)
    busy_msg("")
    last_result(res)

    append_history(
      act$cell_name,
      if (isTRUE(act$desired_on)) "Encender" else "Apagar",
      isTRUE(res$success),
      res$message
    )
    pending_action(NULL)

    showNotification(
      if (isTRUE(res$success)) "Operación exitosa." else paste("Falló:", res$message),
      type = if (isTRUE(res$success)) "message" else "error",
      duration = 8
    )

    # Tras encender/apagar exitoso: refrescar estado automáticamente
    if (isTRUE(res$success)) {
      run_status_query(quiet = FALSE)
    }
  })

  observeEvent(input$btn_status, {
    run_status_query(quiet = FALSE)
  })

  # Al terminar de digitar cell_name (pausa ~0.9 s), consultar estado por defecto
  cell_name_debounced <- debounce(
    reactive(trimws(input$cell_name %||% "")),
    millis = 900
  )
  last_auto_status_cell <- reactiveVal("")

  observeEvent(cell_name_debounced(), {
    cell <- cell_name_debounced()
    v <- validate_cell_name(cell)
    if (!v$ok) return()
    if (identical(v$cell_name, last_auto_status_cell())) return()
    if (!credentials_ok(notify = FALSE)) return()
    if (isTRUE(busy())) return()
    last_auto_status_cell(v$cell_name)
    run_status_query(quiet = TRUE, record_history = FALSE)
  }, ignoreInit = TRUE)

  run_kpi_query <- function(quiet = FALSE, record_history = TRUE) {
    cell <- trimws(input$cell_name %||% "")
    v <- validate_cell_name(cell)
    if (!v$ok) {
      if (!quiet) showNotification(v$message, type = "error", duration = 6)
      return(invisible(NULL))
    }
    if (!credentials_ok(notify = !quiet)) return(invisible(NULL))

    if (!quiet) {
      busy(TRUE)
      busy_msg("Consultando KPI LTE (usuarios + PRB)… hasta 60 s")
    }

    if (isTRUE(quiet)) {
      win <- default_kpi_window_24h()
      date_from <- win$date_from
      date_to <- win$date_to
      updateTextInput(session, "kpi_date_from", value = date_from)
      updateTextInput(session, "kpi_date_to", value = date_to)
    } else {
      date_from <- input$kpi_date_from
      date_to <- input$kpi_date_to
    }

    common <- list(
      base_url = current_base_url(),
      token = input$bearer_token,
      username = input$chaos_user,
      password = input$chaos_pass,
      cell_name = v$cell_name,
      date_from = date_from,
      date_to = date_to,
      timeout = 60
    )

    res_ue <- tryCatch(
      do.call(kpi_lte_717b, common),
      error = function(e) list(
        success = FALSE, status_code = NA_integer_, body = NULL,
        message = paste("Error UE:", conditionMessage(e)), elapsed_sec = NA_real_
      )
    )
    res_prb <- tryCatch(
      kpi_lte_prb(
        base_url = common$base_url,
        token = common$token,
        username = common$username,
        password = common$password,
        cell_name = common$cell_name,
        kpis = c("LTE_PRB_DL", "LTE_PRB_UL"),
        date_from = common$date_from,
        date_to = common$date_to,
        timeout = common$timeout
      ),
      error = function(e) list(
        success = FALSE, status_code = NA_integer_, body = NULL,
        message = paste("Error PRB:", conditionMessage(e)), elapsed_sec = NA_real_
      )
    )

    if (!quiet) {
      busy(FALSE)
      busy_msg("")
    }

    last_ue(res_ue)
    last_prb(res_prb)
    now <- as.numeric(Sys.time())
    last_ue_fetch_ts(now)
    last_prb_fetch_ts(now)

    ok_any <- isTRUE(res_ue$success) || isTRUE(res_prb$success)
    msg <- paste(
      if (isTRUE(res_ue$success)) (res_ue$message %||% "UE OK") else paste("UE:", res_ue$message %||% "error"),
      if (isTRUE(res_prb$success)) (res_prb$message %||% "PRB OK") else paste("PRB:", res_prb$message %||% "error"),
      sep = " · "
    )

    if (isTRUE(record_history)) {
      append_history(v$cell_name, "KPI LTE", ok_any, msg)
    }

    if (!quiet) {
      showNotification(
        msg,
        type = if (ok_any) "message" else "error",
        duration = 7
      )
    }
    invisible(list(ue = res_ue, prb = res_prb))
  }

  observeEvent(input$btn_kpi, {
    run_kpi_query(quiet = FALSE)
  })

  # Auto-refresh periódico de estado
  observe({
    if (!isTRUE(input$auto_refresh)) return()
    interval_ms <- as.integer(input$refresh_interval %||% 15) * 1000L
    if (is.na(interval_ms) || interval_ms < 5000L) interval_ms <- 15000L
    invalidateLater(interval_ms)
    if (isTRUE(busy())) return()
    isolate({
      run_status_query(quiet = TRUE, record_history = FALSE)
      # Acompañar estado con KPI unificado según intervalo KPI (default 15 s)
      now <- as.numeric(Sys.time())
      kpi_iv <- as.integer(input$kpi_refresh_interval %||% 15)
      if (is.na(kpi_iv) || kpi_iv < 15) kpi_iv <- 15L
      last_kpi <- max(last_prb_fetch_ts() %||% 0, last_ue_fetch_ts() %||% 0)
      if ((now - last_kpi) >= kpi_iv) {
        run_kpi_query(quiet = TRUE, record_history = FALSE)
      }
    })
  })

  # Auto-refresh dedicado KPI (default 15 s)
  observe({
    if (!isTRUE(input$kpi_auto_refresh)) return()
    interval_ms <- as.integer(input$kpi_refresh_interval %||% 15) * 1000L
    if (is.na(interval_ms) || interval_ms < 15000L) interval_ms <- 15000L
    invalidateLater(interval_ms)
    if (isTRUE(busy())) return()
    isolate(run_kpi_query(quiet = TRUE, record_history = FALSE))
  })

  output$status_panel <- renderUI({
    res <- last_status()
    if (is.null(res)) {
      return(tagList(
        div(
          class = "semaforo",
          div(class = "semaforo-light idle"),
          div(class = "semaforo-label idle", "SIN CONSULTA")
        ),
        div(class = "result-idle", "Use «Estado» o active auto-refresh.")
      ))
    }

    if (!isTRUE(res$success)) {
      return(tagList(
        div(
          class = "semaforo",
          div(class = "semaforo-light unknown"),
          div(class = "semaforo-label unknown", "ERROR")
        ),
        div(
          class = "result-err",
          tags$strong("Error de estado"),
          tags$p(style = "margin:.2rem 0 0;", res$message),
          tags$span(
            style = "font-size:.72rem; color:#5d6d7e;",
            sprintf("HTTP %s · %.1f s", res$status_code %||% "—", res$elapsed_sec %||% NA_real_)
          )
        ),
        tags$details(
          class = "raw-json",
          tags$summary("JSON"),
          tags$pre(redact_json(res$body))
        )
      ))
    }

    body <- res$body
    cls <- classify_power_state(body)
    kind <- cls$kind
    fields <- extract_status_fields(body)
    meta_keys <- c("region", "dn", "estado_adm", "estado_oper", "status_source", "monitored_at", "tech")
    meta <- fields[intersect(meta_keys, names(fields))]

    warn_ui <- NULL
    lw <- body$live_warning
    if (!is.null(lw) && !identical(lw, FALSE) && nzchar(as.character(lw))) {
      warn_ui <- div(class = "live-warn", icon("triangle-exclamation"), " ", as.character(lw))
    }

    meta_ui <- NULL
    if (length(meta)) {
      meta_ui <- div(
        class = "field-grid",
        lapply(names(meta), function(nm) {
          val <- meta[[nm]]
          if (is.logical(val)) val <- tolower(as.character(val))
          if (is.list(val) || length(val) > 1) val <- toJSON(val, auto_unbox = TRUE)
          div(
            class = "field-item",
            div(class = "lbl", nm),
            div(class = "val", as.character(val))
          )
        })
      )
    }

    tagList(
      div(
        class = "semaforo",
        div(class = paste("semaforo-light", kind)),
        div(
          div(class = paste("semaforo-label", kind), cls$label),
          tags$div(
            style = "font-size:.72rem; color:#5d6d7e; margin-top:.1rem;",
            sprintf("%s · HTTP %s · %.1f s", res$message %||% "", res$status_code %||% "—", res$elapsed_sec %||% NA_real_)
          )
        )
      ),
      warn_ui,
      meta_ui,
      tags$details(
        class = "raw-json",
        tags$summary("JSON"),
        tags$pre(redact_json(body))
      )
    )
  })

  output$result_panel <- renderUI({
    res <- last_result()
    if (is.null(res)) {
      return(div(class = "result-idle", "Sin operaciones aún."))
    }

    ok <- isTRUE(res$success)
    wrap_cls <- if (ok) "result-ok" else "result-err"
    fields <- extract_result_fields(res$body)

    field_uis <- NULL
    if (length(fields)) {
      field_uis <- div(
        class = "field-grid",
        style = "margin-top:.35rem;",
        lapply(names(fields), function(nm) {
          val <- fields[[nm]]
          if (is.logical(val)) val <- tolower(as.character(val))
          if (is.list(val) || length(val) > 1) val <- toJSON(val, auto_unbox = TRUE)
          div(
            class = "field-item",
            div(class = "lbl", nm),
            div(class = "val", as.character(val))
          )
        })
      )
    }

    tagList(
      div(
        class = wrap_cls,
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:.4rem; flex-wrap:wrap;",
          tags$strong(if (ok) "Éxito" else "Error"),
          tags$span(
            style = "font-size:.72rem; color:#5d6d7e;",
            sprintf("HTTP %s · %.1f s", res$status_code %||% "—", res$elapsed_sec %||% NA_real_)
          )
        ),
        tags$p(style = "margin:.2rem 0 0;", res$message)
      ),
      field_uis,
      tags$details(
        class = "raw-json",
        tags$summary("JSON"),
        tags$pre(redact_json(res$body))
      )
    )
  })

  metric_card <- function(title, value, unit, when = "", stats = NULL, sev = "ok", extra_class = "") {
    stats_txt <- if (!is.null(stats)) {
      sprintf(
        "min %s · avg %s · max %s",
        format(round(as.numeric(stats$min %||% NA), 1), nsmall = 1),
        format(round(as.numeric(stats$avg %||% NA), 1), nsmall = 1),
        format(round(as.numeric(stats$max %||% NA), 1), nsmall = 1)
      )
    } else {
      ""
    }
    val_num <- suppressWarnings(as.numeric(value))
    val_txt <- if (length(val_num) == 0 || is.na(val_num)) "—" else format(round(val_num, 1), nsmall = 1)
    div(
      class = paste("prb-card", sev, extra_class),
      div(class = "prb-title", title),
      div(class = "prb-value", val_txt, span(class = "prb-unit", unit)),
      div(class = "prb-meta", if (nzchar(when)) paste("Último:", when) else "Sin datos"),
      if (nzchar(stats_txt)) div(class = "prb-stats", stats_txt)
    )
  }

  output$kpi_cards_ui <- renderUI({
    res_ue <- last_ue()
    res_prb <- last_prb()
    if (is.null(res_ue) && is.null(res_prb)) {
      return(div(class = "result-idle", "Pulse «Consultar KPI» para ver usuarios y PRB."))
    }

    # Usuarios
    ue_block <- if (isTRUE(res_ue$success)) res_ue$body$kpis$LTE_717b else NULL
    ue_latest <- if (!is.null(ue_block)) ue_block$latest else NULL
    ue_val <- if (!is.null(ue_latest)) scalar_num(ue_latest$value) else NA_real_
    ue_when <- if (!is.null(ue_latest)) scalar_chr(ue_latest$period_start_time) else ""
    ue_card <- metric_card(
      "Usuarios activos",
      ue_val,
      "usuarios",
      when = ue_when,
      stats = if (!is.null(ue_block)) ue_block$stats else NULL,
      sev = if (is.null(ue_block)) "na" else "ok",
      extra_class = "users"
    )

    # PRB DL / UL
    prb_kpis <- if (isTRUE(res_prb$success)) {
      k <- res_prb$body$kpis
      if (is.null(k)) list() else k
    } else {
      list()
    }
    mk_prb <- function(code, title) {
      block <- prb_kpis[[code]]
      if (is.null(block)) {
        return(metric_card(title, NA, "%", sev = "na"))
      }
      latest <- block$latest
      val <- if (!is.null(latest)) scalar_num(latest$value) else NA_real_
      when <- if (!is.null(latest)) scalar_chr(latest$period_start_time) else ""
      metric_card(title, val, "%", when = when, stats = block$stats, sev = prb_severity(val))
    }

    errs <- NULL
    if (!is.null(res_ue) && !isTRUE(res_ue$success)) {
      errs <- c(errs, paste("UE:", scalar_chr(res_ue$message, "error")))
    }
    if (!is.null(res_prb) && !isTRUE(res_prb$success)) {
      errs <- c(errs, paste("PRB:", scalar_chr(res_prb$message, "error")))
    }

    ue_src <- if (isTRUE(res_ue$success)) {
      src <- res_ue$body$source_tables
      if (is.null(src) || length(src) == 0) src <- res_ue$body$source_table
      format_source_tables(src)
    } else {
      "—"
    }
    prb_src <- if (isTRUE(res_prb$success)) {
      src <- res_prb$body$source_table
      if (is.null(src) || length(src) == 0) "RAW" else format_source_tables(src)
    } else {
      "—"
    }

    tagList(
      div(class = "prb-cards", ue_card, mk_prb("LTE_PRB_DL", "PRB Downlink"), mk_prb("LTE_PRB_UL", "PRB Uplink")),
      if (!is.null(errs)) div(class = "result-err", paste(errs, collapse = " · ")),
      tags$p(class = "prb-source", sprintf("UE: %s · PRB: %s", ue_src, prb_src))
    )
  })

  output$kpi_plot <- renderPlotly({
    res_ue <- last_ue()
    res_prb <- last_prb()
    empty <- plot_ly() |>
      layout(
        title = list(text = "Sin series KPI", font = list(size = 12)),
        yaxis = list(range = c(0, 100), title = "PRB (%)"),
        margin = list(l = 40, r = 40, t = 20, b = 28),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#f8fafb"
      ) |>
      config(displayModeBar = FALSE)

    has_prb <- isTRUE(res_prb$success)
    has_ue <- isTRUE(res_ue$success)
    if (!has_prb && !has_ue) return(empty)

    df_prb_raw <- if (has_prb) extract_prb_timeseries(res_prb$body) else NULL
    df_ue_raw <- if (has_ue) extract_717b_timeseries(res_ue$body) else NULL
    built <- build_kpi_operability(df_prb_raw, df_ue_raw, interval_min = 15L)
    grid <- built$grid
    df_prb <- built$df_prb
    df_ue <- built$df_ue
    if (!nrow(grid)) return(empty)

    p <- plot_ly()

    # Banda de no operatividad (huecos sin KPI = sector apagado / sin reporting)
    p <- add_trace(
      p,
      data = grid,
      x = ~period_start_time,
      y = ~oper_band,
      type = "scatter",
      mode = "lines",
      name = "Apagado / sin data",
      yaxis = "y",
      line = list(width = 0, shape = "hv", color = "rgba(192,57,43,0)"),
      fill = "tozeroy",
      fillcolor = "rgba(192, 57, 43, 0.18)",
      hoverinfo = "skip",
      showlegend = TRUE
    )

    # Línea de operatividad (1=ON, 0=OFF) en eje derecho auxiliar escalado
    p <- add_trace(
      p,
      data = grid,
      x = ~period_start_time,
      y = ~operativo,
      type = "scatter",
      mode = "lines",
      name = "Operativo",
      yaxis = "y3",
      line = list(width = 2.5, color = "#c0392b", shape = "hv"),
      hovertemplate = paste0(
        "Operativo: %{customdata}<br>%{x}<extra></extra>"
      ),
      customdata = ifelse(grid$operativo == 1L, "SÍ", "NO")
    )

    colors <- c("PRB DL (%)" = "#1e7a46", "PRB UL (%)" = "#d68910")
    if (nrow(df_prb)) {
      for (s in unique(df_prb$serie)) {
        sub <- df_prb[df_prb$serie == s, , drop = FALSE]
        sub <- sub[order(sub$period_start_time), , drop = FALSE]
        p <- add_trace(
          p,
          data = sub,
          x = ~period_start_time,
          y = ~value,
          type = "scatter",
          mode = "lines+markers",
          name = s,
          yaxis = "y",
          connectgaps = FALSE,
          line = list(width = 2, color = colors[[s]] %||% "#34495e"),
          marker = list(size = 5),
          hovertemplate = paste0(s, ": %{y:.1f}%<br>%{x}<extra></extra>")
        )
      }
    }

    if (nrow(df_ue)) {
      df_ue <- df_ue[order(df_ue$period_start_time), , drop = FALSE]
      p <- add_trace(
        p,
        data = df_ue,
        x = ~period_start_time,
        y = ~value,
        type = "scatter",
        mode = "lines+markers",
        name = "Usuarios (#)",
        yaxis = "y2",
        connectgaps = FALSE,
        line = list(width = 2.5, color = "#1a5f7a"),
        marker = list(size = 5, color = "#1a5f7a"),
        hovertemplate = "Usuarios: %{y:.1f}<br>%{x}<extra></extra>"
      )
    }

    p |>
      layout(
        yaxis = list(
          title = "Carga PRB (%)",
          range = c(0, 100),
          side = "left",
          gridcolor = "#e5e8eb"
        ),
        yaxis2 = list(
          title = "Usuarios activos (#)",
          overlaying = "y",
          side = "right",
          rangemode = "tozero",
          showgrid = FALSE
        ),
        yaxis3 = list(
          title = "",
          overlaying = "y",
          side = "right",
          range = c(-0.05, 1.35),
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          position = 0.98
        ),
        xaxis = list(title = "", gridcolor = "#e5e8eb"),
        legend = list(orientation = "h", y = 1.14, x = 0, font = list(size = 10)),
        margin = list(l = 40, r = 50, t = 28, b = 28),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#f8fafb",
        hovermode = "x unified"
      ) |>
      config(displayModeBar = FALSE)
  })

  output$kpi_detail_ui <- renderUI({
    res_ue <- last_ue()
    res_prb <- last_prb()
    parts <- list()

    if (isTRUE(res_prb$success)) {
      df <- extract_prb_timeseries(res_prb$body)
      if (!is.null(df) && nrow(df) > 0) {
        df <- df[order(df$period_start_time, decreasing = TRUE), , drop = FALSE]
        show_n <- min(nrow(df), 150L)
        df_show <- df[seq_len(show_n), , drop = FALSE]
        rows <- lapply(seq_len(nrow(df_show)), function(i) {
          r <- df_show[i, ]
          tags$tr(
            tags$td(format(r$period_start_time, "%Y-%m-%d %H:%M:%S")),
            tags$td(r$kpi),
            tags$td(sprintf("%.1f", r$value))
          )
        })
        parts <- c(parts, list(tags$details(
          class = "raw-json",
          tags$summary(sprintf("Detalle PRB (%d)", show_n)),
          div(
            class = "history-wrap",
            tags$table(
              class = "table table-sm table-striped history-table",
              tags$thead(tags$tr(tags$th("period"), tags$th("kpi"), tags$th("%"))),
              tags$tbody(rows)
            )
          )
        )))
      }
    }

    if (isTRUE(res_ue$success)) {
      df <- extract_717b_timeseries(res_ue$body)
      if (!is.null(df) && nrow(df) > 0) {
        df <- df[order(df$period_start_time, decreasing = TRUE), , drop = FALSE]
        show_n <- min(nrow(df), 150L)
        df_show <- df[seq_len(show_n), , drop = FALSE]
        rows <- lapply(seq_len(nrow(df_show)), function(i) {
          r <- df_show[i, ]
          tags$tr(
            tags$td(format(r$period_start_time, "%Y-%m-%d %H:%M:%S")),
            tags$td(sprintf("%.1f", r$value))
          )
        })
        parts <- c(parts, list(tags$details(
          class = "raw-json",
          tags$summary(sprintf("Detalle usuarios (%d)", show_n)),
          div(
            class = "history-wrap",
            tags$table(
              class = "table table-sm table-striped history-table",
              tags$thead(tags$tr(tags$th("period"), tags$th("usuarios"))),
              tags$tbody(rows)
            )
          )
        )))
      }
    }

    if (!length(parts)) return(NULL)
    do.call(tagList, parts)
  })

  output$history_panel <- renderUI({
    hist <- history()
    if (is.null(hist) || nrow(hist) == 0) {
      return(tags$p(style = "color:#5d6d7e; margin:0;", "Sin operaciones todavía."))
    }
    rows <- lapply(seq_len(nrow(hist)), function(i) {
      r <- hist[i, ]
      tags$tr(
        tags$td(r$hora),
        tags$td(tags$code(r$cell_name)),
        tags$td(r$accion),
        tags$td(
          if (isTRUE(r$exito)) {
            span(class = "status-pill ok", "OK")
          } else {
            span(class = "status-pill bad", "Error")
          }
        ),
        tags$td(r$mensaje)
      )
    })
    tags$table(
      class = "table table-sm table-striped history-table",
      tags$thead(tags$tr(
        tags$th("Hora"), tags$th("cell_name"), tags$th("Acción"),
        tags$th("Éxito"), tags$th("Mensaje")
      )),
      tags$tbody(rows)
    )
  })
}

shinyApp(ui, server)

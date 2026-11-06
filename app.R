# =============================================================================
# Chaos NetAct — Sector Power Control (Shiny)
# UI operativa NOC: conectar, operar sector, ver resultado e historial.
# =============================================================================

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)

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

DEFAULT_BASE_URL <- env_or("CHAOS_BASE_URL", "http://172.22.129.126:8001")
DEFAULT_TOKEN    <- env_or("CHAOS_BEARER_TOKEN", "")
DEFAULT_USER     <- env_or("CHAOS_USERNAME", "")

HISTORY_MAX <- 20L

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
  "border-radius" = "0.35rem",
  "font-size-base" = "0.95rem"
) |>
  bs_add_rules("
    :root {
      --noc-bg: #e8edf1;
      --noc-panel: #ffffff;
      --noc-ink: #1c2833;
      --noc-muted: #5d6d7e;
      --noc-line: #d5dde5;
      --noc-on: #1e7a46;
      --noc-off: #c0392b;
      --noc-accent: #1a5f7a;
    }
    body { background: linear-gradient(165deg, #dfe6ed 0%, #eef2f5 45%, #d4e0e8 100%);
           color: var(--noc-ink); min-height: 100vh; }
    .app-header {
      background: linear-gradient(90deg, #1c2833 0%, #1a5f7a 100%);
      color: #f4f7f9; padding: 1rem 1.25rem; margin: -0.5rem -0.75rem 1.25rem;
      border-bottom: 3px solid #148f77; box-shadow: 0 2px 8px rgba(28,40,51,.25);
    }
    .app-header h1 { font-size: 1.35rem; margin: 0; font-weight: 600; letter-spacing: .02em; }
    .app-header .subtitle { font-size: .85rem; opacity: .85; margin: .2rem 0 0; }
    .noc-card {
      background: var(--noc-panel); border: 1px solid var(--noc-line);
      border-radius: .4rem; padding: 1.1rem 1.2rem; margin-bottom: 1rem;
      box-shadow: 0 1px 3px rgba(28,40,51,.06);
    }
    .noc-card h2 {
      font-size: 1rem; font-weight: 600; text-transform: uppercase;
      letter-spacing: .06em; color: var(--noc-accent); margin: 0 0 .9rem;
      padding-bottom: .45rem; border-bottom: 2px solid var(--noc-line);
    }
    .status-pill {
      display: inline-flex; align-items: center; gap: .4rem;
      font-size: .8rem; font-weight: 600; padding: .25rem .65rem;
      border-radius: 999px; border: 1px solid transparent;
    }
    .status-pill.ok { background: #d5f5e3; color: #145a32; border-color: #82e0aa; }
    .status-pill.bad { background: #fadbd8; color: #7b241c; border-color: #f1948a; }
    .status-pill.idle { background: #eaecee; color: #566573; border-color: #ccd1d1; }
    .status-dot { width: .55rem; height: .55rem; border-radius: 50%; background: currentColor; }
    .btn-power-on {
      background: var(--noc-on) !important; border-color: #196f3d !important;
      color: #fff !important; font-weight: 600; min-height: 3rem; font-size: 1.05rem;
    }
    .btn-power-on:hover { background: #196f3d !important; }
    .btn-power-off {
      background: #d68910 !important; border-color: #b9770e !important;
      color: #fff !important; font-weight: 600; min-height: 3rem; font-size: 1.05rem;
    }
    .btn-power-off:hover { background: #b9770e !important; }
    .result-ok { border-left: 4px solid var(--noc-on); background: #eafaf1; padding: .85rem 1rem; }
    .result-err { border-left: 4px solid var(--noc-off); background: #fdedec; padding: .85rem 1rem; }
    .result-idle { border-left: 4px solid #95a5a6; background: #f4f6f7; padding: .85rem 1rem; color: var(--noc-muted); }
    .field-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); gap: .5rem .75rem; }
    .field-item { background: #f8fafb; border: 1px solid var(--noc-line); padding: .45rem .6rem; border-radius: .3rem; }
    .field-item .lbl { font-size: .7rem; text-transform: uppercase; color: var(--noc-muted); letter-spacing: .04em; }
    .field-item .val { font-size: .9rem; font-weight: 600; word-break: break-all; }
    .history-table { font-size: .85rem; }
    .history-table th { background: #1c2833; color: #fff; }
    details.raw-json { margin-top: .75rem; font-size: .8rem; }
    details.raw-json summary { cursor: pointer; color: var(--noc-accent); font-weight: 600; }
    details.raw-json pre {
      background: #1c2833; color: #d5dbdb; padding: .75rem; border-radius: .3rem;
      max-height: 280px; overflow: auto; margin-top: .4rem;
    }
    .busy-banner {
      display: flex; align-items: center; gap: .75rem; padding: .75rem 1rem;
      background: #d4e6f1; border: 1px solid #7fb3d5; border-radius: .35rem;
      color: #1a5276; font-weight: 600; margin-bottom: 1rem;
    }
    .confirm-warn { color: #7b241c; font-weight: 600; }
  ")

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_fluid(
  theme = noc_theme,
  tags$head(tags$title("Chaos · Sector Power")),
  div(
    class = "app-header",
    h1("Chaos · Control de potencia de sector"),
    p(class = "subtitle", "Operación NetAct vía API externa — NOC")
  ),
  uiOutput("busy_banner"),
  layout_columns(
    col_widths = c(4, 4, 4),
    # --- Conexión ---
    div(
      class = "noc-card",
      h2("1 · Conexión"),
      textInput("base_url", "Base URL", value = DEFAULT_BASE_URL, width = "100%"),
      passwordInput("bearer_token", "Bearer token", value = DEFAULT_TOKEN, width = "100%"),
      textInput("chaos_user", "Usuario Chaos (Administrador)", value = DEFAULT_USER, width = "100%"),
      passwordInput("chaos_pass", "Contraseña Chaos", value = "", width = "100%"),
      div(
        style = "display:flex; align-items:center; gap:.75rem; flex-wrap:wrap; margin-top:.25rem;",
        actionButton("btn_ping", "Probar conexión", class = "btn-secondary", icon = icon("plug")),
        uiOutput("ping_indicator")
      ),
      uiOutput("ping_detail")
    ),
    # --- Operación ---
    div(
      class = "noc-card",
      h2("2 · Operación de sector"),
      textInput(
        "cell_name", "cell_name",
        value = "",
        placeholder = "CAL.Ingenio_S1",
        width = "100%"
      ),
      selectInput(
        "tech", "Tecnología",
        choices = c("Auto" = "auto", "4G" = "4G", "5G" = "5G"),
        selected = "auto",
        width = "100%"
      ),
      helpText("No se envían region_code ni dn; la API los resuelve. Timeout: 120 s."),
      div(
        style = "display:grid; grid-template-columns:1fr 1fr; gap:.6rem; margin-top:.5rem;",
        actionButton("btn_on", "Encender sector", class = "btn-power-on", icon = icon("bolt")),
        actionButton("btn_off", "Apagar sector", class = "btn-power-off", icon = icon("power-off"))
      )
    ),
    # --- Resultado ---
    div(
      class = "noc-card",
      h2("3 · Resultado"),
      uiOutput("result_panel")
    )
  ),
  div(
    class = "noc-card",
    h2("Historial de sesión"),
    uiOutput("history_panel")
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  ping_state <- reactiveVal(list(status = "idle", message = "Sin probar"))
  last_result <- reactiveVal(NULL)
  busy <- reactiveVal(FALSE)
  history <- reactiveVal(data.frame(
    hora = character(),
    cell_name = character(),
    accion = character(),
    exito = logical(),
    mensaje = character(),
    stringsAsFactors = FALSE
  ))

  pending_action <- reactiveVal(NULL) # list(desired_on = TRUE/FALSE)

  output$busy_banner <- renderUI({
    if (!isTRUE(busy())) return(NULL)
    div(
      class = "busy-banner",
      icon("spinner", class = "fa-spin"),
      span("Enviando a NetAct… espere (hasta 120 s).")
    )
  })

  output$ping_indicator <- renderUI({
    st <- ping_state()
    cls <- switch(st$status, ok = "ok", bad = "bad", "idle")
    label <- switch(st$status, ok = "Conectado", bad = "Sin conexión", "Sin probar")
    span(class = paste("status-pill", cls), span(class = "status-dot"), label)
  })

  output$ping_detail <- renderUI({
    st <- ping_state()
    if (identical(st$status, "idle")) return(NULL)
    tags$p(
      style = "margin:.6rem 0 0; font-size:.85rem; color:#5d6d7e;",
      st$message
    )
  })

  observeEvent(input$btn_ping, {
    busy(TRUE)
    on.exit(busy(FALSE), add = TRUE)
    res <- ping_api(input$base_url, input$bearer_token, timeout = 15)
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
    if (!nzchar(trimws(input$bearer_token %||% ""))) {
      showNotification("Configure el Bearer token.", type = "error", duration = 5)
      return()
    }
    if (!nzchar(trimws(input$chaos_user %||% "")) || !nzchar(input$chaos_pass %||% "")) {
      showNotification("Configure usuario y contraseña Chaos.", type = "error", duration = 5)
      return()
    }

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
    # Ejecutar fuera del hilo reactivo inmediato vía isolate + withProgress
    withProgress(message = "Enviando a NetAct…", value = 0.3, {
      tech_arg <- if (identical(input$tech, "auto")) NULL else input$tech
      res <- sector_power(
        base_url = input$base_url,
        token = input$bearer_token,
        username = input$chaos_user,
        password = input$chaos_pass,
        cell_name = act$cell_name,
        desired_on = act$desired_on,
        tech = tech_arg,
        timeout = 120
      )
      setProgress(1)
    })
    busy(FALSE)
    last_result(res)

    # Historial (sin secretos)
    row <- data.frame(
      hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      cell_name = act$cell_name,
      accion = if (isTRUE(act$desired_on)) "Encender" else "Apagar",
      exito = isTRUE(res$success),
      mensaje = substr(as.character(res$message %||% ""), 1, 120),
      stringsAsFactors = FALSE
    )
    hist <- rbind(row, history())
    if (nrow(hist) > HISTORY_MAX) hist <- hist[seq_len(HISTORY_MAX), , drop = FALSE]
    history(hist)
    pending_action(NULL)

    showNotification(
      if (isTRUE(res$success)) "Operación exitosa." else paste("Falló:", res$message),
      type = if (isTRUE(res$success)) "message" else "error",
      duration = 8
    )
  })

  output$result_panel <- renderUI({
    res <- last_result()
    if (is.null(res)) {
      return(div(class = "result-idle", "Aún no hay operaciones en esta sesión."))
    }

    ok <- isTRUE(res$success)
    wrap_cls <- if (ok) "result-ok" else "result-err"
    fields <- extract_result_fields(res$body)

    field_uis <- NULL
    if (length(fields)) {
      field_uis <- div(
        class = "field-grid",
        style = "margin-top:.65rem;",
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

    raw_json <- tryCatch(
      toJSON(res$body, pretty = TRUE, auto_unbox = TRUE, null = "null"),
      error = function(e) "{}"
    )

    # Redactar posibles secretos si la API los ecoara (defensa en profundidad)
    raw_json <- gsub(
      '"(password|token|bearer|Authorization)"\\s*:\\s*"[^"]*"',
      '"\\1":"***"',
      raw_json,
      ignore.case = TRUE
    )

    tagList(
      div(
        class = wrap_cls,
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:.5rem; flex-wrap:wrap;",
          tags$strong(if (ok) "Éxito" else "Error"),
          tags$span(
            style = "font-size:.8rem; color:#5d6d7e;",
            sprintf("HTTP %s · %.1f s", res$status_code %||% "—", res$elapsed_sec %||% NA_real_)
          )
        ),
        tags$p(style = "margin:.4rem 0 0;", res$message)
      ),
      field_uis,
      tags$details(
        class = "raw-json",
        tags$summary("JSON crudo (diagnóstico)"),
        tags$pre(raw_json)
      )
    )
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

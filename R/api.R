# =============================================================================
# Chaos / NetAct — Sector Power API client
# No imprime ni registra Bearer token ni contraseñas.
# =============================================================================

#' Valida formato razonable de cell_name (sitio_sector con "_").
#' @param cell_name character
#' @return list(ok = logical, message = character)
validate_cell_name <- function(cell_name) {
  cell_name <- trimws(as.character(cell_name %||% ""))
  if (!nzchar(cell_name)) {
    return(list(ok = FALSE, message = "cell_name es obligatorio."))
  }
  if (!grepl("_", cell_name, fixed = TRUE)) {
    return(list(
      ok = FALSE,
      message = "cell_name debe separar sitio y sector con '_' (ej. CAL.Ingenio_S1)."
    ))
  }
  parts <- strsplit(cell_name, "_", fixed = TRUE)[[1]]
  if (length(parts) < 2 || !nzchar(parts[[1]]) || !nzchar(parts[[length(parts)]])) {
    return(list(
      ok = FALSE,
      message = "cell_name inválido: se espera formato sitio_sector (ej. CAQ.La Argentina_L2)."
    ))
  }
  list(ok = TRUE, message = "", cell_name = cell_name)
}

#' Fallback seguro ante NULL / vacío / NA; tolera vectores (no usa && con length>1).
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1L) {
    if (is.na(x)) return(y)
    if (is.character(x) && !nzchar(x)) return(y)
  }
  x
}

#' Extrae un escalar character para UI (evita length>1 en if/nzchar).
scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  x <- x[[1]]
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(default)
  out <- as.character(x)
  if (!nzchar(out)) default else out
}

#' Extrae un escalar numeric para UI.
scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (length(v) == 0 || is.na(v)) default else v
}

#' Normaliza URL base (sin barra final).
normalize_base_url <- function(base_url) {
  url <- trimws(as.character(base_url %||% ""))
  url <- sub("/+$", "", url)
  url
}

#' Mensaje amigable según código HTTP / cuerpo de error.
friendly_http_error <- function(status, body = NULL) {
  code <- body$error %||% body$code %||% body$detail %||% NULL
  msg  <- body$message %||% body$error_description %||% body$detail %||% NULL
  if (is.list(msg)) msg <- paste(unlist(msg), collapse = "; ")
  if (is.list(code)) code <- paste(unlist(code), collapse = "; ")

  base <- switch(
    as.character(status),
    "400" = "Solicitud inválida (validación de parámetros).",
    "401" = "No autorizado: Bearer inválido o credenciales Chaos incorrectas.",
    "403" = "Prohibido: sin credenciales NetAct guardadas, usuario inactivo o sin permisos.",
    "404" = "Recurso no encontrado (endpoint o tabla RAW ausente).",
    "502" = "Error de NetAct / consulta KPI o gateway (502).",
    "504" = "Timeout hacia NetAct (504).",
    sprintf("Error HTTP %s.", status %||% "?")
  )

  extras <- character(0)
  if (!is.null(code) && nzchar(as.character(code))) {
    extras <- c(extras, paste0("código: ", code))
  }
  if (!is.null(msg) && nzchar(as.character(msg))) {
    extras <- c(extras, as.character(msg))
  }

  # Matchear códigos típicos mencionados en el contrato
  blob <- tolower(paste(c(code, msg), collapse = " "))
  hint <- NULL
  if (grepl("invalid_bearer", blob)) hint <- "Bearer token inválido o ausente."
  if (grepl("invalid_credentials", blob)) hint <- "Usuario o contraseña Chaos incorrectos."
  if (grepl("missing_netact_credentials", blob)) {
    hint <- "El usuario Chaos no tiene credenciales NetAct guardadas para el RC."
  }
  if (grepl("user_inactive", blob)) hint <- "Usuario Chaos inactivo."
  if (grepl("forbidden", blob) && is.null(hint)) hint <- "Acceso denegado (403)."
  if (grepl("missing_site", blob)) hint <- "Falta cell_name o site_name."
  if (grepl("invalid_kpis", blob)) hint <- "KPI inválidos (use LTE_PRB_DL / LTE_PRB_UL)."
  if (grepl("raw_table_missing", blob)) hint <- "Tabla RAW de KPI no disponible en el servidor."
  if (grepl("kpi_query_error", blob)) hint <- "Error al consultar KPI en el servidor (502)."

  if (!is.null(hint)) {
    paste(hint, if (length(extras)) paste0("(", paste(extras, collapse = " | "), ")") else "")
  } else if (length(extras)) {
    paste(base, paste(extras, collapse = " | "))
  } else {
    base
  }
}

#' Intenta parsear JSON; si falla, devuelve lista con raw.
safe_parse_json <- function(text) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) return(list())
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = TRUE, simplifyDataFrame = FALSE),
    error = function(e) list(raw = text, parse_error = conditionMessage(e))
  )
}

#' GET ping / health del servicio de sector-power.
#'
#' @param base_url Base URL del API (ej. http://172.22.129.126:8001)
#' @param token Bearer token del aplicativo externo
#' @param timeout Segundos de timeout (default 15)
#' @return list(success, status_code, body, message, elapsed_sec)
ping_api <- function(base_url, token, timeout = 15) {
  base_url <- normalize_base_url(base_url)
  token <- trimws(as.character(token %||% ""))

  if (!nzchar(base_url)) {
    return(list(
      success = FALSE,
      status_code = NA_integer_,
      body = NULL,
      message = "Base URL vacía.",
      elapsed_sec = NA_real_
    ))
  }
  if (!nzchar(token)) {
    return(list(
      success = FALSE,
      status_code = NA_integer_,
      body = NULL,
      message = "Bearer token vacío.",
      elapsed_sec = NA_real_
    ))
  }

  url <- paste0(base_url, "/api/external/netact/sector-power/ping/")
  t0 <- proc.time()[["elapsed"]]

  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_headers(Authorization = paste("Bearer", token)) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body_text <- httr2::resp_body_string(resp)
      body <- safe_parse_json(body_text)
      ok <- status >= 200 && status < 300 && isTRUE(body$success %||% FALSE)

      list(
        success = ok,
        status_code = status,
        body = body,
        message = if (ok) {
          paste0(
            "Conexión OK",
            if (!is.null(body$service)) paste0(" — ", body$service) else "",
            if (!is.null(body$auth)) paste0(" (", body$auth, ")") else ""
          )
        } else {
          friendly_http_error(status, body)
        },
        elapsed_sec = proc.time()[["elapsed"]] - t0
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        status_code = NA_integer_,
        body = NULL,
        message = paste("Error de red o timeout en ping:", conditionMessage(e)),
        elapsed_sec = proc.time()[["elapsed"]] - t0
      )
    }
  )

  result
}

#' Encender o apagar un sector vía API Chaos / NetAct.
#'
#' @param base_url Base URL
#' @param token Bearer token
#' @param username Usuario Chaos (Administrador)
#' @param password Contraseña Chaos
#' @param cell_name Nombre de celda sitio_sector
#' @param desired_on TRUE = encender (unlock), FALSE = apagar (lock)
#' @param tech Opcional: "4G" | "5G" (NULL = Auto, no se envía)
#' @param timeout Timeout en segundos (recomendado 120)
#' @return list(success, status_code, body, message, elapsed_sec, request_meta)
sector_power <- function(base_url, token, username, password, cell_name,
                         desired_on, tech = NULL, timeout = 120) {
  base_url <- normalize_base_url(base_url)
  token <- trimws(as.character(token %||% ""))
  username <- trimws(as.character(username %||% ""))
  password <- as.character(password %||% "")
  desired_on <- isTRUE(desired_on)

  v <- validate_cell_name(cell_name)
  if (!v$ok) {
    return(list(
      success = FALSE,
      status_code = 400L,
      body = list(error = "validation", message = v$message),
      message = v$message,
      elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, desired_on = desired_on, tech = tech)
    ))
  }
  cell_name <- v$cell_name

  if (!nzchar(base_url)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Base URL vacía.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, desired_on = desired_on)
    ))
  }
  if (!nzchar(token)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Bearer token vacío.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, desired_on = desired_on)
    ))
  }
  if (!nzchar(username) || !nzchar(password)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Usuario y contraseña Chaos son obligatorios.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, desired_on = desired_on)
    ))
  }

  payload <- list(
    username = username,
    password = password,
    cell_name = cell_name,
    desired_on = desired_on
  )

  tech <- trimws(as.character(tech %||% ""))
  if (nzchar(tech) && !identical(tolower(tech), "auto")) {
    payload$tech <- tech
  }

  url <- paste0(base_url, "/api/external/netact/sector-power/")
  t0 <- proc.time()[["elapsed"]]

  # Metadatos seguros (sin secretos) para UI / historial
  request_meta <- list(
    cell_name = cell_name,
    desired_on = desired_on,
    action = if (desired_on) "encender" else "apagar",
    tech = if (nzchar(tech) && !identical(tolower(tech), "auto")) tech else "auto"
  )

  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body_text <- httr2::resp_body_string(resp)
      body <- safe_parse_json(body_text)
      ok <- status >= 200 && status < 300 && isTRUE(body$success %||% (status < 300))

      # Algunas APIs marcan success en el cuerpo; priorizar cuerpo si existe
      if (!is.null(body$success)) {
        ok <- isTRUE(body$success) && status >= 200 && status < 300
      }

      msg <- if (ok) {
        body$message %||% sprintf(
          "Operación %s completada para %s.",
          request_meta$action, cell_name
        )
      } else {
        friendly_http_error(status, body)
      }

      list(
        success = ok,
        status_code = status,
        body = body,
        message = as.character(msg),
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      timed_out <- grepl("timeout|timed out|Timeout", err_msg, ignore.case = TRUE)
      list(
        success = FALSE,
        status_code = if (timed_out) 504L else NA_integer_,
        body = NULL,
        message = if (timed_out) {
          paste("Timeout esperando respuesta de NetAct (recomendado 120s):", err_msg)
        } else {
          paste("Error de red:", err_msg)
        },
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    }
  )

  result
}

#' Extrae campos relevantes de una respuesta exitosa para mostrar en UI.
extract_result_fields <- function(body) {
  if (is.null(body) || !is.list(body)) return(list())
  keys <- c(
    "region", "dn", "cell_name", "plan_name", "confId", "operationId",
    "estado_adm", "desired_on_effective", "cm_status", "tech", "action",
    "desired_on"
  )
  out <- list()
  for (k in keys) {
    if (!is.null(body[[k]])) out[[k]] <- body[[k]]
  }
  # nested data / result
  for (nest in c("data", "result", "payload")) {
    if (is.list(body[[nest]])) {
      for (k in keys) {
        if (is.null(out[[k]]) && !is.null(body[[nest]][[k]])) {
          out[[k]] <- body[[nest]][[k]]
        }
      }
    }
  }
  out
}

#' Consulta estado de potencia de un sector (POST JSON; sin password en query).
#'
#' @param base_url Base URL
#' @param token Bearer token
#' @param username Usuario Chaos
#' @param password Contraseña Chaos
#' @param cell_name Nombre sitio_sector
#' @param tech Opcional "4g"|"5g" (NULL = no enviar)
#' @param live TRUE = enriquecer con NetAct CM en vivo
#' @param site_name Opcional si no se infiere
#' @param timeout Segundos; default 30 (OSS) o 120 (live)
#' @return list(success, status_code, body, message, elapsed_sec, request_meta)
sector_status <- function(base_url, token, username, password, cell_name,
                          tech = NULL, live = FALSE, site_name = NULL,
                          timeout = NULL) {
  base_url <- normalize_base_url(base_url)
  token <- trimws(as.character(token %||% ""))
  username <- trimws(as.character(username %||% ""))
  password <- as.character(password %||% "")
  live <- isTRUE(live)
  if (is.null(timeout) || is.na(timeout)) {
    timeout <- if (live) 120 else 30
  }

  v <- validate_cell_name(cell_name)
  if (!v$ok) {
    return(list(
      success = FALSE,
      status_code = 400L,
      body = list(error = "validation", message = v$message),
      message = v$message,
      elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, live = live)
    ))
  }
  cell_name <- v$cell_name

  if (!nzchar(base_url)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Base URL vacía.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, live = live)
    ))
  }
  if (!nzchar(token)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Bearer token vacío.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, live = live)
    ))
  }
  if (!nzchar(username) || !nzchar(password)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Usuario y contraseña Chaos son obligatorios.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, live = live)
    ))
  }

  payload <- list(
    username = username,
    password = password,
    cell_name = cell_name,
    live = live
  )

  tech <- trimws(as.character(tech %||% ""))
  if (nzchar(tech) && !identical(tolower(tech), "auto")) {
    payload$tech <- tolower(tech)
  }

  site_name <- trimws(as.character(site_name %||% ""))
  if (nzchar(site_name)) {
    payload$site_name <- site_name
  }

  url <- paste0(base_url, "/api/external/netact/sector-power/status/")
  t0 <- proc.time()[["elapsed"]]
  request_meta <- list(
    cell_name = cell_name,
    live = live,
    tech = if (nzchar(tech) && !identical(tolower(tech), "auto")) tolower(tech) else "auto",
    action = "status"
  )

  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body <- safe_parse_json(httr2::resp_body_string(resp))
      ok <- status >= 200 && status < 300
      if (!is.null(body$success)) {
        ok <- isTRUE(body$success) && ok
      }

      msg <- if (ok) {
        body$message %||% sprintf("Estado consultado para %s.", cell_name)
      } else {
        friendly_http_error(status, body)
      }

      list(
        success = ok,
        status_code = status,
        body = body,
        message = as.character(msg),
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      timed_out <- grepl("timeout|timed out|Timeout", err_msg, ignore.case = TRUE)
      list(
        success = FALSE,
        status_code = if (timed_out) 504L else NA_integer_,
        body = NULL,
        message = if (timed_out) {
          paste("Timeout consultando estado:", err_msg)
        } else {
          paste("Error de red:", err_msg)
        },
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    }
  )

  result
}

#' Clasifica el estado de potencia para UI (on / off / unknown).
#' @return list(kind, label, power_on)
classify_power_state <- function(body) {
  if (is.null(body) || !is.list(body)) {
    return(list(kind = "unknown", label = "DESCONOCIDO", power_on = NA))
  }
  power_on <- body$power_on
  power_state <- tolower(as.character(body$power_state %||% ""))
  power_label <- body$power_label %||% NULL

  if (identical(power_state, "unknown") || is.null(power_on)) {
    label <- if (!is.null(power_label) && nzchar(as.character(power_label))) {
      toupper(as.character(power_label))
    } else {
      "DESCONOCIDO"
    }
    return(list(kind = "unknown", label = label, power_on = NA))
  }

  if (isTRUE(power_on) || identical(power_state, "on")) {
    label <- if (!is.null(power_label) && nzchar(as.character(power_label))) {
      toupper(as.character(power_label))
    } else {
      "ENCENDIDO"
    }
    return(list(kind = "on", label = label, power_on = TRUE))
  }

  if (identical(power_on, FALSE) || identical(power_state, "off")) {
    label <- if (!is.null(power_label) && nzchar(as.character(power_label))) {
      toupper(as.character(power_label))
    } else {
      "APAGADO"
    }
    return(list(kind = "off", label = label, power_on = FALSE))
  }

  list(kind = "unknown", label = "DESCONOCIDO", power_on = NA)
}

#' Campos de estado a mostrar en UI.
extract_status_fields <- function(body) {
  if (is.null(body) || !is.list(body)) return(list())
  keys <- c(
    "cell_name", "site_name", "sector", "region", "dn", "tech",
    "power_on", "power_state", "power_label", "estado_adm", "estado_oper",
    "status_source", "live_requested", "monitored_at"
  )
  out <- list()
  for (k in keys) {
    if (!is.null(body[[k]])) out[[k]] <- body[[k]]
  }
  out
}

#' Normaliza aliases de KPI PRB a códigos canónicos.
normalize_prb_kpis <- function(kpis) {
  if (is.null(kpis) || length(kpis) == 0) {
    return(c("LTE_PRB_DL", "LTE_PRB_UL"))
  }
  raw <- toupper(trimws(as.character(kpis)))
  out <- unique(vapply(raw, function(k) {
    switch(
      k,
      "LTE_PRB_DL" = "LTE_PRB_DL",
      "PRB_DL" = "LTE_PRB_DL",
      "DL" = "LTE_PRB_DL",
      "LTE_PRB_UL" = "LTE_PRB_UL",
      "PRB_UL" = "LTE_PRB_UL",
      "UL" = "LTE_PRB_UL",
      k
    )
  }, character(1), USE.NAMES = FALSE))
  valid <- out[out %in% c("LTE_PRB_DL", "LTE_PRB_UL")]
  if (!length(valid)) c("LTE_PRB_DL", "LTE_PRB_UL") else valid
}

#' Valida rango de fechas KPI (máx. 3 días).
validate_kpi_date_range <- function(date_from, date_to) {
  parse_ts <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (!nzchar(x)) return(NULL)
    # Intentar varios formatos
    for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
      t <- as.POSIXct(x, format = fmt, tz = "UTC")
      if (!is.na(t)) return(t)
    }
    t <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (is.na(t)) NULL else t
  }
  t0 <- parse_ts(date_from)
  t1 <- parse_ts(date_to)
  if (is.null(t0) || is.null(t1)) {
    return(list(ok = FALSE, message = "Fechas inválidas. Use YYYY-MM-DD o YYYY-MM-DD HH:MI[:SS]."))
  }
  if (t1 < t0) {
    return(list(ok = FALSE, message = "date_to debe ser posterior a date_from."))
  }
  span_days <- as.numeric(difftime(t1, t0, units = "days"))
  if (span_days > 3.0001) {
    return(list(ok = FALSE, message = "El rango máximo permitido es de 3 días."))
  }
  list(
    ok = TRUE,
    message = "",
    date_from = format(t0, "%Y-%m-%d %H:%M:%S"),
    date_to = format(t1, "%Y-%m-%d %H:%M:%S")
  )
}

#' Default últimas 24 h en hora local del servidor.
default_kpi_window_24h <- function() {
  t1 <- Sys.time()
  t0 <- t1 - 24 * 3600
  list(
    date_from = format(t0, "%Y-%m-%d %H:%M:%S"),
    date_to = format(t1, "%Y-%m-%d %H:%M:%S")
  )
}

#' Consulta KPI de carga PRB LTE (POST JSON).
#'
#' @param base_url Base URL
#' @param token Bearer token
#' @param username Usuario Chaos
#' @param password Contraseña Chaos
#' @param cell_name Opcional sitio_sector (requerido si no hay site_name)
#' @param site_name Opcional nombre de sitio (requerido si no hay cell_name)
#' @param kpis Vector de KPI (default ambos DL/UL)
#' @param date_from / date_to Opcionales; default últimas 24 h; máx. 3 días
#' @param timeout Segundos (recomendado 60)
#' @return list(success, status_code, body, message, elapsed_sec, request_meta)
kpi_lte_prb <- function(base_url, token, username, password,
                        cell_name = NULL, site_name = NULL,
                        kpis = c("LTE_PRB_DL", "LTE_PRB_UL"),
                        date_from = NULL, date_to = NULL,
                        timeout = 60) {
  base_url <- normalize_base_url(base_url)
  token <- trimws(as.character(token %||% ""))
  username <- trimws(as.character(username %||% ""))
  password <- as.character(password %||% "")
  cell_name <- trimws(as.character(cell_name %||% ""))
  site_name <- trimws(as.character(site_name %||% ""))
  kpis <- normalize_prb_kpis(kpis)

  if (!nzchar(cell_name) && !nzchar(site_name)) {
    return(list(
      success = FALSE, status_code = 400L, body = NULL,
      message = "Debe indicar cell_name o site_name.",
      elapsed_sec = NA_real_,
      request_meta = list(kpis = kpis)
    ))
  }
  if (nzchar(cell_name)) {
    v <- validate_cell_name(cell_name)
    if (!v$ok) {
      return(list(
        success = FALSE, status_code = 400L,
        body = list(error = "validation", message = v$message),
        message = v$message, elapsed_sec = NA_real_,
        request_meta = list(cell_name = cell_name, kpis = kpis)
      ))
    }
    cell_name <- v$cell_name
  }

  if (!nzchar(base_url)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Base URL vacía.", elapsed_sec = NA_real_,
      request_meta = list(kpis = kpis)
    ))
  }
  if (!nzchar(token)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Bearer token vacío.", elapsed_sec = NA_real_,
      request_meta = list(kpis = kpis)
    ))
  }
  if (!nzchar(username) || !nzchar(password)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Usuario y contraseña Chaos son obligatorios.", elapsed_sec = NA_real_,
      request_meta = list(kpis = kpis)
    ))
  }

  # Fechas: default 24 h
  if (!nzchar(trimws(as.character(date_from %||% ""))) ||
      !nzchar(trimws(as.character(date_to %||% "")))) {
    win <- default_kpi_window_24h()
    date_from <- win$date_from
    date_to <- win$date_to
  }
  dv <- validate_kpi_date_range(date_from, date_to)
  if (!dv$ok) {
    return(list(
      success = FALSE, status_code = 400L,
      body = list(error = "validation", message = dv$message),
      message = dv$message, elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, site_name = site_name, kpis = kpis)
    ))
  }
  date_from <- dv$date_from
  date_to <- dv$date_to

  payload <- list(
    username = username,
    password = password,
    kpis = as.list(kpis),
    date_from = date_from,
    date_to = date_to
  )
  if (nzchar(cell_name)) payload$cell_name <- cell_name
  if (nzchar(site_name)) payload$site_name <- site_name

  url <- paste0(base_url, "/api/external/kpi/lte-prb/")
  t0 <- proc.time()[["elapsed"]]
  request_meta <- list(
    cell_name = if (nzchar(cell_name)) cell_name else NULL,
    site_name = if (nzchar(site_name)) site_name else NULL,
    kpis = kpis,
    date_from = date_from,
    date_to = date_to,
    action = "kpi_lte_prb"
  )

  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body <- safe_parse_json(httr2::resp_body_string(resp))
      ok <- status >= 200 && status < 300
      if (!is.null(body$success)) {
        ok <- isTRUE(body$success) && ok
      }

      msg <- if (ok) {
        body$message %||% sprintf(
          "KPI PRB OK%s",
          if (nzchar(cell_name)) paste0(" — ", cell_name) else ""
        )
      } else {
        friendly_http_error(status, body)
      }

      list(
        success = ok,
        status_code = status,
        body = body,
        message = as.character(msg),
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      timed_out <- grepl("timeout|timed out|Timeout", err_msg, ignore.case = TRUE)
      list(
        success = FALSE,
        status_code = if (timed_out) 504L else NA_integer_,
        body = NULL,
        message = if (timed_out) {
          paste("Timeout consultando KPI PRB:", err_msg)
        } else {
          paste("Error de red:", err_msg)
        },
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    }
  )

  result
}

#' Severidad de carga PRB (%) para UI.
#' @return "ok" | "warn" | "crit" | "na"
prb_severity <- function(value) {
  v <- suppressWarnings(as.numeric(value))
  if (length(v) == 0 || is.na(v)) return("na")
  if (v >= 80) "crit" else if (v >= 70) "warn" else "ok"
}

#' Convierte bloque KPI (points) a data.frame para gráfico/tabla.
kpi_points_to_df <- function(kpi_block, kpi_code) {
  if (is.null(kpi_block) || !is.list(kpi_block)) {
    return(data.frame(
      period_start_time = as.POSIXct(character()),
      value = numeric(),
      kpi = character(),
      lncel_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  pts <- kpi_block$points
  if (is.null(pts) || length(pts) == 0) {
    return(data.frame(
      period_start_time = as.POSIXct(character()),
      value = numeric(),
      kpi = character(),
      lncel_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  # fromJSON puede devolver data.frame o lista de listas
  if (is.data.frame(pts)) {
    df <- pts
  } else {
    df <- do.call(rbind, lapply(pts, function(p) {
      data.frame(
        period_start_time = as.character(p$period_start_time %||% NA),
        value = suppressWarnings(as.numeric(p$value %||% NA)),
        lncel_name = as.character(p$lncel_name %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  if (!"period_start_time" %in% names(df)) df$period_start_time <- NA_character_
  if (!"value" %in% names(df)) df$value <- NA_real_
  if (!"lncel_name" %in% names(df)) df$lncel_name <- NA_character_
  df$value <- suppressWarnings(as.numeric(df$value))
  ts <- suppressWarnings(as.POSIXct(df$period_start_time, tz = "UTC"))
  if (all(is.na(ts))) {
    ts <- suppressWarnings(as.POSIXct(df$period_start_time))
  }
  data.frame(
    period_start_time = ts,
    value = df$value,
    kpi = kpi_code,
    lncel_name = as.character(df$lncel_name),
    stringsAsFactors = FALSE
  )
}

#' Extrae data.frame combinado DL/UL desde body de kpi_lte_prb.
extract_prb_timeseries <- function(body) {
  kpis <- body$kpis
  if (is.null(kpis) || !is.list(kpis)) {
    return(data.frame(
      period_start_time = as.POSIXct(character()),
      value = numeric(),
      kpi = character(),
      lncel_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  parts <- list()
  for (code in c("LTE_PRB_DL", "LTE_PRB_UL")) {
    if (!is.null(kpis[[code]])) {
      parts[[code]] <- kpi_points_to_df(kpis[[code]], code)
    }
  }
  if (!length(parts)) {
    return(data.frame(
      period_start_time = as.POSIXct(character()),
      value = numeric(),
      kpi = character(),
      lncel_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, parts)
}

#' Consulta KPI LTE_717b — Avg active users per cell (POST JSON).
#'
#' Fuente RAW 15 min (no hour). Timeout recomendado 60 s.
#'
#' @param base_url Base URL
#' @param token Bearer token
#' @param username Usuario Chaos Administrador
#' @param password Contraseña Chaos
#' @param cell_name Obligatorio sitio_sector
#' @param date_from / date_to Opcionales; default 24 h; máx. 3 días
#' @param site_name Opcional si no se infiere
#' @param timeout Segundos (default 60)
#' @return list(success, status_code, body, message, elapsed_sec, request_meta)
kpi_lte_717b <- function(base_url, token, username, password, cell_name,
                         date_from = NULL, date_to = NULL,
                         site_name = NULL, timeout = 60) {
  base_url <- normalize_base_url(base_url)
  token <- trimws(as.character(token %||% ""))
  username <- trimws(as.character(username %||% ""))
  password <- as.character(password %||% "")
  cell_name <- trimws(as.character(cell_name %||% ""))
  site_name <- trimws(as.character(site_name %||% ""))

  if (!nzchar(cell_name)) {
    return(list(
      success = FALSE, status_code = 400L, body = NULL,
      message = "cell_name es obligatorio.",
      elapsed_sec = NA_real_,
      request_meta = list(action = "kpi_lte_717b")
    ))
  }
  v <- validate_cell_name(cell_name)
  if (!v$ok) {
    return(list(
      success = FALSE, status_code = 400L,
      body = list(error = "validation", message = v$message),
      message = v$message, elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name, action = "kpi_lte_717b")
    ))
  }
  cell_name <- v$cell_name

  if (!nzchar(base_url)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Base URL vacía.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name)
    ))
  }
  if (!nzchar(token)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Bearer token vacío.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name)
    ))
  }
  if (!nzchar(username) || !nzchar(password)) {
    return(list(
      success = FALSE, status_code = NA_integer_, body = NULL,
      message = "Usuario y contraseña Chaos son obligatorios.", elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name)
    ))
  }

  if (!nzchar(trimws(as.character(date_from %||% ""))) ||
      !nzchar(trimws(as.character(date_to %||% "")))) {
    win <- default_kpi_window_24h()
    date_from <- win$date_from
    date_to <- win$date_to
  }
  dv <- validate_kpi_date_range(date_from, date_to)
  if (!dv$ok) {
    return(list(
      success = FALSE, status_code = 400L,
      body = list(error = "validation", message = dv$message),
      message = dv$message, elapsed_sec = NA_real_,
      request_meta = list(cell_name = cell_name)
    ))
  }
  date_from <- dv$date_from
  date_to <- dv$date_to

  payload <- list(
    username = username,
    password = password,
    cell_name = cell_name,
    date_from = date_from,
    date_to = date_to
  )
  if (nzchar(site_name)) payload$site_name <- site_name

  url <- paste0(base_url, "/api/external/kpi/lte-717b/")
  t0 <- proc.time()[["elapsed"]]
  request_meta <- list(
    cell_name = cell_name,
    site_name = if (nzchar(site_name)) site_name else NULL,
    date_from = date_from,
    date_to = date_to,
    action = "kpi_lte_717b",
    kpi = "LTE_717b"
  )

  result <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_method("POST") |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body <- safe_parse_json(httr2::resp_body_string(resp))
      ok <- status >= 200 && status < 300
      if (!is.null(body$success)) {
        ok <- isTRUE(body$success) && ok
      }

      msg <- if (ok) {
        latest_val <- tryCatch(
          body$kpis$LTE_717b$latest$value,
          error = function(e) NULL
        )
        if (!is.null(latest_val) && !is.na(suppressWarnings(as.numeric(latest_val)))) {
          sprintf(
            "LTE_717b OK — %s: %s usuarios activos",
            cell_name,
            format(round(as.numeric(latest_val), 1), nsmall = 1)
          )
        } else {
          body$message %||% sprintf("LTE_717b OK — %s", cell_name)
        }
      } else {
        friendly_http_error(status, body)
      }

      list(
        success = ok,
        status_code = status,
        body = body,
        message = as.character(msg),
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    },
    error = function(e) {
      err_msg <- conditionMessage(e)
      timed_out <- grepl("timeout|timed out|Timeout", err_msg, ignore.case = TRUE)
      list(
        success = FALSE,
        status_code = if (timed_out) 504L else NA_integer_,
        body = NULL,
        message = if (timed_out) {
          paste("Timeout consultando KPI LTE_717b:", err_msg)
        } else {
          paste("Error de red:", err_msg)
        },
        elapsed_sec = proc.time()[["elapsed"]] - t0,
        request_meta = request_meta
      )
    }
  )

  result
}

#' Extrae serie temporal LTE_717b desde body de respuesta.
extract_717b_timeseries <- function(body) {
  block <- body$kpis$LTE_717b
  if (is.null(block)) {
    return(data.frame(
      period_start_time = as.POSIXct(character()),
      value = numeric(),
      kpi = character(),
      lncel_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  kpi_points_to_df(block, "LTE_717b")
}

#' Formatea source_tables (character o lista) para UI.
format_source_tables <- function(x) {
  if (is.null(x)) return("RAW 15 min")
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (!length(x)) "RAW 15 min" else paste(x, collapse = ", ")
}

#' Redondea POSIXct al múltiplo inferior de `mins` minutos.
floor_to_minutes <- function(x, mins = 15L) {
  x <- as.POSIXct(x)
  if (!length(x) || all(is.na(x))) return(x)
  mins <- as.integer(mins)
  if (is.na(mins) || mins < 1L) mins <- 15L
  origin <- as.POSIXct("1970-01-01", tz = attr(x, "tzone") %||% "")
  secs <- as.numeric(difftime(x, origin, units = "secs"))
  step <- mins * 60
  as.POSIXct(floor(secs / step) * step, origin = origin, tz = attr(x, "tzone") %||% "")
}

#' Grilla regular 15 min + operatividad inferida por presencia de KPI.
#'
#' Si no hay muestra en un bucket, se asume sector no operativo (apagado / sin reporting).
#' También reindexa series KPI con NA en huecos para que el gráfico corte la línea.
#'
#' @param df_prb data.frame de extract_prb_timeseries (o vacío)
#' @param df_ue data.frame de extract_717b_timeseries (o vacío)
#' @param interval_min intervalo en minutos (default 15)
#' @return list(grid, df_prb, df_ue) con period_start_time alineados
build_kpi_operability <- function(df_prb = NULL, df_ue = NULL, interval_min = 15L) {
  empty_grid <- data.frame(
    period_start_time = as.POSIXct(character()),
    operativo = integer(),
    oper_band = numeric(),
    stringsAsFactors = FALSE
  )
  empty_prb <- data.frame(
    period_start_time = as.POSIXct(character()),
    value = numeric(),
    kpi = character(),
    serie = character(),
    stringsAsFactors = FALSE
  )
  empty_ue <- data.frame(
    period_start_time = as.POSIXct(character()),
    value = numeric(),
    stringsAsFactors = FALSE
  )

  ts_all <- c(
    if (!is.null(df_prb) && nrow(df_prb)) df_prb$period_start_time else as.POSIXct(character()),
    if (!is.null(df_ue) && nrow(df_ue)) df_ue$period_start_time else as.POSIXct(character())
  )
  ts_all <- ts_all[!is.na(ts_all)]
  if (!length(ts_all)) {
    return(list(grid = empty_grid, df_prb = empty_prb, df_ue = empty_ue))
  }

  t_min <- min(floor_to_minutes(ts_all, interval_min), na.rm = TRUE)
  t_max <- max(floor_to_minutes(ts_all, interval_min), na.rm = TRUE)
  grid_ts <- seq(from = t_min, to = t_max, by = paste(interval_min, "min"))
  # Comparar por epoch para evitar fallos de igualdad POSIXct/TZ
  present_num <- unique(as.numeric(floor_to_minutes(ts_all, interval_min)))
  operativo <- as.integer(as.numeric(grid_ts) %in% present_num)
  # Banda visual: 100 en apagado (sombreado), 0 en operativo
  oper_band <- ifelse(operativo == 0L, 100, 0)

  grid <- data.frame(
    period_start_time = grid_ts,
    operativo = operativo,
    oper_band = oper_band,
    stringsAsFactors = FALSE
  )

  # Reindex PRB por kpi en la grilla (NA = hueco → línea rota)
  if (!is.null(df_prb) && nrow(df_prb)) {
    df_prb <- df_prb[!is.na(df_prb$period_start_time) & !is.na(df_prb$value), , drop = FALSE]
    df_prb$period_start_time <- floor_to_minutes(df_prb$period_start_time, interval_min)
    parts <- lapply(unique(df_prb$kpi), function(k) {
      sub <- df_prb[df_prb$kpi == k, , drop = FALSE]
      # si hay duplicados en el mismo bucket, último valor
      sub <- sub[order(sub$period_start_time), , drop = FALSE]
      sub <- sub[!duplicated(sub$period_start_time, fromLast = TRUE), , drop = FALSE]
      out <- data.frame(
        period_start_time = grid_ts,
        value = NA_real_,
        kpi = k,
        stringsAsFactors = FALSE
      )
      m <- match(out$period_start_time, sub$period_start_time)
      out$value <- ifelse(is.na(m), NA_real_, sub$value[m])
      out$serie <- if (identical(k, "LTE_PRB_DL")) "PRB DL (%)" else if (identical(k, "LTE_PRB_UL")) {
        "PRB UL (%)"
      } else {
        k
      }
      out
    })
    df_prb_out <- do.call(rbind, parts)
  } else {
    df_prb_out <- empty_prb
  }

  if (!is.null(df_ue) && nrow(df_ue)) {
    df_ue <- df_ue[!is.na(df_ue$period_start_time) & !is.na(df_ue$value), , drop = FALSE]
    df_ue$period_start_time <- floor_to_minutes(df_ue$period_start_time, interval_min)
    df_ue <- df_ue[order(df_ue$period_start_time), , drop = FALSE]
    df_ue <- df_ue[!duplicated(df_ue$period_start_time, fromLast = TRUE), , drop = FALSE]
    df_ue_out <- data.frame(
      period_start_time = grid_ts,
      value = NA_real_,
      stringsAsFactors = FALSE
    )
    m <- match(df_ue_out$period_start_time, df_ue$period_start_time)
    df_ue_out$value <- ifelse(is.na(m), NA_real_, df_ue$value[m])
  } else {
    df_ue_out <- empty_ue
  }

  list(grid = grid, df_prb = df_prb_out, df_ue = df_ue_out)
}

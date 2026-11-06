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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x

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
    "400" = "Solicitud inválida (validación de cell_name, desired_on o action).",
    "401" = "No autorizado: Bearer inválido o credenciales Chaos incorrectas.",
    "403" = "Prohibido: sin credenciales NetAct guardadas, usuario inactivo o sin permisos.",
    "404" = "Endpoint no encontrado. Verifique la Base URL.",
    "502" = "Error de NetAct o gateway (502).",
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

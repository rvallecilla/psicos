# Chaos · Control de potencia de sector (Shiny)

Aplicación web en **R / Shiny** para encender o apagar un sector de red móvil mediante la API HTTP externa Chaos / NetAct. Pensada para operadores NOC: conexión por sesión, confirmación obligatoria (sobre todo al apagar) y resultado claro.

## Requisitos

- R ≥ 4.1 (el lockfile actual registra R 4.6.2)
- Dependencias gestionadas con **renv** (`renv.lock`)

### Restaurar el entorno (otro PC o servidor)

```r
# En la raíz del proyecto clonado:
install.packages("renv")   # solo si aún no está en la librería de usuario
renv::restore()            # instala exactamente lo de renv.lock en renv/library/
shiny::runApp()
```

`renv::restore()` no versiona `renv/library/`; la reconstruye desde `renv.lock`. En CI/despliegue: clonar → `renv::restore()` → `shiny::runApp()` (o el proceso del host).

Instalación manual (sin renv), solo si hace falta:

```r
install.packages(c("shiny", "bslib", "httr2", "jsonlite"))
```

## Configuración de credenciales

1. Copie el ejemplo de entorno:

```bash
# Windows (PowerShell)
Copy-Item .Renviron.example .Renviron

# Unix / Git Bash
cp .Renviron.example .Renviron
```

2. Edite `.Renviron` o use **Guardar credenciales** en la UI:

```
CHAOS_BASE_URL=http://172.22.129.126:8001
CHAOS_PORT=8001
CHAOS_BEARER_TOKEN=su_bearer_token
CHAOS_USERNAME=usuario_chaos_admin
CHAOS_PASSWORD=su_password
```

3. `.Renviron` es local y está en `.gitignore` (no subir secretos al repo).

Autenticación doble:

| Capa | Dónde | Qué |
|------|--------|-----|
| Bearer | Header `Authorization` | Token del aplicativo externo |
| Chaos Admin | Body JSON | `username` / `password` (rol Administrador, con NetAct guardado en Chaos) |

No hardcodee secretos en el código. La UI **nunca** muestra Bearer ni contraseña en el historial ni en el JSON de diagnóstico redactado.

## Cómo ejecutar

Desde el directorio del proyecto:

```r
setwd("ruta/a/psicos")   # si hace falta
shiny::runApp()
```

O desde la consola del sistema:

```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

La app carga defaults de `CHAOS_*` si existen en el entorno / `.Renviron`.

## Uso en la UI

1. **Conexión** — Host/IP + **Puerto** (default `8001`), Bearer, usuario y contraseña Chaos → **Conectar**. **Guardar credenciales** persiste Bearer + usuario/contraseña Chaos + host/puerto en `.Renviron` local (no se versiona).
2. **Operación** — `cell_name` (ej. `CAL.Ingenio_S1`), tecnología Auto/4G/5G → **Encender** o **Apagar**.
3. Confirme en el modal (obligatorio). Tras éxito, se consulta el **estado** automáticamente.
4. **Estado del sector** — **Estado**, opcional **Live NetAct**, auto-refresh 15/30/60 s (OFF). Semáforo ENCENDIDO / APAGADO / DESCONOCIDO. Si el auto-refresh de estado está activo, también refresca PRB como máximo cada 60 s.
5. **Carga PRB** — Ambos/DL/UL, `date_from`/`date_to` (default 24 h, máx. 3 días), **Consultar PRB**, auto-refresh PRB ≥ 60 s. Tarjetas con valor actual grande + stats + gráfico plotly 0–100 %.
6. **Usuarios activos (LTE_717b)** — Avg active users per cell (RAW 15 min). Valor actual grande + min/avg/max + serie temporal. Auto-refresh ≥ 60 s.
7. **Resultado** — éxito/error de la operación y JSON colapsable.
8. **Historial** — últimas operaciones/consultas manuales de la sesión (sin secretos).

### Formato de `cell_name`

Obligatorio, con `_` separando sitio y sector:

- `CAL.Ingenio_S1`
- `CAQ.La Argentina_L2`

No enviar `region_code` ni `dn` (la API los resuelve).

## Funciones R reutilizables

Definidas en `R/api.R`:

```r
source("R/api.R")

# Health
ping_api(base_url, token, timeout = 15)

# Encender / apagar (timeout recomendado 120 s)
sector_power(
  base_url   = "http://172.22.129.126:8001",
  token      = Sys.getenv("CHAOS_BEARER_TOKEN"),
  username   = Sys.getenv("CHAOS_USERNAME"),
  password   = "••••",           # no versionar
  cell_name  = "CAL.Ingenio_S1",
  desired_on = TRUE,             # TRUE=encender, FALSE=apagar
  tech       = NULL,             # o "4G" / "5G"
  timeout    = 120
)

# Estado (POST JSON; timeout 30 s OSS / 120 s live)
sector_status(
  base_url  = "http://172.22.129.126:8001",
  token     = Sys.getenv("CHAOS_BEARER_TOKEN"),
  username  = Sys.getenv("CHAOS_USERNAME"),
  password  = "••••",
  cell_name = "CAL.Ingenio_S1",
  tech      = NULL,             # o "4g" / "5g"
  live      = FALSE,
  timeout   = 30
)

# Carga PRB LTE (RAW; timeout 60 s; default últimas 24 h)
kpi_lte_prb(
  base_url  = "http://172.22.129.126:8001",
  token     = Sys.getenv("CHAOS_BEARER_TOKEN"),
  username  = Sys.getenv("CHAOS_USERNAME"),
  password  = "••••",
  cell_name = "CAL.Ingenio_S1",
  kpis      = c("LTE_PRB_DL", "LTE_PRB_UL"),
  date_from = NULL,
  date_to   = NULL,
  timeout   = 60
)

# Usuarios activos LTE_717b (RAW 15 min; timeout 60 s)
kpi_lte_717b(
  base_url  = "http://172.22.129.126:8001",
  token     = Sys.getenv("CHAOS_BEARER_TOKEN"),
  username  = Sys.getenv("CHAOS_USERNAME"),
  password  = "••••",
  cell_name = "CAL.Ingenio_S1",
  date_from = NULL,
  date_to   = NULL,
  timeout   = 60
)
# Último valor: resp$body$kpis$LTE_717b$latest$value
```

### Ejemplo de prueba rápida (consola)

```r
source("R/api.R")

base  <- Sys.getenv("CHAOS_BASE_URL", "http://172.22.129.126:8001")
token <- Sys.getenv("CHAOS_BEARER_TOKEN")
user  <- Sys.getenv("CHAOS_USERNAME")
pass  <- Sys.getenv("CHAOS_PASSWORD")

# 1) Ping
ping_api(base, token)

# 2) Estado OSS
sector_status(base, token, user, pass, "CAL.Ingenio_S1", live = FALSE)

# 3) KPI PRB DL/UL (últimas 24 h)
kpi_lte_prb(base, token, user, pass, cell_name = "CAL.Ingenio_S1")

# 4) Usuarios activos LTE_717b
kpi_lte_717b(base, token, user, pass, cell_name = "CAL.Ingenio_S1")

# 5) Encender / apagar (opcional)
sector_power(base, token, user, pass, "CAL.Ingenio_S1", desired_on = TRUE)
```

## API (referencia)

**Base URL** (configurable): `http://172.22.129.126:8001`

| Método | Ruta | Uso |
|--------|------|-----|
| `GET` | `/api/external/netact/sector-power/ping/` | Health + validación Bearer |
| `POST` | `/api/external/netact/sector-power/` | Encender/apagar sector |
| `POST` | `/api/external/netact/sector-power/status/` | Consultar estado (OSS o live) |
| `POST` | `/api/external/kpi/lte-prb/` | Carga PRB DL/UL (tablas RAW) |
| `POST` | `/api/external/kpi/lte-717b/` | Usuarios activos Avg (LTE_717b, RAW 15 min) |

Body POST (ejemplo):

```json
{
  "username": "<usuario Chaos Administrador>",
  "password": "<password Chaos>",
  "cell_name": "CAL.Ingenio_S1",
  "desired_on": true
}
```

- `desired_on=true` → encender (unlock)
- `desired_on=false` → apagar (lock)
- Alternativa aceptada por la API: `"action": "on"|"off"|"encender"|"apagar"` (esta app usa `desired_on`)
- `tech` solo se envía si no es Auto

## Errores típicos (UI amigable)

| HTTP | Significado habitual |
|------|----------------------|
| 401 | `invalid_bearer` / `invalid_credentials` |
| 403 | `forbidden` / `missing_netact_credentials` / `user_inactive` |
| 400 | Validación (`cell_name`, `desired_on`/`action`) |
| 502 / 504 | Error NetAct / timeout |

## Estructura

```
psicos/
├── app.R                 # UI + server Shiny (bslib)
├── R/api.R               # ping_api(), sector_power(), helpers
├── renv.lock             # versiones fijadas (renv)
├── renv/activate.R       # activa la librería del proyecto
├── .Rprofile             # autoload de renv al abrir R aquí
├── .Renviron.example     # plantilla de entorno
├── .gitignore
└── README.md
```

No se versiona `renv/library/` (paquetes instalados localmente).

## Cómo probar (checklist)

1. Instale paquetes y configure `.Renviron` con Base URL, Bearer y usuario.
2. `shiny::runApp()` → abra el navegador.
3. Ingrese contraseña Chaos → **Conectar** → indicador verde y mensaje `bearer_ok`.
4. **Estado** → semáforo ENCENDIDO/APAGADO; pruebe Live NetAct y auto-refresh.
5. **Consultar PRB** → tarjetas DL/UL con valor actual (%), stats min/avg/max y gráfico 0–100 %; active auto-refresh PRB (≥ 60 s).
6. **Consultar usuarios activos** → valor grande “N usuarios”, stats y serie 15 min (LTE_717b).
7. **Encender** → confirme → Resultado + estado; KPI no se fuerza tras power (opcional manual).
8. **Apagar** (modal de advertencia) y verifique semáforo + historial.
9. Cadena mínima: **ping → status → kpi lte-prb → kpi lte-717b**.

## Seguridad operativa

- Confirmación modal antes de cualquier cambio de potencia.
- Aviso reforzado al apagar.
- Sin logs visibles de password ni Bearer.
- Credenciales pueden persistirse solo en `.Renviron` local (gitignore); nunca en el código ni en el historial/JSON de la UI.

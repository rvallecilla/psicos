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

2. Edite `.Renviron` (sin password):

```
CHAOS_BASE_URL=http://172.22.129.126:8001
CHAOS_BEARER_TOKEN=su_bearer_token
CHAOS_USERNAME=usuario_chaos_admin
```

3. La **contraseña Chaos** se introduce solo en la UI (no se recomienda guardarla en archivo).

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

1. **Conexión** — Base URL, Bearer, usuario y contraseña Chaos → **Probar conexión** (ping). Indicador verde/rojo.
2. **Operación** — `cell_name` (ej. `CAL.Ingenio_S1`), tecnología Auto/4G/5G → **Encender** o **Apagar**.
3. Confirme en el modal (obligatorio).
4. **Resultado** — éxito/error, campos clave (`region`, `dn`, `plan_name`, etc.) y JSON colapsable.
5. **Historial** — últimas operaciones de la sesión (sin secretos).

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
```

### Ejemplo de prueba rápida (consola)

```r
source("R/api.R")

base  <- Sys.getenv("CHAOS_BASE_URL", "http://172.22.129.126:8001")
token <- Sys.getenv("CHAOS_BEARER_TOKEN")

# 1) Ping
ping_api(base, token)

# 2) Encender sector de prueba
sector_power(base, token, "admin_user", "••••", "CAL.Ingenio_S1", desired_on = TRUE)

# 3) Apagar
sector_power(base, token, "admin_user", "••••", "CAL.Ingenio_S1", desired_on = FALSE)
```

## API (referencia)

**Base URL** (configurable): `http://172.22.129.126:8001`

| Método | Ruta | Uso |
|--------|------|-----|
| `GET` | `/api/external/netact/sector-power/ping/` | Health + validación Bearer |
| `POST` | `/api/external/netact/sector-power/` | Encender/apagar sector |

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
3. Ingrese contraseña Chaos → **Probar conexión** → indicador verde y mensaje `bearer_ok`.
4. Ingrese un `cell_name` de prueba válido → **Encender** → confirme → revise panel Resultado e historial.
5. Repita con **Apagar** (modal de advertencia) y verifique éxito/error legible.
6. Pruebe a propósito un Bearer incorrecto (401) y un usuario sin NetAct (403) para validar mensajes.

## Seguridad operativa

- Confirmación modal antes de cualquier cambio de potencia.
- Aviso reforzado al apagar.
- Sin logs visibles de password ni Bearer.
- Credenciales Chaos solo en memoria de la sesión Shiny.

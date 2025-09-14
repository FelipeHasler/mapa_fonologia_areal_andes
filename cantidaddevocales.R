# ============================
# Mapa de rasgo fonológico: cantidad de vocales en lenguas andinas
# Genera plantillas .md (si faltan) y publica index.html self-contained
# ============================

# ---- 0) Paquetes ----
library(readr)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(stringi)
library(fs)
library(commonmark)   # Markdown -> HTML
library(htmltools)
library(purrr)

# ---- 1) Parámetros ----
input_csv              <- "data/lenguas_vocales_andino_extendido.csv"  # <- relativo
md_dir                 <- "md"
popups_dir             <- "popups"
output_html            <- "index.html"                                  # <- Pages
overwrite_md_templates <- FALSE

# ---- 2) Utilidades ----
slugify <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    gsub("[^a-z0-9]+", "-", x = _) |>
    gsub("(^-|-$)", "", x = _)
}
normaliza_categoria <- function(x) {
  x <- tolower(trimws(x))
  dplyr::case_when(
    x %in% c("pequeño","pequenho","pequeno") ~ "pequeño",
    x %in% c("promedio","medio","media")     ~ "promedio",
    x %in% c("grande","amplio")              ~ "grande",
    TRUE                                     ~ x
  ) |> factor(levels = c("pequeño","promedio","grande"))
}
colores_categoria <- c("pequeño"="#1b9e77","promedio"="#d95f02","grande"="#7570b3")

# ---- 3) Cargar datos ----
if (!file_exists(input_csv)) stop("No se encuentra el CSV: ", input_csv)
df <- readr::read_csv(input_csv, show_col_types = FALSE, locale = locale(encoding="UTF-8")) |>
  rename_with(tolower) |>
  mutate(
    cantidad_vocales = normaliza_categoria(.data$cantidad_vocales),
    language_slug    = slugify(.data$language),
    md_file          = dplyr::coalesce(.data$md_file, paste0(.data$language_slug, ".md"))
  )
req <- c("language","lat","lon","cantidad_vocales","md_file")
faltan <- setdiff(req, names(df)); if (length(faltan)) stop("Faltan columnas: ", paste(faltan, collapse=", "))

# ---- 3.5) Plantillas .md si faltan ----
dir_create(md_dir)
make_md_template_text <- function(language, cat) {
  paste0(
    "---\n",
    'title: "', language, "\"\n",
    "generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\nstatus: draft\n---\n\n",
    "# ", language, "\n\n",
    "**Cantidad de vocales (clase):** ", as.character(cat), "\n\n",
    "## Metadatos\n- **Familia:** \n- **Rama:** \n- **Área/región:** \n- **Fuente(s):** \n\n",
    "## Inventario vocálico\n> Reemplaza con el inventario real (IPA).\n\n",
    "| Categoría | Vocales |\n|---|---|\n| Orales | /i, e, a, o, u/ |\n| Nasales | — |\n| Largas | — |\n| Cortas | — |\n| Centralización | — |\n\n",
    "## Notas\n- Observaciones.\n\n## Referencias\n- Autor, A. (Año). *Título*.\n"
  )
}
pwalk(
  list(df$language, df$cantidad_vocales, file.path(md_dir, df$md_file)),
  function(lang, cat, md_path) {
    if (!file.exists(md_path) || isTRUE(overwrite_md_templates)) {
      write_file(make_md_template_text(lang, cat), md_path)
    }
  }
)

# ---- 4) Renderizar .md a HTML completos (opcionales) ----
dir_create(popups_dir)
render_md_to_full_html <- function(md_path, title=NULL, out_html) {
  if (!file_exists(md_path)) {
    contenido <- paste0("# Información no disponible\n\nNo se encontró `", md_path, "`.\n")
    html_body <- commonmark::markdown_html(contenido, extensions = TRUE)
  } else {
    md_text  <- readr::read_file(md_path, locale = locale(encoding="UTF-8"))
    html_body <- commonmark::markdown_html(md_text, extensions = TRUE)
  }
  page <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<style>body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Arial,sans-serif;padding:12px;max-width:640px;margin:auto}",
    "table{border-collapse:collapse}table,th,td{border:1px solid #ddd;padding:6px}",
    "img{max-width:100%;height:auto}a{color:#0366d6;text-decoration:none}a:hover{text-decoration:underline}</style></head><body>",
    if (!is.null(title)) paste0("<h2 style='margin-top:0'>", htmltools::htmlEscape(title), "</h2>") else "",
    html_body, "</body></html>"
  )
  write_file(page, out_html)
  invisible(out_html)
}
df <- df |>
  mutate(md_path = file.path(md_dir, md_file),
         popup_html_rel = file.path(popups_dir, paste0(language_slug, ".html")))
pwalk(list(df$md_path, df$language, df$popup_html_rel),
      ~ render_md_to_full_html(..1, title=..2, out_html=..3))

# ---- 5) Popups incrustados ----
popup_fragment <- function(md_path, title=NULL) {
  if (!file.exists(md_path)) {
    body <- commonmark::markdown_html("### Información no disponible\n\nAgrega el .md correspondiente.", extensions = TRUE)
  } else {
    md_text <- readr::read_file(md_path, locale = locale(encoding="UTF-8"))
    body <- commonmark::markdown_html(md_text, extensions = TRUE)
  }
  as.character(htmltools::HTML(paste0(
    "<div style='max-width:420px;padding:8px;font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Arial,sans-serif;'>",
    if (!is.null(title)) paste0("<h3 style='margin-top:0'>", htmltools::htmlEscape(title), "</h3>") else "",
    body, "</div>"
  )))
}
popup_contents <- vapply(seq_len(nrow(df)),
                         function(i) popup_fragment(df$md_path[i], df$language[i]),
                         FUN.VALUE = character(1))

# ---- 6) Mapa ----
pal <- leaflet::colorFactor(
  palette = unname(colores_categoria[levels(df$cantidad_vocales)]),
  domain  = levels(df$cantidad_vocales),
  na.color = "#999999"
)
m <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addScaleBar(position = "bottomleft") |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 7, stroke = TRUE, weight = 1, opacity = 1,
    color = ~pal(cantidad_vocales), fillColor = ~pal(cantidad_vocales),
    fillOpacity = 0.9, popup = popup_contents,
    label = ~paste0(language, " (", as.character(cantidad_vocales), ")")
  ) |>
  addLegend(position = "bottomright", pal = pal, values = ~cantidad_vocales,
            title = "Cantidad de vocales", opacity = 1)

# ---- 7) Guardar (self-contained para Pages) ----
saveWidget(m, file = output_html, selfcontained = TRUE)
message("✔ Generado ", output_html, " (self-contained).")



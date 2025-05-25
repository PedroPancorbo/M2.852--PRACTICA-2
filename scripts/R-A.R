# Establecer directorio de trabajo
#setwd("C:/Users/Pedro Jesus/Desktop/MASTER UOC DE DATOS/ASIGNATURAS/TIPOLOGIA Y CICLO DE VIDA DE LOS DATOS/PRACT2")

# Cargar librerías necesarias
library(dplyr)
library(readr)
library(stringr)
library(summarytools)
library(stringi)    # Para eliminar acentos
library(lubridate)  # Para manejo de fechas

# Leer el dataset
df <- read_csv("ofertas_completas.csv")

# Inspección inicial
View(head(df))
str(df)
view(dfSummary(df))

# Crear copia del dataset sin la columna ID
df_limpio <- df %>% select(-ID)

# Limpieza del campo Salario
df_limpio$Salario_limpio <- df_limpio$Salario %>%
  str_trim() %>%
  str_remove_all("€|euros?|brutos|anuales|mensuales|/mes|/año|al año|aproximadamente|[^0-9.,\\-]") %>%
  str_replace_all("\\.", "") %>%        
  str_replace_all(",", ".")

# Separar salario mínimo y máximo
salarios_split <- str_split_fixed(df_limpio$Salario_limpio, "-", 2)

# Convertir a numéricos
df_limpio$Salario_min <- round(as.numeric(salarios_split[, 1]), 0)
df_limpio$Salario_max <- round(as.numeric(salarios_split[, 2]), 0)

# Calcular salario promedio redondeado
df_limpio <- df_limpio %>%
  mutate(SalarioPromedio = round(rowMeans(cbind(Salario_min, Salario_max), na.rm = TRUE), 0))

# Ajustes al salario promedio
df_limpio <- df_limpio %>%
  mutate(
    SalarioPromedio = ifelse(SalarioPromedio > 0 & SalarioPromedio <= 100, SalarioPromedio * 1000, SalarioPromedio),
    SalarioPromedio = ifelse(SalarioPromedio < 7000, SalarioPromedio * 12, SalarioPromedio),
    SalarioPromedio = round(SalarioPromedio / 1000, 0)
  )

# Crear columna 'España'
df_limpio <- df_limpio %>%
  mutate(
    España = case_when(
      stri_trans_general(Localización, "Latin-ASCII") %>% str_to_lower() == "sin especificar" ~ NA_character_,
      str_detect(stri_trans_general(Localización, "Latin-ASCII") %>% str_to_lower(), "espana") ~ "SI",
      TRUE ~ "NO"
    )
  )

# Detectar CCAA
comunidades <- c(
  "Andalucia", "Aragon", "Baleares", "Canarias", "Cantabria", "Castilla la Mancha",
  "Castilla Leon", "Cataluna", "Madrid", "Navarra", "Comunidad Valenciana",
  "Extremadura", "Galicia", "Pais Vasco", "Asturias", "Murcia", "La Rioja"
)

detectar_ccaa <- function(localizacion) {
  loc_norm <- stri_trans_general(localizacion, "Latin-ASCII") %>% str_to_lower()
  if (loc_norm == "sin especificar") {
    return(NA_character_)
  }
  for (ccaa in comunidades) {
    if (str_detect(loc_norm, str_to_lower(ccaa))) {
      return(ccaa)
    }
  }
  return(NA_character_)
}

df_limpio$CCAA <- sapply(df_limpio$Localización, detectar_ccaa)

# Añadir columna 'Mes' a partir de 'Fecha'
df_limpio <- df_limpio %>%
  mutate(
    Fecha_parsed = dmy(Fecha),
    Mes = month(Fecha_parsed, label = TRUE, abbr = FALSE) %>% tolower()
  )

# Clasificación del campo 'Formación' en nuevo campo 'Nivel'
df_limpio <- df_limpio %>%
  mutate(
    Nivel = case_when(
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), "doctorado") ~ "Doctorado",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), "universitaria|universidad|universitarios") ~ "Universidad",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), 
                 "grado superior|grado medio|formacion profesional|fp|bachillerato|bachiller") ~ "FP/Bachiller",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), 
                 "eso|primario|primaria|educacion basica") ~ "Básico",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), "sin estudio") ~ "Sin estudios",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), "desconocido") ~ "Desconocido",
      str_detect(stri_trans_general(Formación, "Latin-ASCII") %>% str_to_lower(), "otro") ~ "Otros",
      TRUE ~ NA_character_
    )
  )

# Vista general
View(df_limpio)

readr::write_csv(df_limpio, "C:/Users/Pedro Jesus/Desktop/MASTER UOC DE DATOS/ASIGNATURAS/TIPOLOGIA Y CICLO DE VIDA DE LOS DATOS/PRACT2/df_limpio_exportado.csv")


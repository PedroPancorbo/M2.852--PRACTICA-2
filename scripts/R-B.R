# Crear df_imp con las columnas seleccionadas
df_imp <- df_limpio %>%
  select(Título, SalarioPromedio, España, CCAA, Mes, Nivel)

# Calcular mediana general del salario (excluyendo NA)
mediana_general <- median(df_imp$SalarioPromedio, na.rm = TRUE)

# Imputar valores faltantes por mediana de su Nivel, con respaldo a mediana general
df_imp <- df_imp %>%
  group_by(Nivel) %>%
  mutate(
    mediana_nivel = median(SalarioPromedio, na.rm = TRUE),
    SalarioPromedio = ifelse(
      is.na(SalarioPromedio),
      ifelse(is.na(mediana_nivel), mediana_general, mediana_nivel),
      SalarioPromedio
    )
  ) %>%
  select(-mediana_nivel) %>%
  ungroup()

# Excluir valores de SalarioPromedio > 100
df_imp <- df_imp %>%
  filter(SalarioPromedio <= 100)

# Añadir columna 'Modalidad' basada en el campo 'Título'
df_imp <- df_imp %>%
  mutate(
    Modalidad = case_when(
      str_detect(stri_trans_general(Título, "Latin-ASCII") %>% str_to_lower(), "hibrid") ~ "Híbrido",
      str_detect(stri_trans_general(Título, "Latin-ASCII") %>% str_to_lower(), "remoto") ~ "Remoto",
      str_detect(stri_trans_general(Título, "Latin-ASCII") %>% str_to_lower(), "presencial") ~ "Presencial",
      TRUE ~ "Presencial"  # Valor por defecto si no se identifica modalidad
    )
  )

# Visualizar resultado final
View(df_imp)

readr::write_csv(df_imp, "C:/Users/Pedro Jesus/Desktop/MASTER UOC DE DATOS/ASIGNATURAS/TIPOLOGIA Y CICLO DE VIDA DE LOS DATOS/PRACT2/df_imp_limpio.csv"
)


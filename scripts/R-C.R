# Cargar librerías
library(ggplot2)
library(dplyr)
library(forcats)

# Preprocesamiento para gráficos sin outliers (por valores)
df_sin_outliers <- df_imp %>% filter(SalarioPromedio <= 100)

# Filtrado para niveles poco significativos
niveles_filtrados <- df_imp %>%
  count(Nivel) %>%
  filter(n > 10) %>%
  pull(Nivel)

### VENTANA 1: SalarioPromedio (con y sin outliers visuales)
windows(title = "Ventana 1: SalarioPromedio")

par(mfrow = c(2, 2))

# Histograma completo
hist(df_imp$SalarioPromedio, col = "steelblue", main = "Histograma - SalarioPromedio",
     xlab = "Salario (miles €)")

# Boxplot completo
boxplot(df_imp$SalarioPromedio, col = "salmon", main = "Boxplot - SalarioPromedio",
        ylab = "Salario (miles €)")

# Histograma sin outliers (rango fijo 15-40)
hist(df_imp$SalarioPromedio, col = "darkgreen", main = "Histograma sin outliers",
     xlab = "Salario (miles €)", xlim = c(15, 40))

# Boxplot sin outliers (rango fijo 15-40)
boxplot(df_imp$SalarioPromedio, col = "orange", main = "Boxplot sin outliers",
        ylab = "Salario (miles €)", ylim = c(24, 29), outline = FALSE)

### VENTANA 2: España y CCAA
windows(title = "Ventana 2: España y CCAA")

par(mfrow = c(1, 2))

# España (incluir NA explícitamente)
df_es <- df_imp %>%
  mutate(España = fct_explicit_na(España, na_level = "NA")) %>%
  count(España)

pie(df_es$n, labels = df_es$España, col = rainbow(nrow(df_es)),
    main = "Distribución por España (incluye NA)")

# CCAA (evitar solapamiento con barras horizontales)
df_ccaa <- df_imp %>%
  filter(!is.na(CCAA)) %>%
  count(CCAA) %>%
  arrange(n)

barplot(df_ccaa$n, names.arg = df_ccaa$CCAA, las = 2, horiz = TRUE,
        col = rainbow(nrow(df_ccaa)), main = "Distribución por CCAA",
        xlab = "Cantidad", cex.names = 0.7)

### VENTANA 4: Nivel educativo
windows(title = "Ventana 4: Nivel")

# Gráfico de barras horizontales para evitar solapamientos
df_nivel <- df_imp %>% count(Nivel)

ggplot(df_nivel, aes(x = fct_reorder(Nivel, n), y = n, fill = Nivel)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Distribución por Nivel", x = "Nivel", y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

### VENTANA 5: Modalidad
windows(title = "Ventana 5: Modalidad")

df_mod <- df_imp %>% count(Modalidad)

pie(df_mod$n, labels = df_mod$Modalidad, col = rainbow(nrow(df_mod)),
    main = "Distribución por Modalidad")

# === Apartado 4.2: Contraste de hipótesis ===
# Queremos comparar si hay diferencias significativas en los salarios promedio
# entre los empleos en España (SI) y fuera de España (NO).

library(dplyr)
library(ggplot2)
library(car)  # para el test de Levene (homocedasticidad)

# Filtrar dataset sin valores NA en SalarioPromedio y España
df_test <- df_imp %>%
  filter(!is.na(SalarioPromedio), !is.na(España))

# Comprobamos la normalidad de cada grupo
grupo_espana <- df_test %>% filter(España == "SI") %>% pull(SalarioPromedio)
grupo_no_espana <- df_test %>% filter(España == "NO") %>% pull(SalarioPromedio)

# Test de Shapiro-Wilk para normalidad
cat("\n✅ Test de normalidad (Shapiro-Wilk):\n")
shapiro_espana <- shapiro.test(grupo_espana)
shapiro_no_espana <- shapiro.test(grupo_no_espana)
print(shapiro_espana)
print(shapiro_no_espana)

# Gráficos QQ-plot para visualizar la normalidad
par(mfrow = c(1, 2))
qqnorm(grupo_espana, main = "QQ-plot: España = SI")
qqline(grupo_espana)
qqnorm(grupo_no_espana, main = "QQ-plot: España = NO")
qqline(grupo_no_espana)
par(mfrow = c(1, 1))

# Comprobamos homocedasticidad (varianzas iguales) con test de Levene
cat("\n✅ Test de homocedasticidad (Levene):\n")
levene_test <- leveneTest(SalarioPromedio ~ España, data = df_test)
print(levene_test)

# Selección de test en función de normalidad y homocedasticidad
# Si normalidad y varianzas iguales: t-test (paramétrico)
# Si no, usar Wilcoxon rank-sum test (no paramétrico)

# Para este ejemplo, usamos el test no paramétrico por seguridad
cat("\n✅ Contraste de hipótesis (Wilcoxon rank-sum test):\n")
wilcox_test <- wilcox.test(SalarioPromedio ~ España, data = df_test)
print(wilcox_test)

# Interpretación rápida en consola
if (wilcox_test$p.value < 0.05) {
  cat("\n💡 Hay diferencia significativa en los salarios promedio según España.\n")
} else {
  cat("\n💡 No se detecta diferencia significativa en los salarios promedio según España.\n")
}

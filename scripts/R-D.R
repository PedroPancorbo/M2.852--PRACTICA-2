# Cargar librerías
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
library(factoextra)

# === Preprocesamiento ===

# Seleccionar variables relevantes
df_model <- df_imp %>%
  select(SalarioPromedio, España, CCAA, Nivel, Modalidad) %>%
  na.omit()  # Eliminar NA para modelos simples

# Convertir factores
df_model <- df_model %>%
  mutate(across(c(España, CCAA, Nivel, Modalidad), as.factor))

# === MODELO SUPERVISADO ===
# Objetivo: Predecir SalarioPromedio

# Codificar variables categóricas
dummies <- dummyVars(SalarioPromedio ~ ., data = df_model)
X <- predict(dummies, newdata = df_model)
y <- df_model$SalarioPromedio

# Dividir en entrenamiento y prueba
set.seed(123)
train_idx <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Entrenar modelo: regresión lineal
modelo_lm <- lm(y_train ~ ., data = as.data.frame(X_train))

# Evaluar
predicciones <- predict(modelo_lm, newdata = as.data.frame(X_test))
mse <- mean((y_test - predicciones)^2)
cat("MSE del modelo supervisado (regresión lineal):", round(mse, 2), "\n")

# === MODELO NO SUPERVISADO ===
# Objetivo: detectar agrupamientos en los tipos de empleo

# Usamos las mismas variables (sin la variable objetivo)
X_cluster <- scale(X)  # Normalizar

# Determinar número óptimo de clusters (opcional)
# fviz_nbclust(X_cluster, kmeans, method = "wss")

# Clustering con K-Means (K=3 arbitrario, puede ajustarse)
set.seed(123)
kmeans_model <- kmeans(X_cluster, centers = 3, nstart = 25)

# Visualización de los clusters en ventana emergente
windows()
fviz_cluster(kmeans_model, data = X_cluster,
             geom = "point", ellipse.type = "norm",
             main = "Clustering de Empleos (K-Means)")

# Añadir los clusters al dataframe original
df_model$Cluster <- factor(kmeans_model$cluster)

# Visualizar distribución de clusters por Nivel en ventana emergente
windows()
print(
  ggplot(df_model, aes(x = Nivel, fill = Cluster)) +
    geom_bar(position = "dodge") +
    labs(title = "Distribución de Clusters por Nivel de Formación")
)

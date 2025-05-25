# Librería para métricas adicionales
library(Metrics)  # si no la tienes instalada, haz: install.packages("Metrics")

# Predecir en test set
predicciones <- predict(modelo_lm, newdata = as.data.frame(X_test))
y_true <- y_test

# Calcular métricas básicas
mse <- mean((y_true - predicciones)^2)
rmse <- sqrt(mse)
mae <- mean(abs(y_true - predicciones))
r2 <- 1 - sum((y_true - predicciones)^2) / sum((y_true - mean(y_true))^2)

cat("MSE:", round(mse, 3), "\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE (Error absoluto medio):", round(mae, 3), "\n")
cat("R2 (Coeficiente de determinación):", round(r2, 3), "\n")

# Visualización de residuos
windows()
plot(y_true, predicciones,
     main = "Valores Reales vs Predichos",
     xlab = "Salario Promedio Real",
     ylab = "Salario Promedio Predicho",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)

windows()
plot(predicciones, y_true - predicciones,
     main = "Residuos vs Valores Predichos",
     xlab = "Valores Predichos",
     ylab = "Residuos (Error)",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

library(e1071)
library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggplot2)
library(ggpubr)


# Import data
data <- read_excel("path/0-data/bd_clean.xlsx")
# Select variables
data <- data[,c("Vol_i", "VOL_TOT", "DMOVCARGA", "Conc_100", "PEF", "DE", "Grupo")]
# Filter out rows with NA values in any of the selected variables
data <- data %>% filter(!is.na(Vol_i) & 
                          !is.na(VOL_TOT) & 
                          !is.na(DMOVCARGA) & 
                          !is.na(Conc_100) & 
                          !is.na(PEF) & 
                          !is.na(DE) & 
                          !is.na(Grupo))

###################################################################################
# Division de los datos en 70/30
set.seed(1) # fit a seed 

indice <- sample(1:nrow(data), round(0.7*nrow(data)))

train <- data[indice,] # train data 
test <- data[-indice,] # test data

# modelo de SVM
modelo <- svm(PEF~Vol_i+VOL_TOT+DE, train)
print(modelo)

# Predecir el modelo
pred <- predict(modelo, test)
pred_ <- predict(modelo, test, decision.values = TRUE)
attr(pred_, "decision.values")[1:4,]

plot(pred, test$PEF)

library(ggplot2)

# Predicciones en el conjunto de prueba
predicciones <- predict(modelo, test)

# Crear un nuevo dataframe con las predicciones y los valores reales
resultados <- data.frame(PEF_real = test$PEF, PEF_predicho = predicciones)


theme_crojfe <- function(base_size = 10, base_family = "Tahoma") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      panel.border = element_rect(color = "#4D4D4D", fill = NA, linewidth = 0.4),
      axis.line = element_line(color = "#4D4D4D", linewidth = 0.4),
      axis.ticks = element_line(color = "#4D4D4D", linewidth = 0.4),
      
      # grid for CROJFE
      panel.grid.major = element_line(color = "#B3B3B3", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.title = element_text(hjust = 0.5, size = base_size),
      axis.title = element_text(size = base_size),
      axis.text  = element_text(size = base_size)
    )
}


# Gráfico de dispersión comparando valores reales y predichos
plot1_svr <- ggplot(resultados, aes(y = PEF_real, x = PEF_predicho)) +
  geom_point(shape = 21, fill = "#CFCFCF", color = "#4D4D4D", size = 2, stroke = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#4D4D4D", linewidth = 0.6, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.5) +
  labs(
    x = expression("Predicted productivity, m"^3*" h"^{-1}),
    y = expression("Observed productivity, m"^3*" h"^{-1})
  ) +
  scale_x_continuous(breaks = seq(0, 120, 20), limits = c(0, 120)) +
  scale_y_continuous(breaks = seq(0, 120, 20), limits = c(0, 120)) +
  #coord_equal() +
  theme_crojfe(base_size = 8, base_family = "Tahoma")

lm_eq <- lm(PEF_real ~ PEF_predicho, data = resultados)

eq <- bquote(italic(y) == .(round(coef(lm_eq)[1], 2)) + .(round(coef(lm_eq)[2], 2)) %.% italic(x))
r_squared <- bquote(R^2 == .(round(summary(lm_eq)$r.squared, 2)))
sample_size <- bquote(italic(n) == .(nrow(resultados)))

plot_with_eq <- plot1_svr +
  annotate("text", x = 50, y = 25, label = as.expression(eq), family = "Tahoma", size = 2.5, hjust = 0) +
  annotate("text", x = 50, y = 17, label = as.expression(r_squared), family = "Tahoma", size = 2.5, hjust = 0) +
  annotate("text", x = 50, y = 9,  label = as.expression(sample_size), family = "Tahoma", size = 2.5, hjust = 0)


# Mostrar el gráfico con la ecuación, R² y el valor de 'n'
print(plot_with_eq)
#############################################################################
# Calcular los residuos
# Crear un nuevo dataframe con las predicciones y los valores reales
resultados <- data.frame(PEF_real = test$PEF, PEF_predicho = predicciones)
residuos <- test$PEF - resultados$PEF_predicho
resultados$residuos <- residuos

plot2 <- ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_density(fill = "#CFCFCF", color = "black", linewidth = 0.5) +
  labs(
    x = "Residuals",
    y = "Density"
  ) +
  theme_crojfe(base_size = 8, base_family = "Tahoma")

################################################################################
residuos <- test$PEF - resultados$PEF_predicho

plot3 <- ggplot(resultados, aes(x = PEF_real, y = residuos)) +
  geom_point(shape = 21, fill = "#CFCFCF", color = "#4D4D4D", size = 2, stroke = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
    x = expression("Observed Productivity, m"^3*" h"^{-1}),
    y = "Residuals"
  ) +
  theme_crojfe(base_size = 8, base_family = "Tahoma")



library(ggpubr)
PLOT_SVM <- ggarrange(
  plot_with_eq, plot2, plot3,
  labels = c("A", "B", "C"),
  font.label = list(size = 8, face = "bold", family = "Tahoma"),
  ncol = 3,
  widths = c(1, 1, 1),   # <- fuerza mismo ancho
  align = "hv"
)
PLOT_SVM <- PLOT_SVM + theme(text = element_text(size = 14))
PLOT_SVM

ggsave("Fig6.png", PLOT_SVM, width = 18, height = 9, units = "cm", dpi = 600)

library(e1071)
library(readxl)
library(dplyr)
library(ggplot2)
library(purrr)
library(nlme)
library(ggpubr)



# Import data
data <- data <- read_excel("path/0-data/bd_clean.xlsx")

# Select variables
data <- data[,c("Vol_i", "VOL_TOT", "DMOVCARGA", "Conc_100", "PEF", "DE", "Grupo", "sitio")]

# Filter out rows with NA values in any of the selected variables
data <- data %>% filter(!is.na(Vol_i) & 
                          !is.na(VOL_TOT) & 
                          !is.na(DMOVCARGA) & 
                          !is.na(Conc_100) & 
                          !is.na(PEF) & 
                          !is.na(DE) & 
                          !is.na(Grupo)& 
                          !is.na(sitio))

set.seed(1) # fit a seed 

indice <- sample(1:nrow(data), round(0.7*nrow(data)))

train <- data[indice,] # train data 
test <- data[-indice,] # test data

# Mixed effect model
modelo_mixto <- lme(log(PEF)~log(DE)+log(Vol_i)+VOL_TOT, random = ~1|sitio, data)
summary(modelo_mixto)
data$predichos <- exp(predict(modelo_mixto, data))

error <- data$PEF - data$predichos
rmse <- sqrt(mean(error^2))
rmse

# Predictions
test$predichos <- exp(predict(modelo_mixto, test))

################################################################################
library(ggplot2)

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

# Plot A: Observed vs Predicted
plot1 <- ggplot(test, aes(x = predichos, y = PEF)) +
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

lm_eq <- lm(PEF ~ predichos, data = test)

eq <- bquote(italic(y) == .(round(coef(lm_eq)[1], 2)) + .(round(coef(lm_eq)[2], 2)) %.% italic(x))
r_squared <- bquote(R^2 == .(round(summary(lm_eq)$r.squared, 2)))
sample_size <- bquote(italic(n) == .(nrow(test)))

plot_with_eq <- plot1 +
  annotate("text", x = 50, y = 25, label = as.expression(eq), family = "Tahoma", size = 2.5, hjust = 0) +
  annotate("text", x = 50, y = 17, label = as.expression(r_squared), family = "Tahoma", size = 2.5, hjust = 0) +
  annotate("text", x = 50, y = 9,  label = as.expression(sample_size), family = "Tahoma", size = 2.5, hjust = 0)

residuos <- test$PEF - test$predichos

plot2 <- ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_density(fill = "#CFCFCF", color = "black", linewidth = 0.5) +
  labs(
    x = "Residuals",
    y = "Density"
  ) +
  theme_crojfe(base_size = 8, base_family = "Tahoma")
residuos_df <- data.frame(PEF_observed = test$PEF, Residuos = residuos)

plot3 <- ggplot(residuos_df, aes(x = PEF_observed, y = Residuos)) +
  geom_point(shape = 21, fill = "#CFCFCF", color = "#4D4D4D", size = 2, stroke = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
    x = expression("Observed Productivity, m"^3*" h"^{-1}),
    y = "Residuals"
  ) +
  theme_crojfe(base_size = 8, base_family = "Tahoma")



PLOT_MLM <- ggarrange(
  plot_with_eq, plot2, plot3,
  labels = c("A", "B", "C"),
  font.label = list(size = 8, face = "bold", family = "Tahoma"),
  ncol = 3,
  widths = c(1, 1, 1),   # <- fuerza mismo ancho
  align = "hv"
)

PLOT_MLM

ggsave("fig3.png", PLOT_MLM, width = 18, height = 9, units = "cm", dpi = 600)

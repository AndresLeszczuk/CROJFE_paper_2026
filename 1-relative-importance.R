# Libraries
library(relaimpo)
library(readxl)
library(dplyr)
library(ggplot2)
library(car)

# Import data
data <- read_excel("path/0-data/bd_clean.xlsx")

# Filter out rows with NA values in any of the selected variables
data <- data %>% filter(!is.na(Vol_i) & 
                          !is.na(VOL_TOT) & 
                          !is.na(DMOVCARGA) & 
                          !is.na(Conc_100) & 
                          !is.na(PEF) & 
                          !is.na(DE) & 
                          !is.na(Grupo))

# Create binary group for forwarder
data <- data %>%
  mutate(Grupo_binario = ifelse(Grupo == "19 t", 1, 0))

# Remove the 'Grupo' variable and keep only the binary group variable
data <- data[,c("Vol_i", "VOL_TOT", "DMOVCARGA", "Conc_100", "PEF", "DE", "Grupo_binario")]

############################################################################################
# Create a generic model for all variables
modelo_generico <- lm(PEF~., data)
# Summary of generic full model
summary(modelo_generico)
vif(modelo_generico)
# Anova to generic full model
anova(modelo_generico)

# Relaimpo with bootstrap model
bootPEF <- boot.relimp(modelo_generico, 
                       b = 1000,
                       rank = TRUE,
                       diff = TRUE, 
                       rela = TRUE)

booteval.relimp(bootPEF)
plot(booteval.relimp(bootPEF))

# plot for realimpo
results <- booteval.relimp(bootPEF)
str(results)

# Extract data for results
lmg_means <- results@lmg
lmg_lower <- as.vector(results@lmg.lower) 
lmg_upper <- as.vector(results@lmg.upper) 
names_ <- results@namen
names_ <- names_[2:7]

labels_plot <- c("Log volume, m³",
                 "Load volume, m³",
                 "Distance between loads, m", 
                 "Log concentration, m³ 100m",
                 "Extraction distance, m", 
                 "Forwarder size")

# Convert to a data frame
df <- data.frame(
  names_,
  labels_plot,
  lmg_means,
  lmg_lower, 
  lmg_upper
)

df

# Color for plots
my_colors <- c("#1A1A1A", "#4D4D4D", "#808080", "#A6A6A6", "#C7C7C7", "#E6E6E6")
my_colors2 <-  c("#1A1A1A", "#4D4D4D", "#808080", "#A6A6A6", "#C7C7C7", "#E6E6E6")

# plot
plot <- ggplot(df, aes(x = names_, y = lmg_means, fill = labels_plot)) +
  geom_col() + # bars
  geom_errorbar(aes(ymin = lmg_lower, ymax = lmg_upper), width = 0.2) +
  scale_fill_manual(values = my_colors, name = "") + # Assign colors
  labs(x = "Variable", 
       y = "% de R² explined") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  theme_classic() +
  theme(legend.position = "bottom", # legend position
        legend.text = element_text( size = 10), # text size legend
        legend.title = element_text(size = 10), 
        axis.title = element_text(  size = 10), 
        plot.title = element_text(  size = 10, hjust = 0.5)) + 
  theme(axis.text.x = element_blank()) + 
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) 

print(plot)

################################################################################
# Plot for CROFJE
# Grayscale palette (adjust the number of colors according to the levels of labels_plot)
my_colors_gray <- c("#1A1A1A", "#4D4D4D", "#7F7F7F", "#A6A6A6", "#CFCFCF", "#E6E6E6")

plot_crofje <- ggplot(df, aes(x = names_, y = lmg_means, fill = labels_plot)) +
  geom_col(color = "#4D4D4D", linewidth = 0.3, width = 0.3) +   # bar borders
  geom_errorbar(
    aes(ymin = lmg_lower, ymax = lmg_upper),
    width = 0.2,
    color = "#4D4D4D",
    linewidth = 0.4
  ) +
  scale_fill_manual(values = my_colors_gray, name = "") +
  labs(
    x = "Variable",
    y = expression("% of " * R^2 * " explained")
  ) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  
  # CROJFE style: visible grid and white background
  theme_classic(base_size = 10, base_family = "Tahoma") +
  theme(
    panel.border = element_rect(color = "#4D4D4D", fill = NA, linewidth = 0.4),
    
    # journal-like grid (strong but thin)
    panel.grid.major.y = element_line(color = "#B3B3B3", linewidth = 0.4),
    panel.grid.major.x = element_line(color = "#B3B3B3", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "#4D4D4D", linewidth = 0.4),
    axis.ticks = element_line(color = "#4D4D4D", linewidth = 0.4),
    
    legend.position = c(0.05, 0.95),  # inside the panel, top-left
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "#4D4D4D", linewidth = 0.3),
    legend.key = element_rect(fill = "white", color = NA),
    
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    
    plot.title = element_text(size = 10, hjust = 0.5),
    
    axis.text.x = element_blank() # as in your original version
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

plot_crofje

# Save plot
ggsave("fig2.png", plot_crofje, width = 6, height = 6, dpi = 400)
############################################################################################
# analysis for 19 t forwarder
data_19t <- filter(data, Grupo_binario == 0)
data_19t <- data_19t[,-7]

modelo_19t <- lm(PEF~., data_19t )
summary(modelo_19t)

anova(modelo_19t)

# Relaimpo with bootstrap model
bootPEF_19t <- boot.relimp(modelo_19t, 
                           b = 1000,
                           rank = TRUE,
                           diff = TRUE, 
                           rela = TRUE)

booteval.relimp(bootPEF_19t)
plot(booteval.relimp(bootPEF_19t))

# plot
results_19t <- booteval.relimp(bootPEF_19t)
str(results_19t)

# Extract data from the `results_19t` object
lmg_means <- results_19t@lmg
lmg_lower <- as.vector(results_19t@lmg.lower) 
lmg_upper <- as.vector(results_19t@lmg.upper) 
names_19t <- results_19t@namen
names_19t <- names_19t[2:6]

labels_19t <- c(  "Log volume, m³",
                  "Load volume, m³",
                  "Distance between loads, m", 
                  "Log concentration, m³ 100m",
                  "Extraction distance, m")

# data frame
df_19t <- data.frame(
  names_19t,
  labels_19t,
  lmg_means,
  lmg_lower, 
  lmg_upper
)

df_19t

# Use ggplot2 to create the plot
plot19t <- ggplot(df_19t, aes(x = names_19t, y = lmg_means, fill = labels_19t)) + # Assign 'labels' to fill for the legend
  geom_col() + # Remove fill so it uses the color according to the legend
  geom_errorbar(aes(ymin = lmg_lower, ymax = lmg_upper), width = 0.2) +
  scale_fill_manual(values = my_colors2, name = "Legend") + # Assign colors manually
  labs(x = "Variables", 
       y = "% de R² explicado") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) + # Set limits and breaks for the Y axis
  theme_classic() +
  theme(legend.position = "none", # Legend position
        legend.text = element_text(size = 16), # Legend text size
        legend.title = element_text(size = 16), # Legend title text size
        axis.title = element_text(size = 16), # Axis title text size
        plot.title = element_text(size = 16, hjust = 0.5)) + # Plot title size and position
  theme(axis.text.x = element_blank()) # Hide x-axis labels

plot19t

# Plot CROFJE 19 t
plot_crofje_19t <- ggplot(df_19t, aes(x = names_19t, y = lmg_means, fill = labels_19t)) +
  geom_col(color = "#4D4D4D", linewidth = 0.3, width = 0.3) +   # bar borders
  geom_errorbar(
    aes(ymin = lmg_lower, ymax = lmg_upper),
    width = 0.2,
    color = "#4D4D4D",
    linewidth = 0.4
  ) +
  scale_fill_manual(values = my_colors_gray, name = "") +
  labs(
    x = "Variable",
    y = expression("% of " * R^2 * " explained")
  ) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  
  # CROJFE style: visible grid and white background
  theme_classic(base_size = 10) +
  theme(
    panel.border = element_rect(color = "#4D4D4D", fill = NA, linewidth = 0.4),
    
    # journal-like grid (strong but thin)
    panel.grid.major.y = element_line(color = "#B3B3B3", linewidth = 0.4),
    panel.grid.major.x = element_line(color = "#B3B3B3", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "#4D4D4D", linewidth = 0.4),
    axis.ticks = element_line(color = "#4D4D4D", linewidth = 0.4),
    
    legend.position = c(0.05, 0.95),  # inside the panel, top-left
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "#4D4D4D", linewidth = 0.3),
    legend.key = element_rect(fill = "white", color = NA),
    
    legend.text = element_text(size = 6),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    
    plot.title = element_text(size = 10, hjust = 0.5),
    
    axis.text.x = element_blank() # as in your original version
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

plot_crofje_19t


############################################################################################
# analysis for 16 t 
data_16t <- filter(data, Grupo_binario == 1)
data_16t <- data_16t[,-7]

modelo_16t <- lm(PEF~., data_16t )
summary(modelo_16t)

anova(modelo_16t)

# Relaimpo with bootstrap model
bootPEF_16t <- boot.relimp(modelo_16t, 
                           b = 1000,
                           rank = TRUE,
                           diff = TRUE, 
                           rela = TRUE)

booteval.relimp(bootPEF_16t)
plot(booteval.relimp(bootPEF_16t))

# plot
results <- booteval.relimp(bootPEF_16t)
str(results)

# Extract data from the `results` object
lmg_means <- results@lmg
lmg_lower <- as.vector(results@lmg.lower) 
lmg_upper <- as.vector(results@lmg.upper) 
names_16t <- results@namen
names_16t <- names_16t[2:6]

labels_16t <- c(  "Log volume, m³",
                  "Load volume, m³",
                  "Distance between loads, m", 
                  "Log concentration, m³ 100m",
                  "Extraction distance, m")


# Convert to a data frame
df_16t <- data.frame(
  names_16t,
  labels_16t,
  lmg_means,
  lmg_lower, 
  lmg_upper
)


# Use ggplot2 to create the plot
plot_16t <- ggplot(df_16t, aes(x = names_16t, y = lmg_means, fill = labels_16t)) +
  geom_col() + # Remove fill so it uses the color according to the legend
  geom_errorbar(aes(ymin = lmg_lower, ymax = lmg_upper), width = 0.2) +
  scale_fill_manual(values = my_colors2, name = "Legend") + # Assign colors manually
  labs(x = "Variables", 
       y = "% de R² explicado") +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) + # Set limits and breaks for the Y axis
  theme_classic() +
  theme(legend.position = "none", # Legend position
        legend.text = element_text(size = 16), # Legend text size
        legend.title = element_text(size = 16), # Legend title text size
        axis.title = element_text(size = 16), # Axis title text size
        plot.title = element_text(size = 16, hjust = 0.5)) + # Plot title size and position
  theme(axis.text.x = element_blank()) # Hide x-axis labels

plot_16t


#library(ggpubr)
#
## Configura el tamaño de la fuente de las etiquetas de los subgráficos
#plot12 <- ggarrange(
#  plot,
#  ggarrange(plot1, plot2, nrow = 2, labels = c("B", "C")),
#  ncol = 2,
#  labels = "A",
#  font.label = list(size = 16, face = "bold") # Ajustar el tamaño de la fuente aquí
#)
#
#plot12










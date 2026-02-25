library(e1071)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(neuralnet)
library(xgboost)
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(DiagrammeR)
library(cvTools)
library(tidyr)
library(purrr)
library(ggpubr)
library(nlme)

# Import data
data <- read_excel("path/0-data/bd_clean.xlsx")
# Select variables
data <- data[,c("Vol_i", "VOL_TOT", "PEF", "DE")]

# Filter out rows with NA values in any of the selected variables
data <- data %>% filter(!is.na(Vol_i) & 
                          !is.na(VOL_TOT) & 
                          #!is.na(DMOVCARGA) & 
                          #!is.na(Conc_100) & 
                          !is.na(PEF) & 
                          !is.na(DE) 
                          #& !is.na(Grupo)
                        )
# Escalar los datos para redes neuronales
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
scaled
################################################################################
# Modelos 
set.seed(123)
# Suport vector machine
modelo_SVM <- svm(PEF~Vol_i+VOL_TOT+DE, data)

modelo_NN <- neuralnet(PEF~DE+Vol_i+VOL_TOT,
                       data = scaled,
                       threshold = 0.001,
                       act.fct = "logistic",
                       linear.output = T,
                       hidden = c(4,6,6),
                       stepmax = 1000000)




################################################################################
# Funcion para crear los folders
fold_cv <- function(data,k){
  folds=cvTools::cvFolds(nrow(data),K=k)
  invisible(folds)
}

fold<-data%>%fold_cv(.,k=10)

str(fold)

################################################################################
temp<-data%>%mutate(Fold=rep(0,nrow(data)),
                    holdoutpred=rep(0,nrow(data)),
                    MSE=rep(0,nrow(.)),
                    RMSE=rep(0,nrow(.)),
                    MAE=rep(0,nrow(.)),
                    R2=rep(0,nrow(.)),
                    AIC=rep(0,nrow(.)),
                    BIC=rep(0,nrow(.)),
                    COR=rep(0,nrow(.)))

temp

for(i in 1:10){
  # set two set of data in train and validation ----
  train <- temp[fold$subsets[fold$which != i],]
  validation <- temp[fold$subsets[fold$which == i], ]
  
  newpred <- predict(modelo_SVM, validation)
  newpred

  true <- validation$PEF
  true
  
  error <- (true-newpred)
  error
  
  rmse <- sqrt(mean(error^2))
  rmse
  
  mse=mean((newpred-true)^2)
  mse
  
  # R2 estimation 
  df <- data.frame(newpred, true)
  modlm <- lm(newpred~true, df)
  
  
  R2=summary(modlm)$r.squared
  R2
  
  mae=mean(abs(error))
  mae
  
  COR = cor(newpred,validation$PEF)
  COR[1]
  
  temp[fold$subsets[fold$which == i], ]$holdoutpred <- newpred[1]
  temp[fold$subsets[fold$which == i], ]$RMSE <- rmse
  temp[fold$subsets[fold$which == i], ]$MSE <- mse
  temp[fold$subsets[fold$which == i], ]$MAE <- mae
  temp[fold$subsets[fold$which == i], ]$R2 <- R2
  #temp[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  #temp[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp[fold$subsets[fold$which == i], ]$COR <-  COR[1]
  temp[fold$subsets[fold$which == i], ]$Fold <- i
  
  print(temp)
}

temp <- temp %>% pivot_longer( 
  cols = c("RMSE", "R2", "MAE"),
  names_to = "Statistic",
  values_to = "Value")

temp

theme_crojfe <- function(base_size = 8, base_family = "Tahoma") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      panel.border = element_rect(color = "#4D4D4D", fill = NA, linewidth = 0.4),
      axis.line = element_line(color = "#4D4D4D", linewidth = 0.4),
      axis.ticks = element_line(color = "#4D4D4D", linewidth = 0.4),
      
      plot.margin = margin(5.5, 40, 5.5, 10),
      
      # grid for CROJFE
      panel.grid.major = element_line(color = "#B3B3B3", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.title = element_text( hjust = 1, size = base_size),
      axis.title = element_text( hjust = 1,size = base_size),
      axis.text  = element_text( hjust = 1,size = base_size)
    )
}

colores <- c("#4D4D4D", "#A6A6A6", "#E6E6E6")

plot_SVM_MAE <- temp %>% filter(Statistic == "MAE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[1]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 10))+
  theme_crojfe()

plot_SVM_R2 <- temp %>% filter(Statistic == "R2") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[2]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(0.5, 1))+
  theme_crojfe()

plot_SVM_RMSE <- temp %>% filter(Statistic == "RMSE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[3]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 12))+
  theme_crojfe()

plot_SVM_MAE
plot_SVM_R2
plot_SVM_RMSE

plot_SVM <- ggarrange(plot_SVM_MAE, plot_SVM_R2, plot_SVM_RMSE,
                      nrow = 3)
plot_SVM
################################################################################


# NN ----
temp2<-data%>%mutate(Fold=rep(0,nrow(data)),
                    holdoutpred=rep(0,nrow(data)),
                    MSE=rep(0,nrow(.)),
                    RMSE=rep(0,nrow(.)),
                    MAE=rep(0,nrow(.)),
                    R2=rep(0,nrow(.)),
                    AIC=rep(0,nrow(.)),
                    BIC=rep(0,nrow(.)),
                    COR=rep(0,nrow(.)))

for(i in 1:10){
  # set two set of data in train and validation ----
  train <- temp2[fold$subsets[fold$which != i],]
  validation <- temp2[fold$subsets[fold$which == i], ]
  
  maxs <- apply(train[,c(1:4)], 2, max)
  mins <- apply(train[,c(1:4)], 2, min)
  train_scaled <- as.data.frame(scale(train[,c(1:4)], center = mins, scale = maxs - mins))
  
  
  maxs <- apply(validation[,c(1:4)], 2, max)
  mins <- apply(validation[,c(1:4)], 2, min)
  validation_scaled <- as.data.frame(scale(validation[,c(1:4)], center = mins, scale = maxs - mins))
  
  #newpred <- predict(newlm,validation_scaled)
  #newpred
  
  pr.nn <- compute(modelo_NN, validation_scaled) # This takes the test data and predicts the productivity of the forwarder.
  #pr.nn$net.result           # The values are scaled
  # are transformed to productivity values 
  newpred <- pr.nn$net.result*(max(data$PEF)-min(data$PEF))+min(data$PEF)
  newpred
  
  
  true <- validation$PEF
  true
  
  error <- (true-newpred)
  error
  
  rmse <- sqrt(mean(error^2))
  rmse
  
  mse=mean((newpred-true)^2)
  mse
  
  # R2 estimation 
  df <- data.frame(newpred, true)
  modlm <- lm(newpred~true, df)
  R2=summary(modlm)$r.squared
  R2
  
  mae=mean(abs(error))
  mae
  
  cor = cor(newpred,validation$PEF)
  cor[1]
  
  
  temp2[fold$subsets[fold$which == i], ]$holdoutpred <- newpred[1]
  temp2[fold$subsets[fold$which == i], ]$RMSE <- rmse
  temp2[fold$subsets[fold$which == i], ]$MSE <- mse
  temp2[fold$subsets[fold$which == i], ]$MAE <- mae
  temp2[fold$subsets[fold$which == i], ]$R2 <- R2
  #temp2[fold$subsets[fold$which == i], ]$AIC=AIC(newlm)
  #temp2[fold$subsets[fold$which == i], ]$BIC=BIC(newlm)
  temp2[fold$subsets[fold$which == i], ]$COR <-  cor[1]
  temp2[fold$subsets[fold$which == i], ]$Fold <- i
  
}

temp2 <- temp2 %>% pivot_longer( 
  cols = c("RMSE", "R2", "MAE"),
  names_to = "Statistic",
  values_to = "Value")


plot_NN_MAE <- temp2 %>% filter(Statistic == "MAE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[1]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 10))+
  theme_crojfe()

plot_NN_R2 <- temp2 %>% filter(Statistic == "R2") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[2]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(0.5, 1))+
  theme_crojfe()

plot_NN_RMSE <- temp2 %>% filter(Statistic == "RMSE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[3]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 12))+
  theme_crojfe()

plot_NN_MAE
plot_NN_R2
plot_NN_RMSE

plot_NN <- ggarrange(plot_NN_MAE, plot_NN_R2, plot_NN_RMSE,
                      nrow = 3)
plot_NN

################################################################################
# XGB ----
temp3<-data%>%mutate(Fold=rep(0,nrow(data)),
                     holdoutpred=rep(0,nrow(data)),
                     MSE=rep(0,nrow(.)),
                     RMSE=rep(0,nrow(.)),
                     MAE=rep(0,nrow(.)),
                     R2=rep(0,nrow(.)),
                     AIC=rep(0,nrow(.)),
                     BIC=rep(0,nrow(.)),
                     COR=rep(0,nrow(.)))

datos <- list()
for(i in 1:10){
  # set two set of data in train and validation ----
  datos$train <- temp3[fold$subsets[fold$which != i],]
  datos$test <- temp3[fold$subsets[fold$which == i], ]
  
  datos$train_mat <- 
    datos$train %>% 
    select(-PEF) %>% 
    as.matrix() %>% 
    xgb.DMatrix(data = ., label = datos$train$PEF)
  
  datos$test_mat <- 
    datos$test %>% 
    select(-PEF) %>% 
    as.matrix() %>% 
    xgb.DMatrix(data = ., label = datos$test$PEF)
  
  datos$train_mat
  datos$test_mat
  
  datos$modelo_xgb <- xgboost(data = datos$train_mat, 
                             objective = "reg:squarederror",
                             nrounds = 1000, 
                             max.depth = 2, 
                             eta = 0.3, 
                             nthread = 4)
  
  newpred <- predict(datos$modelo_xgb, datos$test_mat)
  
  true <- datos$test$PEF
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp3[fold$subsets[fold$which == i], ]$holdoutpred 
  temp3[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp3[fold$subsets[fold$which == i], ]$MSE=mse
  temp3[fold$subsets[fold$which == i], ]$MAE=mae
  temp3[fold$subsets[fold$which == i], ]$R2=R2
  #temp3[fold$subsets[fold$which == i], ]$cor = cor
  temp3[fold$subsets[fold$which == i], ]$Fold=i
  
  
  }
temp3 <- temp3 %>% pivot_longer( 
  cols = c("RMSE", "R2", "MAE"),
  names_to = "Statistic",
  values_to = "Value")


plot_XGB_MAE <- temp3 %>% filter(Statistic == "MAE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[1]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 10))+
  theme_crojfe()

plot_XGB_R2 <- temp3 %>% filter(Statistic == "R2") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[2]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(0.5, 1))+
  theme_crojfe()

plot_XGB_RMSE <- temp3 %>% filter(Statistic == "RMSE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[3]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 12))+
  theme_crojfe()

plot_XGB_MAE
plot_XGB_R2
plot_XGB_RMSE

plot_XGB <- ggarrange(plot_XGB_MAE, plot_XGB_R2, plot_XGB_RMSE,
                      nrow = 3)
plot_XGB
################################################################################
# Modelo mixto ----
# Import data
data <- read_excel("D:/Investigacion/0-Propia/TESIS_DOCTORAL/0 TESIS/1-Factores que afectan a la productividada de FW en raleos y tala Rasa/1-Armado de Base de datos/BBDDFW_Tala_rasa.xlsx", sheet = "Hoja1")

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

set.seed(123) # fit a seed 

indice <- sample(1:nrow(data), round(0.7*nrow(data)))

train <- data[indice,] # train data 
test <- data[-indice,] # test data

# Modelo mixto
modelo_mixto <- lme(log(PEF)~log(DE)+log(Vol_i)+VOL_TOT, random = ~1|sitio, data)

# Validacion de datos de redes neuronales
temp4<-data%>%mutate(Fold=rep(0,nrow(data)),
                     holdoutpred=rep(0,nrow(data)),
                     MSE=rep(0,nrow(.)),
                     RMSE=rep(0,nrow(.)),
                     MAE=rep(0,nrow(.)),
                     R2=rep(0,nrow(.)),
                     AIC=rep(0,nrow(.)),
                     BIC=rep(0,nrow(.)),
                     COR=rep(0,nrow(.)))

datos <- list()

for(i in 1:10){
  # set two set of data in train and validation ----
  datos$train <- temp4[fold$subsets[fold$which != i],]
  datos$test <- temp4[fold$subsets[fold$which == i], ]
  
  newpred <- exp(predict(modelo_mixto, datos$test))
  
  true <- datos$test$PEF
  error=(true-newpred)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred-true)^2)
  R2=1-(sum((true-newpred)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp4[fold$subsets[fold$which == i], ]$holdoutpred 
  temp4[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp4[fold$subsets[fold$which == i], ]$MSE=mse
  temp4[fold$subsets[fold$which == i], ]$MAE=mae
  temp4[fold$subsets[fold$which == i], ]$R2=R2
  #temp4[fold$subsets[fold$which == i], ]$cor = cor
  temp4[fold$subsets[fold$which == i], ]$Fold=i
  
  
}
temp4 <- temp4 %>% 
       pivot_longer(
         cols = c("RMSE", "R2", "MAE"),
         names_to = "Statistic",
         values_to = "Value"
       )

#colores <- c("#1F77B4", "#FF7F0E", "#C7C7C7")

plot_MLM_MAE <- temp4 %>% filter(Statistic == "MAE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[1]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 10))

plot_MLM_R2 <- temp4 %>% filter(Statistic == "R2") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[2]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(0.5, 1))

plot_MLM_RMSE <- temp4 %>% filter(Statistic == "RMSE") %>% 
  ggplot(aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_boxplot() +
  scale_fill_manual(values = colores[3]) +
  geom_point() +
  coord_flip() +
  facet_wrap(~Statistic, ncol = 1, scales = "free_y") +
  theme_minimal() +
  guides(fill = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5, colour = "black", linetype = 1),
    strip.text.x = element_blank()
  ) +
  scale_y_continuous(limits=c(5, 12))

plot_MLM_MAE
plot_MLM_R2
plot_MLM_RMSE

plot_MLM <- ggarrange(plot_MLM_MAE, plot_MLM_R2, plot_MLM_RMSE,
                      nrow = 3)
plot_MLM
################################################################################


library(ggpubr)
PLOT_Kfolds <- ggarrange(plot_NN, plot_SVM, plot_XGB,
                          labels = c("A","B","C"),
                          ncol = 3)
PLOT_Kfolds <- PLOT_Kfolds + theme(text = element_text(size = 10))
PLOT_Kfolds


ggsave("Fig7.png", PLOT_Kfolds, width = 18, height = 12, units = "cm", dpi = 600)

################################################################################
# redes
temp2 %>% group_by(Statistic) %>% 
  summarise(prom = mean(Value),
            sd = sd(Value),
            min = min(Value),
            max = max(Value))

# SVM
temp %>% group_by(Statistic) %>% 
  summarise(prom = mean(Value),
            sd = sd(Value),
            min = min(Value),
            max = max(Value))

#XGBoost
temp3 %>% group_by(Statistic) %>% 
  summarise(prom = mean(Value),
            sd = sd(Value),
            min = min(Value),
            max = max(Value))

#Modelo lineal mixto
temp4 %>% group_by(Statistic) %>% 
  summarise(prom = mean(Value),
            sd = sd(Value),
            min = min(Value),
            max = max(Value))


library(dplyr)
library(forecast)
library(quantmod)
library(QuantTools)
library(tidyverse)

library(glmnet)
library(car)

# treino1 <- base_completa2[1:69392,]
# teste1 <- base_completa2[69393:93080,]
# treino2 <- base_completa2[93081:165275,]
# teste2 <- base_completa2[165276:190564,]
# 
# base_completa_tab1 <- base_completa2[1:93080,c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
#                                  "RSI","MACD","WILL_R","ADO","CCI","ROC","DISP","OSCP","PSY",
#                                  "DIU","DID","BIAS","VOLR","ARATIO","BRATIO","ATR","BB_UP",
#                                  "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
#                                  "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
#                                  "MPP","PPO","SAR","OBV","VOLAT","MFI","VARR","MQO_BETA",
#                                  "OPEN","CLOSE","VOLUME","YYY")]
# 
# base_completa_tudo_treino <- base_completa2[93081:165275,]
# base_completa_tudo_teste <- base_completa2[165276:190564,]
# 
# base_completa_tab1_treino <- base_completa2[93081:165275,c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
#                                                            "RSI","MACD","WILL_R","ADO","CCI","ROC","DISP","OSCP","PSY",
#                                                            "DIU","DID","BIAS","VOLR","ARATIO","BRATIO","ATR","BB_UP",
#                                                            "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
#                                                            "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
#                                                            "MPP","PPO","SAR","OBV","VOLAT","MFI","VARR","MQO_BETA",
#                                                            "OPEN","CLOSE","VOLUME","YYY")]
# base_completa_tab1_teste <- base_completa2[165276:190564,c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
#                                                            "RSI","MACD","WILL_R","ADO","CCI","ROC","DISP","OSCP","PSY",
#                                                            "DIU","DID","BIAS","VOLR","ARATIO","BRATIO","ATR","BB_UP",
#                                                            "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
#                                                            "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
#                                                            "MPP","PPO","SAR","OBV","VOLAT","MFI","VARR","MQO_BETA",
#                                                            "OPEN","CLOSE","VOLUME","YYY")]
# 
# save(base_completa_tudo_treino, file = "base_completa_tudo_treino.RData")
# save(base_completa_tudo_teste, file = "base_completa_tudo_teste.RData")
# save(base_completa_tab1_treino, file = "base_completa_tab1_treino.RData")
# save(base_completa_tab1_teste, file = "base_completa_tab1_teste.RData")
# 
# 
# # Dumy code categorical predictor variables
# xx <- model.matrix(YYY~., treino1)[,-1]
# # Convert the outcome (class) to a numerical variable
# yy <- as.matrix(ifelse(treino1$YYY == 1, 1, 0))
# 
# mood <- glmnet(xx, yy, family = "binomial", alpha = 1, lambda = NULL)

paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')


load("bases_finais_sem_col_zoadas/nao_bugadas_ALE.csv")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_BRA.RData")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_CHN.RData")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_FRA.RData")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_JAP.RData")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_UK.RData")
load("C:/Users/b05652877465/Desktop/CAYAN TORNEIOS/CROSS-TESIS/coleta_global/basecompleta_USA.RData")

base_completa2 <- read.csv2("bases_finais_sem_col_zoadas/nao_bugadas_USA.csv",
                            header = T)

# for (i in 1:ncol(base_completa2)){
#   base_completa2[which(base_completa2[,i] %>% is.infinite()),i] <- 100000
# }


################ ICI!!!!!!!!!!!!!!!!!!!
base_completa2 <- base_completa2[,-which(colnames(base_completa2) == "ATIVO")]

#caso tabelas 1 e 2
prov <- base_completa2[1:floor(0.75*nrow(base_completa2)),]
xx <- model.matrix(YYY~., prov)[,-1]

# xx <- xx[,-52]
# Convert the outcome (class) to a numerical variable
yy <- as.matrix(ifelse(prov$YYY == 1, 1, 0))

xx <- apply(xx,2,function(x){(x-mean(x))/sd(x)})

gc()
set.seed(96500)

cv.lasso <- cv.glmnet(xx, yy, alpha = 1, family = "binomial", maxit = 801)
# Fit the final model on the training data

model <- glmnet(xx, yy, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

# Display regression coefficients
coef(model)

# Save selecionados tabela 1 and 2
ab <- coef(model)
nao_nulos <- which( ab[-1,1] != 0)

# Salvando a base 
write.csv( names(nao_nulos) , "Feature_Selection/lasso_tudo_USA.csv", row.names = F)

#caso só tabelas 1
prov <- base_completa2[1:floor(0.75*nrow(base_completa2)),c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
                                                            "RSI","MACD","WILL_R","CCI","ROC","DISP","OSCP","PSY",
                                                            "BIAS","VOLR","ATR","BB_UP",
                                                            "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
                                                            "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
                                                            "MPP","PPO","SAR","OBV","VOLAT","MFI","MQO_BETA",
                                                            "OPEN","CLOSE","VOLUME","YYY")]

xx <- model.matrix(YYY~SMA+WMA+EMA+MOM+STOCH_K+STOCH_D+
                     STOCH_D_SLOW+RSI+MACD+WILL_R+
                     CCI+ROC+DISP+OSCP+PSY+BIAS+
                     VOLR+ATR+BB_UP+BB_LOW+
                     DMI+KC_U+KC_L+TRIMA+MAE_UP+MAE_LOW+
                     REX+NVI+PVI+VAMA+HPR+LPR+MAXX+MINN+
                     VMOM+MPP+PPO+SAR+OBV+VOLAT+MFI+
                     MQO_BETA+OPEN+CLOSE+VOLUME, prov)[,-1]

yy <- as.matrix(ifelse(prov$YYY == 1, 1, 0))

xx <- apply(xx,2,function(x){(x-mean(x))/sd(x)})

gc()
set.seed(96500)

cv.lasso <- cv.glmnet(xx, yy, alpha = 1, family = "binomial", maxit = 801)
# Fit the final model on the training data
model <- glmnet(xx, yy, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

# Save selecionados tabela 1 and 2

ab <- coef(model)

nao_nulos <- which( ab[-1,1] != 0)

write.csv( names(nao_nulos) , "Feature_Selection/lasso_tab1_USA.csv", row.names = F)


# # Make predictions on the test data
# x.test <- model.matrix(diabetes ~., test.data)[,-1]
# probabilities <- model %>% predict(newx = x.test)
# predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# # Model accuracy
# observed.classes <- test.data$diabetes
# mean(predicted.classes == observed.classes)






# TORNEIOS TUDO

###### Definindo os grupos #######

prov <- base_completa2[1:round(0.75*nrow(base_completa2)),]
ncol(prov)
prov$YYY <- (prov$YYY+1)/2

# ROUND 1: 100/125

grupo <- list()

set.seed(96500)

grupo[[1]] <- sample( 1:117 , 25)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:117,unlist(grupo)),
                       size = 25)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 20){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 2: 75/100

prov <- prov[,c(sobraron,ncol(prov))]

grupo <- list()
grupo[[1]] <- sample( 1:100 , 20)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:100,unlist(grupo)),
                       size = 20)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 15){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 3: 50/75

prov <- prov[,c(sobraron,ncol(prov))]

grupo <- list()
grupo[[1]] <- sample( 1:75 , 15)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:75,unlist(grupo)),
                       size = 15)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 10){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# # ROUND 4: 20/50
# 
# prov <- prov[,c(sobraron,ncol(prov))]
# 
# grupo <- list()
# grupo[[1]] <- sample( 1:50 , 10)
# 
# for (i in 2:5){
#   grupo[[i]] <- sample(x = setdiff(1:50,unlist(grupo)),
#                        size = 10)
# }
# 
# candidatos <- grupo[[1]]
# modelos    <- list()
# resultado  <- list()
# campeoes  <- list()
# quallis  <- list()
# 
# for (i in 1:5){
#   candidatos <- grupo[[i]]
#   while(length(candidatos) > 4){
#     
#     modelos[[i]] <- glm(YYY ~ . , family = "binomial",
#                         data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
#     
#     resultado[[i]] <- car::Anova(modelos[[i]])
#     
#     pior <- which.max(resultado[[i]][,3])
#     
#     candidatos <- candidatos[-pior]
#     
#     campeoes[[i]] <- colnames(prov[,candidatos])
#     
#     quallis[[i]] <- candidatos
#   }
# }
# 
# unlist(unlist( lapply(resultado , rownames) ))
# sobraron <- unlist(quallis)

lista_final_tudo <- colnames(prov[,sobraron])

# TORNEIOS T1

###### Definindo os grupos #######

prov <- base_completa2[1:round(0.75*nrow(base_completa2)),c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
                                                            "RSI","MACD","WILL_R","CCI","ROC","DISP","OSCP","PSY",
                                                            "BIAS","VOLR","ATR","BB_UP",
                                                            "BB_LOW","DMI","KC_U","KC_L","TRIMA","MAE_UP","MAE_LOW",
                                                            "REX","NVI","PVI","VAMA","HPR","LPR","MAXX","MINN","VMOM",
                                                            "MPP","PPO","SAR","OBV","VOLAT","MFI","MQO_BETA",
                                                            "OPEN","CLOSE","VOLUME","YYY")]
ncol(prov)
prov$YYY <- (prov$YYY+1)/2

# ROUND 1: 40/51

grupo <- list()

set.seed(96500)

grupo[[1]] <- sample( 1:45 , 10)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:45,unlist(grupo)),
                       size = 10)
}

grupo[[5]] <- c(grupo[[5]],setdiff(1:45,unlist(grupo)))

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 8){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 2: 30/40

prov <- prov[,c(sobraron,ncol(prov))]

grupo <- list()
grupo[[1]] <- sample( 1:40 , 8)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:40,unlist(grupo)),
                       size = 8)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 6){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

# ROUND 3: 20/30

prov <- prov[,c(sobraron,ncol(prov))]

grupo <- list()
grupo[[1]] <- sample( 1:30 , 6)

for (i in 2:5){
  grupo[[i]] <- sample(x = setdiff(1:30,unlist(grupo)),
                       size = 6)
}

candidatos <- grupo[[1]]
modelos    <- list()
resultado  <- list()
campeoes  <- list()
quallis  <- list()

for (i in 1:5){
  candidatos <- grupo[[i]]
  while(length(candidatos) > 4){
    
    modelos[[i]] <- glm(YYY ~ . , family = "binomial",
                        data = prov[,c(candidatos,ncol(prov))] , maxit=1000)
    
    resultado[[i]] <- car::Anova(modelos[[i]])
    
    pior <- which.max(resultado[[i]][,3])
    
    candidatos <- candidatos[-pior]
    
    campeoes[[i]] <- colnames(prov[,candidatos])
    
    quallis[[i]] <- candidatos
  }
}

unlist(unlist( lapply(resultado , rownames) ))
sobraron <- unlist(quallis)

lista_final_t1 <- colnames(prov[,sobraron])


paste(lista_final_tudo, collapse = ',')
paste(lista_final_t1, collapse = ',')


library(keras)
library(dplyr)

# ALEMANHA
# Carregando funcoes erros

# m varia de 1 a 7.
# Fazendo um mercado por vez, checando ao termino de cada rodada.
m = 4
source("erros_fun.R")
paises <- c('ALE', 'BRA', 'CHN', 'FRA', 'JAP', 'UK', 'USA')

# Script inicial
base_completa2 <- readr::read_csv2(paste0('bases_finais_sem_col_zoadas/nao_bugadas_',paises[m],'.csv') )

# Gravando colunas Y e ATIVO para nao entrarem no modelo.
col_Y <- which(colnames(base_completa2) == "YYY")
col_ATIVO <- which(colnames(base_completa2) == "ATIVO")

# Base de treino: primeiras 75% observacoes.
treino_id <- seq_len( floor(0.75*nrow(base_completa2) ) )

# Gravando parametros de treino para escalonar
medias  <- apply(X = base_completa2[treino_id  , -c(col_ATIVO, col_Y) ], 
                 MARGIN = 2, mean)

desvios <- apply(X = base_completa2[treino_id  , -c(col_ATIVO, col_Y) ], 
                 MARGIN = 2, sd ) 

x_test <- list()

# Padronizando dados_teste com parametros de treino
    for (i in 1: length(medias) ) {
      x_test[[i]]  <- (base_completa2[-treino_id , -c(col_ATIVO, col_Y) ][,i] - medias[i]) / desvios[i]
    }

x_test  <- data.frame(x_test)
names(x_test) <- colnames(base_completa2)[-c(col_ATIVO, col_Y)]

# Base para treino escalonada
x_train <- base_completa2[treino_id  , -c(col_ATIVO, col_Y) ] %>% scale()  

# Criando matriz teste e vetores y para formato do keras
x_test <- as.matrix(x_test)

y_train <- ifelse( base_completa2[treino_id,col_Y] == 1 , 1 , 0) %>%
           as.matrix() %>%
           to_categorical(y_train, num_classes = 2)

y_test  <- ifelse( base_completa2[-treino_id,col_Y] == 1 , 1 , 0) %>%
           as.matrix() %>%
           to_categorical(y_test, num_classes = 2)


preco_puro <- base_completa2[ - treino_id, "CLOSE" ]
open_puro  <- base_completa2[ - treino_id, "OPEN"  ]
# Indicadores retirados: "ARATIO", "BRATIO", "ADO", "DIU", "DID", "VARR"
# Todoas as colunas

colunas <- list()

colunas$todas <- colnames(x_train)

colunas$todas_tab1 <- c("SMA","WMA","EMA","MOM","STOCH_K","STOCH_D","STOCH_D_SLOW",
                        "RSI","MACD","WILL_R","CCI","ROC","DISP","OSCP","PSY",
                        "BIAS","VOLR","ATR","BB_UP","BB_LOW","DMI","KC_U","KC_L","TRIMA",
                        "MAE_UP","MAE_LOW","REX","NVI","PVI","VAMA","HPR","LPR","MAXX",
                        "MINN","VMOM","MPP", "PPO","SAR","OBV","VOLAT","MFI",
                        "MQO_BETA","OPEN","CLOSE","VOLUME")  

# Lasso
colunas$lasso <- read.csv2(paste0('Feature_Selection/lasso_tudo_',paises[m],'.csv'),
                           header = T)$x %>% as.character()

colunas$lasso_tab1 <- read.csv2(paste0('Feature_Selection/lasso_tab1_',paises[m],'.csv'),
                                header = T)$x %>% as.character()
  
# Stepwise
colunas$stepwise <- read.csv2(paste0('Feature_Selection/stepwise_tudo_',paises[m],'.csv'),
                              header = T)$x %>% as.character()

colunas$stepwise_tab1 <- read.csv2(paste0('Feature_Selection/stepwise_tab1_',paises[m],'.csv'),
                                   header = T)$x %>% as.character()

# Torneios
colunas$torneio <- read.csv2(paste0('Feature_Selection/torneios_tudo_',paises[m],'.csv'),
                             header = T)$x %>% as.character()

colunas$torneio_tab1 <- read.csv2(paste0('Feature_Selection/torneios_tab1_',paises[m],'.csv'),
                                  header = T)$x %>% as.character()

# i = 1
# k = 'sigmoid'
# j = 0

##### -------------------- Cenario 1: --------------------- ###### 

metricas <- list()
indicadores <- list()

for (i in 1:length(colunas)){
  
  colunas_sel <- colunas[[i]]
  
  for (k in c('sigmoid') ){
    
    for(j in c(0,0.3) ){
      
      model <- keras_model_sequential() 
      
      # Definindo arquitetura da rede
      model %>% 
        layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
        layer_dropout(rate = j) %>% 
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 2, activation = 'sigmoid')
      
      # Funcao de perda, metodo de otimizacao e metrica 
      model %>% 
        compile(
          loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy')
        )
      
      # Treina o modelo. (Definir outro epochs e batch_size ?)
      history <- model %>% fit(
        x_train[ , colunas_sel ], y_train, 
        epochs = 40, batch_size = 128, 
        validation_split = 0.2
      )
      #plot(history)
      
      # data.frame com ativo, valor_real, valor_predito, e preco para calcular erros tipo 1 e 2
      indicadores[[ length(indicadores) + 1 ]] <- data.frame(ativo = base_completa2[ - treino_id , "ATIVO"],
                                                             y_teste = y_test,
                                                             preditos = predict_classes(model, x_test[, colunas_sel]),
                                                             preco_close = preco_puro,
                                                             preco_open  = open_puro)
      
      # matriz de confusao para calculo das metricas  
      confusao <- table(indicadores[[ length(indicadores) ]]$y_teste.2,
                        indicadores[[ length(indicadores) ]]$preditos)
      
      # Calculando quatro metricas
      acc    <- sum(diag(confusao)) / sum(confusao)
      
      prec   <- tryCatch( confusao[2,2] / sum(confusao[,2]),
                          error = function(e) NaN )
      
      recall <- tryCatch( confusao[2,2] / sum(confusao[2,]),
                          error = function(e) NaN )
        
      fscore <- ((1/prec + 1/recall) / 2 )^(-1)
      erro_1 <- type_i ( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      erro_2 <- type_ii( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      
      # data.frame de metricas 
      # Accuracy_out eh igual a Acc (apenas por precaucao)
      metricas[[ length(metricas) + 1]] <- data.frame(Variables  = names(colunas[i]),
                                                      Hidden_layers = 3,
                                                      Dropout    = j,
                                                      Activation = k,
                                                      Loss = model %>% 
                                                        evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                        .$loss,
                                                      Accuracy_in = history$metrics$val_acc[[40]],
                                                      Accuracy_out   = model %>% 
                                                                       evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                                       .$acc,
                                                      Acc    = acc,
                                                      Prec   = prec,
                                                      Recall = recall,
                                                      Fscore = fscore,
                                                      erro_I = erro_1,
                                                      erro_II = erro_2
                                                      )
    }
  }
}

# Salvar e soh escrever depois da terceira rede neural.
metricas1     <- do.call( rbind, metricas )
saveRDS(indicadores, paste0("indicadores_",paises[m],"_1") )

##### -------------------- Fim Cenario 1: --------------------- ###### 



#### ======== CENARIO 2 ========== #####
metricas <- list()
indicadores <- list()

for (i in 1:length(colunas)){
  
  colunas_sel <- colunas[[i]]
  
  for (k in c('sigmoid') ){
    
    for(j in c(0,0.3) ){
      
      model <- keras_model_sequential() 
      
      # Definindo arquitetura da rede
      model %>% 
        layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
        layer_dropout(rate = j) %>% 
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 2, activation = 'sigmoid')
      
      # Funcao de perda, metodo de otimizacao e metrica 
      model %>% 
        compile(
          loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy')
        )
      
      # Treina o modelo. (Definir outro epochs e batch_size ?)
      history <- model %>% fit(
        x_train[ , colunas_sel ], y_train, 
        epochs = 400, batch_size = 128, 
        validation_split = 0.2
      )
      #plot(history)
      
      # data.frame com ativo, valor_real, valor_predito, e preco para calcular erros tipo 1 e 2
      indicadores[[ length(indicadores) + 1 ]] <- data.frame(ativo = base_completa2[ - treino_id , "ATIVO"],
                                                             y_teste = y_test,
                                                             preditos = predict_classes(model, x_test[, colunas_sel]),
                                                             preco_close = preco_puro,
                                                             preco_open  = open_puro)
      
      # matriz de confusao para calculo das metricas
      confusao <- table(indicadores[[ length(indicadores) ]]$y_teste.2,
                        indicadores[[ length(indicadores) ]]$preditos)
      
      # Calculando quatro metricas
      acc    <- sum(diag(confusao)) / sum(confusao)
      prec   <- confusao[2,2] / sum(confusao[,2])
      recall <- confusao[2,2] / sum(confusao[2,])
      fscore <- ((1/prec + 1/recall) / 2 )^(-1)
      erro_1 <- type_i ( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      erro_2 <- type_ii( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      
      # data.frame de metricas 
      # Accuracy_out eh igual a Acc (apenas por precaucao)
      metricas[[ length(metricas) + 1]] <- data.frame(Variables  = names(colunas[i]),
                                                      Hidden_layers = 3,
                                                      Dropout    = j,
                                                      Activation = k,
                                                      Loss = model %>% 
                                                        evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                        .$loss,
                                                      Accuracy_in = history$metrics$val_acc[[400]],
                                                      Accuracy_out   = model %>% 
                                                        evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                        .$acc,
                                                      Acc    = acc,
                                                      Prec   = prec,
                                                      Recall = recall,
                                                      Fscore = fscore,
                                                      erro_I = erro_1,
                                                      erro_II = erro_2
      )
    }
  }
}

metricas2     <- do.call( rbind, metricas )
saveRDS(indicadores, paste0("indicadores_",paises[m],"_2") )
##### -------------------- Fim Cenario 2: --------------------- ###### 



#### ======== CENARIO 3 ========== #####
metricas <- list()
indicadores <- list()

for (i in 1:length(colunas)){
  
  colunas_sel <- colunas[[i]]
  
  for (k in c('sigmoid') ){
    
    for(j in c(0,0.3) ){
      
      model <- keras_model_sequential() 
      
      # Definindo arquitetura da rede
      model %>% 
        layer_dense(units = 15, activation = k, input_shape = length(colunas[[i]]) ) %>% 
        layer_dropout(rate = j) %>% 
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 15, activation = k) %>%
        layer_dropout(rate = j) %>%
        layer_dense(units = 2, activation = 'sigmoid')
      
      # Funcao de perda, metodo de otimizacao e metrica 
      model %>% 
        compile(
          loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy')
        )
      
      # Treina o modelo. (Definir outro epochs e batch_size ?)
      history <- model %>% fit(
        x_train[ , colunas_sel ], y_train, 
        epochs = 400, batch_size = 128, 
        validation_split = 0.2
      )
      #plot(history)
      
      # data.frame com ativo, valor_real, valor_predito, e preco para calcular erros tipo 1 e 2
      indicadores[[ length(indicadores) + 1 ]] <- data.frame(ativo = base_completa2[ - treino_id , "ATIVO"],
                                                             y_teste = y_test,
                                                             preditos = predict_classes(model, x_test[, colunas_sel]),
                                                             preco_close = preco_puro,
                                                             preco_open  = open_puro)
      
      # Transformando em fatores com niveis definidos
      # para o caso em que temos apenas um nivel de valores preditos.
      pred <- factor( indicadores[[ length(indicadores) ]]$preditos , levels = c("0","1") ) 
      obs  <- factor( indicadores[[ length(indicadores) ]]$y_teste.2, levels = c("0","1") )
      
      # matriz de confusao para calculo das metricas
      confusao <- table(obs, pred)
      
      # Calculando quatro metricas
      acc    <- sum(diag(confusao)) / sum(confusao)
      prec   <- confusao[2,2] / sum(confusao[,2])
      recall <- confusao[2,2] / sum(confusao[2,])
      fscore <- ((1/prec + 1/recall) / 2 )^(-1)
      erro_1 <- type_i ( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      erro_2 <- type_ii( indicadores[[ length(indicadores) ]] ) %>% as.numeric()
      
      # data.frame de metricas 
      # Accuracy_out eh igual a Acc (apenas por precaucao)
      metricas[[ length(metricas) + 1]] <- data.frame(Variables  = names(colunas[i]),
                                                      Hidden_layers = 3,
                                                      Dropout    = j,
                                                      Activation = k,
                                                      Loss = model %>% 
                                                        evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                        .$loss,
                                                      Accuracy_in = history$metrics$val_acc[[400]],
                                                      Accuracy_out   = model %>% 
                                                        evaluate(x_test[ , colunas_sel ], y_test) %>% 
                                                        .$acc,
                                                      Acc    = acc,
                                                      Prec   = prec,
                                                      Recall = recall,
                                                      Fscore = fscore,
                                                      erro_I = erro_1,
                                                      erro_II = erro_2
      )
    }
  }
}

metricas3     <- do.call( rbind, metricas )
saveRDS(indicadores, paste0("indicadores_",paises[m],"_3") )
##### -------------------- Fim Cenario 3: --------------------- ###### 


# Juntando as metricas dos tres cenarios (16 x 3 = 48 linhas)
metricas_final <- rbind(metricas1, metricas2, metricas3)

write.csv2(metricas_final,
           file =  paste0("metricas_final_",paises[m],".csv" ) ,
           row.names = F)

# Buy and Hold
BandH <- x_test[1, "CLOSE"] - x_test[nrow(x_test), "CLOSE"]
save(BandH, file= paste0("BandH_",paises[m],".RData") )


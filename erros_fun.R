## Mudar para calcular a diferenca:
## Fechamento de hoje - Preco de amanha
## Deixar funcao generica para diferencas

type_i <- function(x) {
  
  x1 <- x %>%
    mutate(Ganho_ou_Perda = ifelse(preditos == 1 & y_teste.2 == 1 , "Ganho",
                                   ifelse(preditos == 1 & y_teste.2 == 0 , "Perda",
                                          "Nao_aplicavel"))) %>%
    group_by(ATIVO) %>%
    #mutate(Diferenca = c(diff(CLOSE),NA)) %>%
    mutate(Diferenca = c(NA,c(OPEN[2:n()] - CLOSE[1:(n()-1)]))) %>% 
    group_by(ATIVO, Ganho_ou_Perda) %>%
    summarise(Media = mean(Diferenca, na.rm = T))
  
  x1 <- x1 %>%
    filter(sum(Ganho_ou_Perda %in% c("Ganho","Perda")) == 2 ) %>%
    summarise(Contraste = Media[Ganho_ou_Perda == "Ganho"] + Media[Ganho_ou_Perda == "Perda"]) %>%
    summarise(Erro_tipo1 = mean(Contraste))
              #Tipo_Erro   = "I")
  
  return(x1)
  
}

type_ii <- function(x) {
  
  x1 <- x %>%
    mutate(Ganho_ou_Perda = ifelse(preditos == 0 & y_teste.2 == 0 , "Ganho",
                                   ifelse(preditos == 0 & y_teste.2 == 1 , "Perda",
                                          "Nao_aplicavel"))) %>%
    group_by(ATIVO) %>%
   #mutate(Diferenca = c(diff(CLOSE),NA)) %>%
    mutate(Diferenca = c(NA,c(OPEN[2:n()] - CLOSE[1:(n()-1)]))) %>%
    group_by(ATIVO, Ganho_ou_Perda) %>%
    summarise(Media = mean(Diferenca, na.rm = T))
  
  x1 <- x1 %>%
    filter(sum(Ganho_ou_Perda %in% c("Ganho","Perda")) == 2 ) %>%
    summarise(Contraste = Media[Ganho_ou_Perda == "Ganho"] + Media[Ganho_ou_Perda == "Perda"]) %>%
    summarise(Erro_tipo2 = mean(Contraste))
              #Tipo_Erro   = "II")
  
  return(x1)
  
}

loadRData <- function(file) {
  load(file)
  get(ls()[ls() != "file" ])
}
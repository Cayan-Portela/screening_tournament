
usa <- read.csv2('metricas_final_USA.csv', head = T) # 1 e 12
jap <- read.csv2('metricas_final_JAP.csv', head = T) # 1 e 12
uk <- read.csv2('metricas_final_UK.csv', head = T)   # 1 e 12
bra <- read.csv2('metricas_final_BRA.csv', head = T) # 1 e 12
chn <- read.csv2('metricas_final_CHN.csv', head = T) # 1 e 12
fra <- read.csv2('metricas_final_FRA.csv', head = T) # 1 e 12
ale <- read.csv2('metricas_final_ALE.csv', head = T) # 1
# Valores Logicos reciclados para pegar de dois em dois.
# T T F F pega tab1 e tab2 (tab12)
# F F T T pega so tab1

so_tab12 <- ale [ rep( c(T,T,F,F), 12 ) , ]
so_tab12$Acc
dados_sep <- data.frame(Mercado   = "S&P_100",
                        Tabela    = rep("Tab_12",24),
                        Metodo    = c(rep(c("None","SFFS","TS","LASSO"), each = 6)),
                        Hidd_Lay  = c(rep(c(3,3,5,5,7,7), times = 4) ),
                        Dropout   = c(rep(c(0,3), times = 12)),
                        Acuracia  = so_tab12$Acc,  #c(None_acc, SFFS_acc, TS_acc, LASSO_acc),
                        Precision = so_tab12$Prec, #c(None_prc, SFFS_prc, TS_prc, LASSO_prc),
                        Recall    = so_tab12$Recall,  #c(None_rec, SFFS_rec, TS_rec, LASSO_rec),
                        Fscore    = so_tab12$Fscore) #c(None_fsc, SFFS_fsc, TS_fsc, LASSO_fsc))

dados_sep
#tb1_sep
##### Comecando a visualizacao ######
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
#dados_sep <- rbind(tb12_sep, tb1_sep)

# Ler no point click?
metricas_logistica <- get(load('/home/cayan/Área de Trabalho/Doutorado/artigo_torneios/Resultados_ALEMANHA/metricas_logistica.RData'))

logistica <- data.frame(Mercado = "S&P_100",
                        Tabela  = rep(c("Tab_12","Tab_1"), each = 4),
                        Metodo = c("None","LASSO","SFFS","TS"), 
                        Hidd_Lay = "L",
                        Dropout = 0,
                        Acuracia = metricas_logistica$Acuracia[c(1,3,5,7)],
                        Precision = metricas_logistica$Precisao[c(1,3,5,7)],
                        Recall = metricas_logistica$Recall[c(1,3,5,7)],
                        Fscore = metricas_logistica$Fscore[c(1,3,5,7)])

dados_sep2 <- rbind(dados_sep, logistica %>% filter(Tabela == "Tab_12"))

dados2 <- dados_sep2 %>%
          tidyr::gather("Metrica" , "Valor" , 6:9) %>%
          mutate(Metodo_Metrica = paste(Metodo, Metrica, sep = "_"),
                 Layer_Dropout  = paste(Hidd_Lay, Dropout, sep = "_"),
                 Var_Completa   = paste(Metodo, Hidd_Lay, Dropout, sep = "_") )

dados2$Hidd_Lay <- as.factor(dados2$Hidd_Lay)
cores <- c('deepskyblue','deepskyblue4','darkblue','black')
names(cores) <- levels(dados2$Hidd_Lay)

d_teste <- dados2 %>% arrange(desc(Valor)) %>% 
           filter(Tabela %in% c( "Tab_12") & Metrica == "Acuracia") 
           #filter(Tabela == "Tab_1e2")          


# Criando vetor pra x axis ticks.
Var_completa <- str_sub(d_teste$Var_Completa, 1, -5) #as.factor(d_teste$Var_Completa)  
names(Var_completa) <- d_teste$Var_Completa

plot_acc <- ggplot(d_teste,
       aes(x=reorder(Var_Completa, Valor), 
           y=Valor, label=round(Valor,2))) + 
  geom_point(stat='identity', 
             aes(color = Hidd_Lay ),
             fill="black", size=8)  +
  #geom_point(aes(size = Dropout, shape = NA)) +
  geom_segment(aes(y = 0.2, 
                   x = Var_Completa, 
                   yend = Valor-0.015, 
                   xend = Var_Completa), 
               color = "grey69") +
  geom_text(color="white", size=3) +
  annotate("text",  x = reorder(d_teste$Var_Completa[which(d_teste$Dropout == 3)], 
                                d_teste$Valor[which(d_teste$Dropout == 3)]),
                    y = sort(d_teste$Valor[which(d_teste$Dropout == 3)],
                             decreasing = T) + 0.02,
           label = '*', col = "firebrick", size = 8) +
  labs(title="", 
       caption = "The * goes for dropout rate of 0.3.") + 
  xlab("") + ylab("") +
  scale_color_manual(name = "Number of \nhidden layers", 
                     values = cores, labels = c("3","5","7","Logistic")) +
  scale_x_discrete(labels= Var_completa) +
  ylim(0.2, 0.9) +
  coord_flip() + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.subtitle = element_text(color = c("firebrick","black")),
        plot.caption = element_text(size = 11),
        plot.title = element_text(hjus =  0.5),
        axis.text.y = element_text(size = 13)) 


plot_acc



png("ALE_acc_tab12.png", height = 650, width = 1000)
# 2. Create a plot
plot_acc
# Close the pdf file
dev.off() 

  #### ACABA AQUI !!!! ####






# 
# d_prec <- dados2 %>% arrange(desc(Valor)) %>% 
#           filter(Metrica == "Precision" & Tabela == "Tab_1e2")
# 
# plot_prec <- ggplot(d_prec,
#                    aes(x=reorder(Var_Completa, Valor), 
#                        y=Valor, label=round(Valor,2))) + 
#   geom_point(stat='identity', 
#              aes(color = Hidd_Lay ),
#              fill="black", size=8)  +
#   #geom_point(aes(size = Dropout, shape = NA)) +
#   geom_segment(aes(y = 0.45, 
#                    x = Var_Completa, 
#                    yend = Valor-0.01, 
#                    xend = Var_Completa), 
#                color = "grey69") +
#   geom_text(color="white", size=3) +
#   annotate("text",  x = reorder(d_prec$Var_Completa[which(d_prec$Dropout == 3)], 
#                                 d_prec$Valor[which(d_prec$Dropout == 3)]),
#            y = sort(d_prec$Valor[which(d_prec$Dropout == 3)],
#                     decreasing = T) + 0.01,
#            label = '*', col = "firebrick", size = 8) +
#   labs(title="Precision", 
#        caption = "The * goes for dropout rate of 0.3.") + 
#   xlab("") + ylab("") +
#   scale_color_manual(name = "Number of \nhidden layers", 
#                      values = cores, labels = c("3","5","7","Logistic")) +
#   scale_x_discrete(labels= Var_completa) +
#   ylim(0.45, 0.75) +
#   coord_flip() + 
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.subtitle = element_text(color = c("firebrick","black")),
#         plot.caption = element_text(size = 11),
#         plot.title = element_text(hjus =  0.5),
#         axis.text.y = element_text(size = 13)) 
# 
# 
# plot_prec
# 
# d_rec <- dados2 %>% arrange(desc(Valor)) %>% 
#   filter(Metrica == "Recall" & Tabela == "Tab_1e2")
# 
# plot_rec <- ggplot(d_rec,
#                     aes(x=reorder(Var_Completa, Valor), 
#                         y=Valor, label=round(Valor,2))) + 
#   geom_point(stat='identity', 
#              aes(color = Hidd_Lay ),
#              fill="black", size=8)  +
#   #geom_point(aes(size = Dropout, shape = NA)) +
#   geom_segment(aes(y = 0.4, 
#                    x = Var_Completa, 
#                    yend = Valor-0.015, 
#                    xend = Var_Completa), 
#                color = "grey69") +
#   geom_text(color="white", size=3) +
#   annotate("text",  x = reorder(d_rec$Var_Completa[which(d_rec$Dropout == 3)], 
#                                 d_rec$Valor[which(d_rec$Dropout == 3)]),
#            y = sort(d_rec$Valor[which(d_rec$Dropout == 3)],
#                     decreasing = T) + 0.02,
#            label = '*', col = "firebrick", size = 8) +
#   labs(title="Recall", 
#        caption = "The * goes for dropout rate of 0.3.") + 
#   xlab("") + ylab("") +
#   scale_color_manual(name = "Number of \nhidden layers", 
#                      values = cores, labels = c("3","5","7","Logistic")) +
#   scale_x_discrete(labels= Var_completa) +
#   ylim(0.4, 0.9) +
#   coord_flip() + 
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.subtitle = element_text(color = c("firebrick","black")),
#         plot.caption = element_text(size = 11),
#         plot.title = element_text(hjus =  0.5),
#         axis.text.y = element_text(size = 13)) 
# 
# plot_rec
# 
# d_fsc <- dados2 %>% arrange(desc(Valor)) %>% 
#   filter(Metrica == "Fscore" & Tabela == "Tab_1e2")
# 
# plot_fsc <- ggplot(d_fsc,
#                    aes(x=reorder(Var_Completa, Valor), 
#                        y=Valor, label=round(Valor,2))) + 
#   geom_point(stat='identity', 
#              aes(color = Hidd_Lay ),
#              fill="black", size=8)  +
#   #geom_point(aes(size = Dropout, shape = NA)) +
#   geom_segment(aes(y = 0.4, 
#                    x = Var_Completa, 
#                    yend = Valor-0.008, 
#                    xend = Var_Completa), 
#                color = "grey69") +
#   geom_text(color="white", size=3) +
#   annotate("text",  x = reorder(d_fsc$Var_Completa[which(d_fsc$Dropout == 3)], 
#                                 d_fsc$Valor[which(d_fsc$Dropout == 3)]),
#            y = sort(d_fsc$Valor[which(d_fsc$Dropout == 3)],
#                     decreasing = T) + 0.01,
#            label = '*', col = "firebrick", size = 8) +
#   labs(title="F-score", 
#        caption = "The * goes for dropout rate of 0.3.") + 
#   xlab("") + ylab("") +
#   scale_color_manual(name = "Number of \nhidden layers", 
#                      values = cores, labels = c("3","5","7","Logistic")) +
#   scale_x_discrete(labels= Var_completa) +
#   ylim(0.4, 0.8) +
#   coord_flip() + 
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.subtitle = element_text(color = c("firebrick","black")),
#         plot.caption = element_text(size = 11),
#         plot.title = element_text(hjus =  0.5),
#         axis.text.y = element_text(size = 13)) 
# 
# 
# plot_fsc
# 
# 
# 
# grid.arrange(plot_acc, plot_prec, plot_rec, plot_fsc,
#              ncol = 2, nrow = 2)
# 
# 
# # dados2 %>% 
# #   filter(Tabela == "Tab_1e2") %>%
# # ggplot(aes(x = Layer_Dropout,
# #                    y = Metodo_Metrica)) +
# #   geom_tile(aes(fill= Valor, size = 2) ) +
# #   geom_text(aes(label = round(Valor,2) ),
# #             color = "white",
# #             #parse = T,
# #             check_overlap = T) +
# #   scale_fill_gradient2( low = "red", high = "blue", mid = "white",
# #     limits = c(0.48, 0.81),
# #     name = "internet users / 100 people",
# #     guide = guide_colorbar(
# #       direction = "horizontal",
# #       label.position = "bottom",
# #       title.position = "top",
# #       ticks = FALSE,
# #       barwidth = grid::unit(3.5, "in"),
# #       barheight = grid::unit(0.2, "in")
# #     )) +
# #   theme_minimal() +
# #   theme(
# #     axis.line = element_blank(),
# #     axis.ticks = element_blank(),
# #    # axis.ticks.length = grid::unit(1, "pt"),
# #     plot.margin = unit(c(1, 1, 4, 1), "lines"),
# #    # axis.title.x = element_blank(),
# #   #  axis.text.x = element_blank(),
# #     panel.grid.major.x = element_blank(),
# #     panel.grid.minor.x = element_blank(),
# #     panel.spacing = unit(0, "lines"),
# #     legend.position = "top",
# #     legend.justification = "left",
# #     legend.title.align = 0.5,
# #     legend.title = element_text(size = 12*12/14)
# #   )
# #   
# 

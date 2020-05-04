install.packages("data.table")
install.packages("magrittr")
install.packages("corrplot")
install.packages("dplyr")
install.packages("plyr")

library(corrplot)
library(data.table)
library(magrittr)
library(dplyr)
library(plyr) 

df_total_alta = read.csv(file = './src/alta-regiao-2-alta - Página1.csv')
df_total_baixa = read.csv(file = './src/alta-regiao-2-baixa - Página1.csv')

# install.packages("tidyverse")
# library(tidyverse)

# dadosExemplo <- tibble(
#   A = c(-17, -22, 19, 0.21, 0.96),
#   B = c(-10, -13, 0.62, 0.88, 13),
#   C = c(11, -20, -0.36, 14, 0.8),
#   D = c(-0.55, 13, 14, -12, 0.09),
#   E = c(-0.17, 0.32, -0.75, -0.5, 14),
#   F = c(-0.8, 0.54, -23, 11, 10)
# )

# Utils
# =================================
# apply(dadosExemplo, MARGIN = 2, FUN = mean)
# colnames(df_total_alta)

# ====================================================================
# 1) Análise de frequências (1,0 ponto):
# - a) Em quais anos a doença_café foi mais incidente? Comente. 
# seleção de arquivo via menus

converte = function(x){
  if(x < 20.1) return("baixo") 
  else if(x > 60) return("alto") 
  else return("medio") 
}

for(i in 1:nrow(df_total_alta))
  df_total_alta$c_alta_categoria[i] = converte(df_total_alta$alta[i])

for(i in 1:nrow(df_total_baixa))
  df_total_baixa$c_alta_categoria[i] = converte(df_total_baixa$baixa[i])

dados_doenca_alta = df_total_alta[,c(1,2,28)]
dados_doenca_baixa = df_total_baixa[,c(1,2,28)]

result_dados_doenca_alta <- dados_doenca_alta[dados_doenca_alta$c_alta_categoria == "alto",]
result_dados_doenca_baixa <- dados_doenca_baixa[dados_doenca_baixa$c_alta_categoria == "baixo",]

result_dados_doenca_alta
result_dados_doenca_baixa
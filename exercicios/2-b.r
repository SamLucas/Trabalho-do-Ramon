install.packages("data.table")
install.packages("magrittr")
install.packages("corrplot")
install.packages("psych")
install.packages("dplyr")
install.packages("plyr")

library(corrplot)
library(data.table)
library(magrittr)
library(dplyr)
library(psych)
library(plyr) 

df_total_alta = read.csv(file = './src/alta-regiao-2-alta - Página1.csv')
df_total_baixa = read.csv(file = './src/alta-regiao-2-baixa - Página1.csv')

# Utils
# =================================
# apply(dadosExemplo, MARGIN = 2, FUN = mean)
# colnames(df_total_alta)

# ====================================================================
# 2) Análise de correlação e regressão linear (3,0 pontos) Nas análises de correlação e regressão linear, você deve
# - b) Gerar modelos de regressão linear simples e múltipla para previsão da doença do café.

converte = function(x){
  if(x < 20.1) return("baixo") 
  else if(x > 60) return("alto") 
  else return("medio") 
}

for(i in 1:nrow(df_total_alta))
  df_total_alta$alta_categoria[i] = converte(df_total_alta$alta[i])

for(i in 1:nrow(df_total_baixa))
  df_total_baixa$alta_categoria[i] = converte(df_total_baixa$baixa[i])

df_total_alta = df_total_alta[,c(27,1,2,28,3:26)]
df_total_baixa = df_total_baixa[,c(27,1,2,28,3:26)]

df_total_alta[,5:28] <- lapply(df_total_alta[,5:28],function(x){
    options(digits=5)
    as.double(sub(",", ".", x, fixed = TRUE))
  }
)

df_total_baixa[,5:28] <- lapply(df_total_baixa[,5:28],function(x){
    options(digits=5)
    as.double(sub(",", ".", x, fixed = TRUE))
  }
)

df_total_alta$P = apply(df_total_alta[,6:8], 1, mean)
df_total_alta$UR = apply(df_total_alta[,9:11], 1, mean)
df_total_alta$TMAX = apply(df_total_alta[,12:14], 1, mean)
df_total_alta$TMIN = apply(df_total_alta[,15:17], 1, mean)
df_total_alta$NDR.1mm = apply(df_total_alta[,18:20], 1, mean)
df_total_alta$NDR.10mm = apply(df_total_alta[,21:23], 1, mean)
df_total_alta$NRH80 = apply(df_total_alta[,24:26], 1, mean)
df_total_alta$NRH90 = apply(df_total_alta[,27:29], 1, mean)

df_total_baixa$P = apply(df_total_baixa[,6:8], 1, mean)
df_total_baixa$UR = apply(df_total_baixa[,9:11], 1, mean)
df_total_baixa$TMAX = apply(df_total_baixa[,12:14], 1, mean)
df_total_baixa$TMIN = apply(df_total_baixa[,15:17], 1, mean)
df_total_baixa$NDR.1mm = apply(df_total_baixa[,18:20], 1, mean)
df_total_baixa$NDR.10mm = apply(df_total_baixa[,21:23], 1, mean)
df_total_baixa$NRH80 = apply(df_total_baixa[,24:26], 1, mean)
df_total_baixa$NRH90 = apply(df_total_baixa[,27:29], 1, mean)

df_total_sA = df_total_alta[,c(1:5,29:36)]
df_total_sB = df_total_baixa[,c(1:5,29:36)]

teste = data.frame(
  df_total_sA$alta,
  df_total_sA$P,
  df_total_sA$UR,
  df_total_sA$TMAX,
  df_total_sA$TMIN,
  df_total_sA$NDR.1mm,
  df_total_sA$NDR.10mm,
  df_total_sA$NRH80,
  df_total_alta_aux$NRH90
)

M <- cor(teste)
corrplot(M,method = "circle")

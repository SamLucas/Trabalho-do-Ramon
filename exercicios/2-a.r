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
# - a) Gerar correlações entre a variável doença_café e as demais variáveis.

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

df_total_baixa

alta_mes_janeiro <- df_total_alta[df_total_alta$mes == "janeiro",]
alta_mes_fevereiro <- df_total_alta[df_total_alta$mes == "fevereiro",]
alta_mes_marco <- df_total_alta[df_total_alta$mes == "marco",]
alta_mes_abril <- df_total_alta[df_total_alta$mes == "abril",]
alta_mes_maio <- df_total_alta[df_total_alta$mes == "maio",]
alta_mes_junho <- df_total_alta[df_total_alta$mes == "junho",]
alta_mes_julho <- df_total_alta[df_total_alta$mes == "julho",]
alta_mes_agosto <- df_total_alta[df_total_alta$mes == "agosto",]
alta_mes_setembro <- df_total_alta[df_total_alta$mes == "setembro",]
alta_mes_outubro <- df_total_alta[df_total_alta$mes == "outubro",]
alta_mes_novembro <- df_total_alta[df_total_alta$mes == "novembro",]
alta_mes_dezembro <- df_total_alta[df_total_alta$mes == "dezembro",]

baixa_mes_janeiro <- df_total_baixa[df_total_baixa$mes == "janeiro",]
baixa_mes_fevereiro <- df_total_baixa[df_total_baixa$mes == "fevereiro",]
baixa_mes_marco <- df_total_baixa[df_total_baixa$mes == "marco",]
baixa_mes_abril <- df_total_baixa[df_total_baixa$mes == "abril",]
baixa_mes_maio <- df_total_baixa[df_total_baixa$mes == "maio",]
baixa_mes_junho <- df_total_baixa[df_total_baixa$mes == "junho",]
baixa_mes_julho <- df_total_baixa[df_total_baixa$mes == "julho",]
baixa_mes_agosto <- df_total_baixa[df_total_baixa$mes == "agosto",]
baixa_mes_setembro <- df_total_baixa[df_total_baixa$mes == "setembro",]
baixa_mes_outubro <- df_total_baixa[df_total_baixa$mes == "outubro",]
baixa_mes_novembro <- df_total_baixa[df_total_baixa$mes == "novembro",]
baixa_mes_dezembro <- df_total_baixa[df_total_baixa$mes == "dezembro",]


media_alta_mes_janeiro <- mean(alta_mes_janeiro$alta)
media_alta_mes_fevereiro <- mean(alta_mes_fevereiro$alta)
media_alta_mes_marco <- mean(alta_mes_marco$alta)
media_alta_mes_abril <- mean(alta_mes_abril$alta)
media_alta_mes_maio <- mean(alta_mes_maio$alta)
media_alta_mes_junho <- mean(alta_mes_junho$alta)
media_alta_mes_julho <- mean(alta_mes_julho$alta)
media_alta_mes_agosto <- mean(alta_mes_agosto$alta)
media_alta_mes_setembro <- mean(alta_mes_setembro$alta)
media_alta_mes_outubro <- mean(alta_mes_outubro$alta)
media_alta_mes_novembro <- mean(alta_mes_novembro$alta)
media_alta_mes_dezembro <- mean(alta_mes_dezembro$alta)

media_baixa_mes_janeiro <- mean(baixa_mes_janeiro$baixa)
media_baixa_mes_fevereiro <- mean(baixa_mes_fevereiro$baixa)
media_baixa_mes_marco <- mean(baixa_mes_marco$baixa)
media_baixa_mes_abril <- mean(baixa_mes_abril$baixa)
media_baixa_mes_maio <- mean(baixa_mes_maio$baixa)
media_baixa_mes_junho <- mean(baixa_mes_junho$baixa)
media_baixa_mes_julho <- mean(baixa_mes_julho$baixa)
media_baixa_mes_agosto <- mean(baixa_mes_agosto$baixa)
media_baixa_mes_setembro <- mean(baixa_mes_setembro$baixa)
media_baixa_mes_outubro <- mean(baixa_mes_outubro$baixa)
media_baixa_mes_novembro <- mean(baixa_mes_novembro$baixa)
media_baixa_mes_dezembro <- mean(baixa_mes_dezembro$baixa)

colnames(alta_mes_novembro)

df_total_sA = df_total_alta[,c(1:5,29:36)]
df_total_sB = df_total_baixa[,c(1:5,29:36)]

df_alta = data.frame(
  df_total_sA$alta,
  df_total_sA$P,
  df_total_sA$UR,
  df_total_sA$TMAX,
  df_total_sA$TMIN,
  df_total_sA$NDR.1mm,
  df_total_sA$NDR.10mm,
  df_total_sA$NRH80,
  df_total_sA$NRH90
)

M_alta <- cor(df_alta)
corrplot(M_alta,method = "circle")

df_baixa = data.frame(
  df_total_sB$alta,
  df_total_sB$P,
  df_total_sB$UR,
  df_total_sB$TMAX,
  df_total_sB$TMIN,
  df_total_sB$NDR.1mm,
  df_total_sB$NDR.10mm,
  df_total_sB$NRH80,
  df_total_sB$NRH90
)

M_baixa <- cor(df_baixa)
corrplot(M_baixa,method = "circle")

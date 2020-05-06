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

media_alta_mes <- data.frame(
  MES = c("janeiro"
  ,"fevereiro"
  ,"marco"
  ,"abril"
  ,"maio"
  ,"junho"
  ,"julho"
  ,"agosto"
  ,"setembro"
  ,"outubro"
  ,"novembro"
  ,"dezembro"),
  P = c(
    mean(alta_mes_janeiro$P),
    mean(alta_mes_fevereiro$P),
    mean(alta_mes_marco$P),
    mean(alta_mes_abril$P),
    mean(alta_mes_maio$P),
    mean(alta_mes_junho$P),
    mean(alta_mes_julho$P),
    mean(alta_mes_agosto$P),
    mean(alta_mes_setembro$P),
    mean(alta_mes_outubro$P),
    mean(alta_mes_novembro$P),
    mean(alta_mes_dezembro$P)
  ),
  ALTA = c(
    mean(alta_mes_janeiro$alta),
    mean(alta_mes_fevereiro$alta),
    mean(alta_mes_marco$alta),
    mean(alta_mes_abril$alta),
    mean(alta_mes_maio$alta),
    mean(alta_mes_junho$alta),
    mean(alta_mes_julho$alta),
    mean(alta_mes_agosto$alta),
    mean(alta_mes_setembro$alta),
    mean(alta_mes_outubro$alta),
    mean(alta_mes_novembro$alta),
    mean(alta_mes_dezembro$alta)
  ),
  UR = c(
    mean(alta_mes_janeiro$UR),
    mean(alta_mes_fevereiro$UR),
    mean(alta_mes_marco$UR),
    mean(alta_mes_abril$UR),
    mean(alta_mes_maio$UR),
    mean(alta_mes_junho$UR),
    mean(alta_mes_julho$UR),
    mean(alta_mes_agosto$UR),
    mean(alta_mes_setembro$UR),
    mean(alta_mes_outubro$UR),
    mean(alta_mes_novembro$UR),
    mean(alta_mes_dezembro$UR)
  ),
  TMAX = c(
    mean(alta_mes_janeiro$TMAX),
    mean(alta_mes_fevereiro$TMAX),
    mean(alta_mes_marco$TMAX),
    mean(alta_mes_abril$TMAX),
    mean(alta_mes_maio$TMAX),
    mean(alta_mes_junho$TMAX),
    mean(alta_mes_julho$TMAX),
    mean(alta_mes_agosto$TMAX),
    mean(alta_mes_setembro$TMAX),
    mean(alta_mes_outubro$TMAX),
    mean(alta_mes_novembro$TMAX),
    mean(alta_mes_dezembro$TMAX)
  ),
  TMIN = c(
    mean(alta_mes_janeiro$TMIN),
    mean(alta_mes_fevereiro$TMIN),
    mean(alta_mes_marco$TMIN),
    mean(alta_mes_abril$TMIN),
    mean(alta_mes_maio$TMIN),
    mean(alta_mes_junho$TMIN),
    mean(alta_mes_julho$TMIN),
    mean(alta_mes_agosto$TMIN),
    mean(alta_mes_setembro$TMIN),
    mean(alta_mes_outubro$TMIN),
    mean(alta_mes_novembro$TMIN),
    mean(alta_mes_dezembro$TMIN)
  ),
  NDR.1mm = c(
    mean(alta_mes_janeiro$NDR.1mm),
    mean(alta_mes_fevereiro$NDR.1mm),
    mean(alta_mes_marco$NDR.1mm),
    mean(alta_mes_abril$NDR.1mm),
    mean(alta_mes_maio$NDR.1mm),
    mean(alta_mes_junho$NDR.1mm),
    mean(alta_mes_julho$NDR.1mm),
    mean(alta_mes_agosto$NDR.1mm),
    mean(alta_mes_setembro$NDR.1mm),
    mean(alta_mes_outubro$NDR.1mm),
    mean(alta_mes_novembro$NDR.1mm),
    mean(alta_mes_dezembro$NDR.1mm)
  ),
  NDR.10mm = c(
    mean(alta_mes_janeiro$NDR.10mm),
    mean(alta_mes_fevereiro$NDR.10mm),
    mean(alta_mes_marco$NDR.10mm),
    mean(alta_mes_abril$NDR.10mm),
    mean(alta_mes_maio$NDR.10mm),
    mean(alta_mes_junho$NDR.10mm),
    mean(alta_mes_julho$NDR.10mm),
    mean(alta_mes_agosto$NDR.10mm),
    mean(alta_mes_setembro$NDR.10mm),
    mean(alta_mes_outubro$NDR.10mm),
    mean(alta_mes_novembro$NDR.10mm),
    mean(alta_mes_dezembro$NDR.10mm)
  ),
  NRH80 = c(
    mean(alta_mes_janeiro$NRH80),
    mean(alta_mes_fevereiro$NRH80),
    mean(alta_mes_marco$NRH80),
    mean(alta_mes_abril$NRH80),
    mean(alta_mes_maio$NRH80),
    mean(alta_mes_junho$NRH80),
    mean(alta_mes_julho$NRH80),
    mean(alta_mes_agosto$NRH80),
    mean(alta_mes_setembro$NRH80),
    mean(alta_mes_outubro$NRH80),
    mean(alta_mes_novembro$NRH80),
    mean(alta_mes_dezembro$NRH80)
  ),
  NRH90 = c(
    mean(alta_mes_janeiro$NRH90),
    mean(alta_mes_fevereiro$NRH90),
    mean(alta_mes_marco$NRH90),
    mean(alta_mes_abril$NRH90),
    mean(alta_mes_maio$NRH90),
    mean(alta_mes_junho$NRH90),
    mean(alta_mes_julho$NRH90),
    mean(alta_mes_agosto$NRH90),
    mean(alta_mes_setembro$NRH90),
    mean(alta_mes_outubro$NRH90),
    mean(alta_mes_novembro$NRH90),
    mean(alta_mes_dezembro$NRH90)
  ),
  stringsAsFactors = FALSE
)

media_alta_mes

sum_alta_mes <- data.frame(
  MES = c("janeiro"
  ,"fevereiro"
  ,"marco"
  ,"abril"
  ,"maio"
  ,"junho"
  ,"julho"
  ,"agosto"
  ,"setembro"
  ,"outubro"
  ,"novembro"
  ,"dezembro"),
  P = c(
    sum(alta_mes_janeiro$P),
    sum(alta_mes_fevereiro$P),
    sum(alta_mes_marco$P),
    sum(alta_mes_abril$P),
    sum(alta_mes_maio$P),
    sum(alta_mes_junho$P),
    sum(alta_mes_julho$P),
    sum(alta_mes_agosto$P),
    sum(alta_mes_setembro$P),
    sum(alta_mes_outubro$P),
    sum(alta_mes_novembro$P),
    sum(alta_mes_dezembro$P)
  ),
  ALTA = c(
    sum(alta_mes_janeiro$alta),
    sum(alta_mes_fevereiro$alta),
    sum(alta_mes_marco$alta),
    sum(alta_mes_abril$alta),
    sum(alta_mes_maio$alta),
    sum(alta_mes_junho$alta),
    sum(alta_mes_julho$alta),
    sum(alta_mes_agosto$alta),
    sum(alta_mes_setembro$alta),
    sum(alta_mes_outubro$alta),
    sum(alta_mes_novembro$alta),
    sum(alta_mes_dezembro$alta)
  ),
  UR = c(
    sum(alta_mes_janeiro$UR),
    sum(alta_mes_fevereiro$UR),
    sum(alta_mes_marco$UR),
    sum(alta_mes_abril$UR),
    sum(alta_mes_maio$UR),
    sum(alta_mes_junho$UR),
    sum(alta_mes_julho$UR),
    sum(alta_mes_agosto$UR),
    sum(alta_mes_setembro$UR),
    sum(alta_mes_outubro$UR),
    sum(alta_mes_novembro$UR),
    sum(alta_mes_dezembro$UR)
  ),
  TMAX = c(
    sum(alta_mes_janeiro$TMAX),
    sum(alta_mes_fevereiro$TMAX),
    sum(alta_mes_marco$TMAX),
    sum(alta_mes_abril$TMAX),
    sum(alta_mes_maio$TMAX),
    sum(alta_mes_junho$TMAX),
    sum(alta_mes_julho$TMAX),
    sum(alta_mes_agosto$TMAX),
    sum(alta_mes_setembro$TMAX),
    sum(alta_mes_outubro$TMAX),
    sum(alta_mes_novembro$TMAX),
    sum(alta_mes_dezembro$TMAX)
  ),
  TMIN = c(
    sum(alta_mes_janeiro$TMIN),
    sum(alta_mes_fevereiro$TMIN),
    sum(alta_mes_marco$TMIN),
    sum(alta_mes_abril$TMIN),
    sum(alta_mes_maio$TMIN),
    sum(alta_mes_junho$TMIN),
    sum(alta_mes_julho$TMIN),
    sum(alta_mes_agosto$TMIN),
    sum(alta_mes_setembro$TMIN),
    sum(alta_mes_outubro$TMIN),
    sum(alta_mes_novembro$TMIN),
    sum(alta_mes_dezembro$TMIN)
  ),
  NDR.1mm = c(
    sum(alta_mes_janeiro$NDR.1mm),
    sum(alta_mes_fevereiro$NDR.1mm),
    sum(alta_mes_marco$NDR.1mm),
    sum(alta_mes_abril$NDR.1mm),
    sum(alta_mes_maio$NDR.1mm),
    sum(alta_mes_junho$NDR.1mm),
    sum(alta_mes_julho$NDR.1mm),
    sum(alta_mes_agosto$NDR.1mm),
    sum(alta_mes_setembro$NDR.1mm),
    sum(alta_mes_outubro$NDR.1mm),
    sum(alta_mes_novembro$NDR.1mm),
    sum(alta_mes_dezembro$NDR.1mm)
  ),
  NDR.10mm = c(
    sum(alta_mes_janeiro$NDR.10mm),
    sum(alta_mes_fevereiro$NDR.10mm),
    sum(alta_mes_marco$NDR.10mm),
    sum(alta_mes_abril$NDR.10mm),
    sum(alta_mes_maio$NDR.10mm),
    sum(alta_mes_junho$NDR.10mm),
    sum(alta_mes_julho$NDR.10mm),
    sum(alta_mes_agosto$NDR.10mm),
    sum(alta_mes_setembro$NDR.10mm),
    sum(alta_mes_outubro$NDR.10mm),
    sum(alta_mes_novembro$NDR.10mm),
    sum(alta_mes_dezembro$NDR.10mm)
  ),
  NRH80 = c(
    sum(alta_mes_janeiro$NRH80),
    sum(alta_mes_fevereiro$NRH80),
    sum(alta_mes_marco$NRH80),
    sum(alta_mes_abril$NRH80),
    sum(alta_mes_maio$NRH80),
    sum(alta_mes_junho$NRH80),
    sum(alta_mes_julho$NRH80),
    sum(alta_mes_agosto$NRH80),
    sum(alta_mes_setembro$NRH80),
    sum(alta_mes_outubro$NRH80),
    sum(alta_mes_novembro$NRH80),
    sum(alta_mes_dezembro$NRH80)
  ),
  NRH90 = c(
    sum(alta_mes_janeiro$NRH90),
    sum(alta_mes_fevereiro$NRH90),
    sum(alta_mes_marco$NRH90),
    sum(alta_mes_abril$NRH90),
    sum(alta_mes_maio$NRH90),
    sum(alta_mes_junho$NRH90),
    sum(alta_mes_julho$NRH90),
    sum(alta_mes_agosto$NRH90),
    sum(alta_mes_setembro$NRH90),
    sum(alta_mes_outubro$NRH90),
    sum(alta_mes_novembro$NRH90),
    sum(alta_mes_dezembro$NRH90)
  ),
  stringsAsFactors = FALSE
)

sum_alta_mes

media_baixa_mes <- data.frame(
  MES = c("janeiro"
  ,"fevereiro"
  ,"marco"
  ,"abril"
  ,"maio"
  ,"junho"
  ,"julho"
  ,"agosto"
  ,"setembro"
  ,"outubro"
  ,"novembro"
  ,"dezembro"),
  P = c(
    mean(baixa_mes_janeiro$P),
    mean(baixa_mes_fevereiro$P),
    mean(baixa_mes_marco$P),
    mean(baixa_mes_abril$P),
    mean(baixa_mes_maio$P),
    mean(baixa_mes_junho$P),
    mean(baixa_mes_julho$P),
    mean(baixa_mes_agosto$P),
    mean(baixa_mes_setembro$P),
    mean(baixa_mes_outubro$P),
    mean(baixa_mes_novembro$P),
    mean(baixa_mes_dezembro$P)
  ),
  baixa = c(
    mean(baixa_mes_janeiro$baixa),
    mean(baixa_mes_fevereiro$baixa),
    mean(baixa_mes_marco$baixa),
    mean(baixa_mes_abril$baixa),
    mean(baixa_mes_maio$baixa),
    mean(baixa_mes_junho$baixa),
    mean(baixa_mes_julho$baixa),
    mean(baixa_mes_agosto$baixa),
    mean(baixa_mes_setembro$baixa),
    mean(baixa_mes_outubro$baixa),
    mean(baixa_mes_novembro$baixa),
    mean(baixa_mes_dezembro$baixa)
  ),
  UR = c(
    mean(baixa_mes_janeiro$UR),
    mean(baixa_mes_fevereiro$UR),
    mean(baixa_mes_marco$UR),
    mean(baixa_mes_abril$UR),
    mean(baixa_mes_maio$UR),
    mean(baixa_mes_junho$UR),
    mean(baixa_mes_julho$UR),
    mean(baixa_mes_agosto$UR),
    mean(baixa_mes_setembro$UR),
    mean(baixa_mes_outubro$UR),
    mean(baixa_mes_novembro$UR),
    mean(baixa_mes_dezembro$UR)
  ),
  TMAX = c(
    mean(baixa_mes_janeiro$TMAX),
    mean(baixa_mes_fevereiro$TMAX),
    mean(baixa_mes_marco$TMAX),
    mean(baixa_mes_abril$TMAX),
    mean(baixa_mes_maio$TMAX),
    mean(baixa_mes_junho$TMAX),
    mean(baixa_mes_julho$TMAX),
    mean(baixa_mes_agosto$TMAX),
    mean(baixa_mes_setembro$TMAX),
    mean(baixa_mes_outubro$TMAX),
    mean(baixa_mes_novembro$TMAX),
    mean(baixa_mes_dezembro$TMAX)
  ),
  TMIN = c(
    mean(baixa_mes_janeiro$TMIN),
    mean(baixa_mes_fevereiro$TMIN),
    mean(baixa_mes_marco$TMIN),
    mean(baixa_mes_abril$TMIN),
    mean(baixa_mes_maio$TMIN),
    mean(baixa_mes_junho$TMIN),
    mean(baixa_mes_julho$TMIN),
    mean(baixa_mes_agosto$TMIN),
    mean(baixa_mes_setembro$TMIN),
    mean(baixa_mes_outubro$TMIN),
    mean(baixa_mes_novembro$TMIN),
    mean(baixa_mes_dezembro$TMIN)
  ),
  NDR.1mm = c(
    mean(baixa_mes_janeiro$NDR.1mm),
    mean(baixa_mes_fevereiro$NDR.1mm),
    mean(baixa_mes_marco$NDR.1mm),
    mean(baixa_mes_abril$NDR.1mm),
    mean(baixa_mes_maio$NDR.1mm),
    mean(baixa_mes_junho$NDR.1mm),
    mean(baixa_mes_julho$NDR.1mm),
    mean(baixa_mes_agosto$NDR.1mm),
    mean(baixa_mes_setembro$NDR.1mm),
    mean(baixa_mes_outubro$NDR.1mm),
    mean(baixa_mes_novembro$NDR.1mm),
    mean(baixa_mes_dezembro$NDR.1mm)
  ),
  NDR.10mm = c(
    mean(baixa_mes_janeiro$NDR.10mm),
    mean(baixa_mes_fevereiro$NDR.10mm),
    mean(baixa_mes_marco$NDR.10mm),
    mean(baixa_mes_abril$NDR.10mm),
    mean(baixa_mes_maio$NDR.10mm),
    mean(baixa_mes_junho$NDR.10mm),
    mean(baixa_mes_julho$NDR.10mm),
    mean(baixa_mes_agosto$NDR.10mm),
    mean(baixa_mes_setembro$NDR.10mm),
    mean(baixa_mes_outubro$NDR.10mm),
    mean(baixa_mes_novembro$NDR.10mm),
    mean(baixa_mes_dezembro$NDR.10mm)
  ),
  NRH80 = c(
    mean(baixa_mes_janeiro$NRH80),
    mean(baixa_mes_fevereiro$NRH80),
    mean(baixa_mes_marco$NRH80),
    mean(baixa_mes_abril$NRH80),
    mean(baixa_mes_maio$NRH80),
    mean(baixa_mes_junho$NRH80),
    mean(baixa_mes_julho$NRH80),
    mean(baixa_mes_agosto$NRH80),
    mean(baixa_mes_setembro$NRH80),
    mean(baixa_mes_outubro$NRH80),
    mean(baixa_mes_novembro$NRH80),
    mean(baixa_mes_dezembro$NRH80)
  ),
  NRH90 = c(
    mean(baixa_mes_janeiro$NRH90),
    mean(baixa_mes_fevereiro$NRH90),
    mean(baixa_mes_marco$NRH90),
    mean(baixa_mes_abril$NRH90),
    mean(baixa_mes_maio$NRH90),
    mean(baixa_mes_junho$NRH90),
    mean(baixa_mes_julho$NRH90),
    mean(baixa_mes_agosto$NRH90),
    mean(baixa_mes_setembro$NRH90),
    mean(baixa_mes_outubro$NRH90),
    mean(baixa_mes_novembro$NRH90),
    mean(baixa_mes_dezembro$NRH90)
  ),
  stringsAsFactors = FALSE
)

media_baixa_mes

sum_baixa_mes <- data.frame(
  MES = c("janeiro"
  ,"fevereiro"
  ,"marco"
  ,"abril"
  ,"maio"
  ,"junho"
  ,"julho"
  ,"agosto"
  ,"setembro"
  ,"outubro"
  ,"novembro"
  ,"dezembro"),
  P = c(
    sum(baixa_mes_janeiro$P),
    sum(baixa_mes_fevereiro$P),
    sum(baixa_mes_marco$P),
    sum(baixa_mes_abril$P),
    sum(baixa_mes_maio$P),
    sum(baixa_mes_junho$P),
    sum(baixa_mes_julho$P),
    sum(baixa_mes_agosto$P),
    sum(baixa_mes_setembro$P),
    sum(baixa_mes_outubro$P),
    sum(baixa_mes_novembro$P),
    sum(baixa_mes_dezembro$P)
  ),
  baixa = c(
    sum(baixa_mes_janeiro$baixa),
    sum(baixa_mes_fevereiro$baixa),
    sum(baixa_mes_marco$baixa),
    sum(baixa_mes_abril$baixa),
    sum(baixa_mes_maio$baixa),
    sum(baixa_mes_junho$baixa),
    sum(baixa_mes_julho$baixa),
    sum(baixa_mes_agosto$baixa),
    sum(baixa_mes_setembro$baixa),
    sum(baixa_mes_outubro$baixa),
    sum(baixa_mes_novembro$baixa),
    sum(baixa_mes_dezembro$baixa)
  ),
  UR = c(
    sum(baixa_mes_janeiro$UR),
    sum(baixa_mes_fevereiro$UR),
    sum(baixa_mes_marco$UR),
    sum(baixa_mes_abril$UR),
    sum(baixa_mes_maio$UR),
    sum(baixa_mes_junho$UR),
    sum(baixa_mes_julho$UR),
    sum(baixa_mes_agosto$UR),
    sum(baixa_mes_setembro$UR),
    sum(baixa_mes_outubro$UR),
    sum(baixa_mes_novembro$UR),
    sum(baixa_mes_dezembro$UR)
  ),
  TMAX = c(
    sum(baixa_mes_janeiro$TMAX),
    sum(baixa_mes_fevereiro$TMAX),
    sum(baixa_mes_marco$TMAX),
    sum(baixa_mes_abril$TMAX),
    sum(baixa_mes_maio$TMAX),
    sum(baixa_mes_junho$TMAX),
    sum(baixa_mes_julho$TMAX),
    sum(baixa_mes_agosto$TMAX),
    sum(baixa_mes_setembro$TMAX),
    sum(baixa_mes_outubro$TMAX),
    sum(baixa_mes_novembro$TMAX),
    sum(baixa_mes_dezembro$TMAX)
  ),
  TMIN = c(
    sum(baixa_mes_janeiro$TMIN),
    sum(baixa_mes_fevereiro$TMIN),
    sum(baixa_mes_marco$TMIN),
    sum(baixa_mes_abril$TMIN),
    sum(baixa_mes_maio$TMIN),
    sum(baixa_mes_junho$TMIN),
    sum(baixa_mes_julho$TMIN),
    sum(baixa_mes_agosto$TMIN),
    sum(baixa_mes_setembro$TMIN),
    sum(baixa_mes_outubro$TMIN),
    sum(baixa_mes_novembro$TMIN),
    sum(baixa_mes_dezembro$TMIN)
  ),
  NDR.1mm = c(
    sum(baixa_mes_janeiro$NDR.1mm),
    sum(baixa_mes_fevereiro$NDR.1mm),
    sum(baixa_mes_marco$NDR.1mm),
    sum(baixa_mes_abril$NDR.1mm),
    sum(baixa_mes_maio$NDR.1mm),
    sum(baixa_mes_junho$NDR.1mm),
    sum(baixa_mes_julho$NDR.1mm),
    sum(baixa_mes_agosto$NDR.1mm),
    sum(baixa_mes_setembro$NDR.1mm),
    sum(baixa_mes_outubro$NDR.1mm),
    sum(baixa_mes_novembro$NDR.1mm),
    sum(baixa_mes_dezembro$NDR.1mm)
  ),
  NDR.10mm = c(
    sum(baixa_mes_janeiro$NDR.10mm),
    sum(baixa_mes_fevereiro$NDR.10mm),
    sum(baixa_mes_marco$NDR.10mm),
    sum(baixa_mes_abril$NDR.10mm),
    sum(baixa_mes_maio$NDR.10mm),
    sum(baixa_mes_junho$NDR.10mm),
    sum(baixa_mes_julho$NDR.10mm),
    sum(baixa_mes_agosto$NDR.10mm),
    sum(baixa_mes_setembro$NDR.10mm),
    sum(baixa_mes_outubro$NDR.10mm),
    sum(baixa_mes_novembro$NDR.10mm),
    sum(baixa_mes_dezembro$NDR.10mm)
  ),
  NRH80 = c(
    sum(baixa_mes_janeiro$NRH80),
    sum(baixa_mes_fevereiro$NRH80),
    sum(baixa_mes_marco$NRH80),
    sum(baixa_mes_abril$NRH80),
    sum(baixa_mes_maio$NRH80),
    sum(baixa_mes_junho$NRH80),
    sum(baixa_mes_julho$NRH80),
    sum(baixa_mes_agosto$NRH80),
    sum(baixa_mes_setembro$NRH80),
    sum(baixa_mes_outubro$NRH80),
    sum(baixa_mes_novembro$NRH80),
    sum(baixa_mes_dezembro$NRH80)
  ),
  NRH90 = c(
    sum(baixa_mes_janeiro$NRH90),
    sum(baixa_mes_fevereiro$NRH90),
    sum(baixa_mes_marco$NRH90),
    sum(baixa_mes_abril$NRH90),
    sum(baixa_mes_maio$NRH90),
    sum(baixa_mes_junho$NRH90),
    sum(baixa_mes_julho$NRH90),
    sum(baixa_mes_agosto$NRH90),
    sum(baixa_mes_setembro$NRH90),
    sum(baixa_mes_outubro$NRH90),
    sum(baixa_mes_novembro$NRH90),
    sum(baixa_mes_dezembro$NRH90)
  ),
  stringsAsFactors = FALSE
)

sum_baixa_mes
sum_alta_mes

media_baixa_mes
media_alta_mes


################### POR mes #########################
#um mes
df_cor = media_baixa_mes[c(1:11),c(2:10)]
lista = c(media_baixa_mes$baixa[c(2:12)])
df_cor$baixa = lista
df_cor = df_cor[,c(9,1:8)]

cor(df_cor)
teste = data.frame(
  media_baixa_mes$baixa,
  media_baixa_mes$P, 
  media_baixa_mes$UR, 
  media_baixa_mes$TMAX, 
  media_baixa_mes$TMIN, 
  media_baixa_mes$NDR.1mm, 
  media_baixa_mes$NDR.10mm, 
  media_baixa_mes$NRH80, 
  media_baixa_mes$NRH90
)

M <- cor(teste)
corrplot(M, method = "circle")

modelo = lm(baixa ~ TMAX, data = df_cor)

#dois meses
df_cor = media_baixa_mes[c(1:10),c(2:10)]
lista = c(media_baixa_mes$baixa[c(3:12)])
df_cor$baixa = lista
df_cor = df_cor[,c(9,1:8)]

cor(df_cor)
teste = data.frame(
  media_baixa_mes$baixa,
  media_baixa_mes$P, 
  media_baixa_mes$UR, 
  media_baixa_mes$TMAX, 
  media_baixa_mes$TMIN, 
  media_baixa_mes$NDR.1mm, 
  media_baixa_mes$NDR.10mm, 
  media_baixa_mes$NRH80, 
  media_baixa_mes$NRH90
)

M <- cor(teste)
corrplot(M, method = "circle")

modelo = lm(baixa ~  TMAX, data = df_cor)


################### POR mes #########################
#um mes
df_cor = sum_baixa_mes[c(1:11),c(2:10)]
lista = c(sum_baixa_mes$baixa[c(2:12)])
df_cor$baixa = lista
df_cor = df_cor[,c(9,1:8)]

cor(df_cor)
teste = data.frame(
  sum_baixa_mes$baixa,
  sum_baixa_mes$P, 
  sum_baixa_mes$UR, 
  sum_baixa_mes$TMAX, 
  sum_baixa_mes$TMIN, 
  sum_baixa_mes$NDR.1mm, 
  sum_baixa_mes$NDR.10mm, 
  sum_baixa_mes$NRH80, 
  sum_baixa_mes$NRH90
)

M <- cor(teste)
corrplot(M, method = "circle")


#dois meses
df_cor = sum_baixa_mes[c(1:10),c(2:10)]
lista = c(sum_baixa_mes$baixa[c(3:12)])
df_cor$baixa = lista
df_cor = df_cor[,c(9,1:8)]

cor(df_cor)
teste = data.frame(
  sum_baixa_mes$baixa,
  sum_baixa_mes$P, 
  sum_baixa_mes$UR, 
  sum_baixa_mes$TMAX, 
  sum_baixa_mes$TMIN, 
  sum_baixa_mes$NDR.1mm, 
  sum_baixa_mes$NDR.10mm, 
  sum_baixa_mes$NRH80, 
  sum_baixa_mes$NRH90
)

M <- cor(teste)
corrplot(M, method = "circle")

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

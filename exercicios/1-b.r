
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

df_total_alta <- read.csv(file = './src/alta-regiao-2-alta - Página1.csv')
df_total_baixa <- read.csv(file = './src/alta-regiao-2-baixa - Página1.csv')

# Utils
# =================================
# apply(dadosExemplo, MARGIN = 2, FUN = mean)
# colnames(df_total_alta)
# str(df_aux)
# warnings()

# ====================================================================================
# - b) Em quais meses a doença_café foi mais incidente? Houve variação por ano? 
# (ex: a doença geralmente ocorreu nos meses de dezembro, janeiro e fevereiro, 
# porém nos anos de X e Y ocorrem em maior incidência nos meses de abril e maio).

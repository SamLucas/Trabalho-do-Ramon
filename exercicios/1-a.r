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


Aano1 = 0
Aano2 = 0
Aano3 = 0
Aano4 = 0
Aano5 = 0
Aano6 = 0
Aano7 = 0
Aano8 = 0
Aano9 = 0
Aano10 = 0
Aano11 = 0
Aano12 = 0

Bano1 = 0
Bano2 = 0
Bano3 = 0
Bano4 = 0
Bano5 = 0
Bano6 = 0
Bano7 = 0
Bano8 = 0
Bano9 = 0
Bano10 = 0
Bano11 = 0
Bano12 = 0

for(i in 1:nrow(df_total_alta)){
  if(df_total_alta$ano[i]=='2007'){
    Aano1=Aano1+df_total_alta$alta[i]
  }else{
    if(df_total_alta$ano[i]=='2008'){
      Aano2=Aano2+df_total_alta$alta[i]
    }
    else{
      if(df_total_alta$ano[i]=='2009'){
        Aano3=Aano3+df_total_alta$alta[i]
      }
      else{
        if(df_total_alta$ano[i]=='2010'){
          Aano4=Aano4+df_total_alta$alta[i]
        }
        else{
          if(df_total_alta$ano[i]=='2011'){
            Aano5=Aano5+df_total_alta$alta[i]
          }
          else{
            if(df_total_alta$ano[i]=='2012'){
              Aano6=Aano6+df_total_alta$alta[i]
            }
            else{
              if(df_total_alta$ano[i]=='2013'){
                Aano7=Aano7+df_total_alta$alta[i]
              }
              else{
                if(df_total_alta$ano[i]=='2014'){
                  Aano8=Aano8+df_total_alta$alta[i]
                }
                else{
                  if(df_total_alta$ano[i]=='2015'){
                    Aano9=Aano9+df_total_alta$alta[i]
                  }
                  else{
                    if(df_total_alta$ano[i]=='2016'){
                      Aano10=Aano10+df_total_alta$alta[i]
                    }
                    else{
                      if(df_total_alta$ano[i]=='2017'){
                        Aano11=Aano11+df_total_alta$alta[i]
                      }
                      else{
                        if(df_total_alta$ano[i]=='2018'){
                          Aano12=Aano12+df_total_alta$alta[i]
                        }
                      }}}}}}}}}}}}  
                      
for(i in 1:nrow(df_total_baixa)){
  if(df_total_baixa$ano[i]=='2007'){
    Bano1=Bano1+df_total_baixa$alta[i]
  }else{
    if(df_total_baixa$ano[i]=='2008'){
      Bano2=Bano2+df_total_baixa$alta[i]
    }
    else{
      if(df_total_baixa$ano[i]=='2009'){
        Bano3=Bano3+df_total_baixa$alta[i]
      }
      else{
        if(df_total_baixa$ano[i]=='2010'){
          Bano4=Bano4+df_total_baixa$alta[i]
        }
        else{
          if(df_total_baixa$ano[i]=='2011'){
            Bano5=Bano5+df_total_baixa$alta[i]
          }
          else{
            if(df_total_baixa$ano[i]=='2012'){
              Bano6=Bano6+df_total_baixa$alta[i]
            }
            else{
              if(df_total_baixa$ano[i]=='2013'){
                Bano7=Bano7+df_total_baixa$alta[i]
              }
              else{
                if(df_total_baixa$ano[i]=='2014'){
                  Bano8=Bano8+df_total_baixa$alta[i]
                }
                else{
                  if(df_total_baixa$ano[i]=='2015'){
                    Bano9=Bano9+df_total_baixa$alta[i]
                  }
                  else{
                    if(df_total_baixa$ano[i]=='2016'){
                      Bano10=Bano10+df_total_baixa$alta[i]
                    }
                    else{
                      if(df_total_baixa$ano[i]=='2017'){
                        Bano11=Bano11+df_total_baixa$alta[i]
                      }
                      else{
                        if(df_total_baixa$ano[i]=='2018'){
                          Bano12=Bano12+df_total_baixa$alta[i]
                        }
                      }}}}}}}}}}}}  

anos = c('2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018')
Atotal_doencas = c(Aano1, Aano2, Aano3, Aano4, Aano5, Aano6, Aano7, Aano8, Aano9, Aano10, Aano11, Aano12)
Btotal_doencas = c(Bano1, Bano2, Bano3, Bano4, Bano5, Bano6, Bano7, Bano8, Bano9, Bano10, Bano11, Bano12)

barplot(Atotal_doencas, col =  heat.colors(12), legend.text = anos, args.legend = list(x = "topright"))
barplot(Btotal_doencas, col =  heat.colors(12), legend.text = anos, args.legend = list(x = "topright"))
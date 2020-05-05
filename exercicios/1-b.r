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

# Utils
# =================================
# apply(dadosExemplo, MARGIN = 2, FUN = mean)
# colnames(df_total_alta)

# ====================================================================
# 1) Análise de frequências (1,0 ponto):
# -  b) Em quais meses a doença_café foi mais incidente? Houve variação por ano? (ex: a doença geralmente ocorreu nos meses de dezembro, janeiro e fevereiro, porém nos anos de X e Y ocorrem em maior incidência nos meses de abril e maio).

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

mes_a<-c(0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:nrow(df_total_alta)){
    if(df_total_alta$mes[i]=='janeiro'){
      mes_a[1]=mes_a[1]+df_total_alta$alta[i]
    }
    else{
      if(df_total_alta$mes[i]=='fevereiro'){
        mes_a[2]=mes_a[2]+df_total_alta$alta[i]
      }
      else{
        if(df_total_alta$mes[i]=='marco'){
          mes_a[3]=mes_a[3]+df_total_alta$alta[i]
        }
        else{
          if(df_total_alta$mes[i]=='abril'){
            mes_a[4]=mes_a[4]+df_total_alta$alta[i]
          }
          else{
            if(df_total_alta$mes[i]=='maio'){
              mes_a[5]=mes_a[5]+df_total_alta$alta[i]
            }
            else{
              if(df_total_alta$mes[i]=='junho'){
                mes_a[6]=mes_a[6]+df_total_alta$alta[i]
              }
              else{
                if(df_total_alta$mes[i]=='julho'){
                  mes_a[7]=mes_a[7]+df_total_alta$alta[i]
                }
                else{
                  if(df_total_alta$mes[i]=='agosto'){
                    mes_a[8]=mes_a[8]+df_total_alta$alta[i]
                  }
                  else{
                    if(df_total_alta$mes[i]=='setembro'){
                      mes_a[9]=mes_a[9]+df_total_alta$alta[i]
                    }
                    else{
                      if(df_total_alta$mes[i]=='outubro'){
                        mes_a[10]=mes_a[10]+df_total_alta$alta[i]
                      }
                      else{
                        if(df_total_alta$mes[i]=='novembro'){
                          mes_a[11]=mes_a[11]+df_total_alta$alta[i]
                        }
                        else{
                          if(df_total_alta$mes[i]=='dezembro'){
                            mes_a[12]=mes_a[12]+df_total_alta$alta[i]
                          }
                        }}}}}}}}}}}}

mes_b<-c(0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:nrow(df_total_baixa)){
    if(df_total_baixa$mes[i]=='janeiro'){
      mes_b[1]=mes_b[1]+df_total_baixa$baixa[i]
    }
    else{
      if(df_total_baixa$mes[i]=='fevereiro'){
        mes_b[2]=mes_b[2]+df_total_baixa$baixa[i]
      }
      else{
        if(df_total_baixa$mes[i]=='marco'){
          mes_b[3]=mes_b[3]+df_total_baixa$baixa[i]
        }
        else{
          if(df_total_baixa$mes[i]=='abril'){
            mes_b[4]=mes_b[4]+df_total_baixa$baixa[i]
          }
          else{
            if(df_total_baixa$mes[i]=='maio'){
              mes_b[5]=mes_b[5]+df_total_baixa$baixa[i]
            }
            else{
              if(df_total_baixa$mes[i]=='junho'){
                mes_b[6]=mes_b[6]+df_total_baixa$baixa[i]
              }
              else{
                if(df_total_baixa$mes[i]=='julho'){
                  mes_b[7]=mes_b[7]+df_total_baixa$baixa[i]
                }
                else{
                  if(df_total_baixa$mes[i]=='agosto'){
                    mes_b[8]=mes_b[8]+df_total_baixa$baixa[i]
                  }
                  else{
                    if(df_total_baixa$mes[i]=='setembro'){
                      mes_b[9]=mes_b[9]+df_total_baixa$baixa[i]
                    }
                    else{
                      if(df_total_baixa$mes[i]=='outubro'){
                        mes_b[10]=mes_b[10]+df_total_baixa$baixa[i]
                      }
                      else{
                        if(df_total_baixa$mes[i]=='novembro'){
                          mes_b[11]=mes_b[11]+df_total_baixa$baixa[i]
                        }
                        else{
                          if(df_total_baixa$mes[i]=='dezembro'){
                            mes_b[12]=mes_b[12]+df_total_baixa$baixa[i]
                          }
                        }}}}}}}}}}}}

meses = c('jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez')
barplot(mes_a, col =  heat.colors(12), legend.text = meses, args.legend = list(x = "topright"))
barplot(mes_b, col =  heat.colors(12), legend.text = meses, args.legend = list(x = "topright"))

# boxplot(
#   df_total_alta[which(df_total_alta$mes == 'janeiro'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'fevereiro'),]$alta, 
#   df_total_alta[which(df_total_alta$mes == 'marco'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'abril'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'maio'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'junho'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'julho'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'agosto'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'setembro'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'outubro'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'novembro'),]$alta,
#   df_total_alta[which(df_total_alta$mes == 'dezembro'),]$alta
# )

# boxplot(
#   df_total_baixa[which(df_total_baixa$mes == 'janeiro'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'fevereiro'),]$baixa, 
#   df_total_baixa[which(df_total_baixa$mes == 'marco'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'abril'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'maio'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'junho'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'julho'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'agosto'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'setembro'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'outubro'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'novembro'),]$baixa,
#   df_total_baixa[which(df_total_baixa$mes == 'dezembro'),]$baixa
# )
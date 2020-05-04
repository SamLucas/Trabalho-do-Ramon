#sele��o de arquivo via menus
library(corrplot)
library(plyr) 
library(data.table)
library(dplyr)


caminho<-file.choose()    
df_total = fread(file = caminho)

#Resumo do dataset
summary(df_total)
str(df_total)

#P = Volume de chuva
#UR = Umidade relativa
#TMAX = Temperatura m�xima
#TMIN = Temperatura m�nima
#NDR>1mm = N�mero de dias com chuva entre 1 e 10mm
#NDR>10mm = N�mero de dias com chuva acima de 10mm
#NRH80 = N�mero de dias com umidade relativa entre 80 e 90%
#NRH90 = N�mero de dias com umidade relativa acima de 90%


# Criando uma coluna para mes_ano
df_total$mes_ano = paste(df_total$mes, df_total$ano, sep  = '_')

#convertendo c_alta para categorias de n�veis da doen�a
#Abaixo de 20.1 � n�vel baixo
#Entre 20 e 60 � n�vel m�dio
#Acima de 60 � n�vel alto
converte = function(x){
  if(x < 20.1){
    return("baixo") 
  }else if(x > 60){
    return("alto") 
  }else{
    return("medio") 
  }
}
for(i in 1:nrow(df_total)){
  df_total$c_alta_categoria[i] = converte(df_total$c_alta[i])
}


df_total = df_total[,c(28,1,2,29,3:27)]

#Utilizando a m�dia
df_total$P = apply(df_total[,6:8], 1, mean)
df_total$UR = apply(df_total[,9:11], 1, mean)
df_total$TMAX = apply(df_total[,12:14], 1, mean)
df_total$TMIN = apply(df_total[,15:17], 1, mean)
df_total$NDR.1mm = apply(df_total[,18:20], 1, mean)
df_total$NDR.10mm = apply(df_total[,21:23], 1, mean)
df_total$NRH80 = apply(df_total[,24:26], 1, mean)
df_total$NRH90 = apply(df_total[,27:29], 1, mean)

df_total_s = df_total[,c(1:5,30:37)]
######################## Parte 1 #############################
ano1 = 0
ano2 = 0
ano3 = 0
ano4 = 0
ano5 = 0
ano6 = 0
ano7 = 0
ano8 = 0
ano9 = 0
ano10 = 0
ano11 = 0
ano12 = 0

for(i in 1:nrow(df_total)){
  if(df_total$ano[i]=='2007'){
    ano1=ano1+df_total$c_alta[i]
  }else{
    if(df_total$ano[i]=='2008'){
      ano2=ano2+df_total$c_alta[i]
    }
    else{
      if(df_total$ano[i]=='2009'){
        ano3=ano3+df_total$c_alta[i]
      }
      else{
        if(df_total$ano[i]=='2010'){
          ano4=ano4+df_total$c_alta[i]
        }
        else{
          if(df_total$ano[i]=='2011'){
            ano5=ano5+df_total$c_alta[i]
          }
          else{
            if(df_total$ano[i]=='2012'){
              ano6=ano6+df_total$c_alta[i]
            }
            else{
              if(df_total$ano[i]=='2013'){
                ano7=ano7+df_total$c_alta[i]
              }
              else{
                if(df_total$ano[i]=='2014'){
                  ano8=ano8+df_total$c_alta[i]
                }
                else{
                  if(df_total$ano[i]=='2015'){
                    ano9=ano9+df_total$c_alta[i]
                  }
                  else{
                    if(df_total$ano[i]=='2016'){
                      ano10=ano10+df_total$c_alta[i]
                    }
                    else{
                      if(df_total$ano[i]=='2017'){
                        ano11=ano11+df_total$c_alta[i]
                      }
                      else{
                        if(df_total$ano[i]=='2018'){
                          ano12=ano12+df_total$c_alta[i]
                        }
                      }}}}}}}}}}}
}  
total_doencas = c(ano1, ano2, ano3, ano4, ano5, ano6, ano7, ano8, ano9, ano10, ano11, ano12)
anos = c('2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018')
barplot(total_doencas, col =  heat.colors(12), legend.text = anos, args.legend = list(x = "topright"))


boxplot(df_total[which(df_total$ano == '2007'),]$c_alta,
        df_total[which(df_total$ano == '2008'),]$c_alta, 
        df_total[which(df_total$ano == '2009'),]$c_alta,
        df_total[which(df_total$ano == '2010'),]$c_alta,
        df_total[which(df_total$ano == '2011'),]$c_alta,
        df_total[which(df_total$ano == '2012'),]$c_alta,
        df_total[which(df_total$ano == '2013'),]$c_alta,
        df_total[which(df_total$ano == '2014'),]$c_alta,
        df_total[which(df_total$ano == '2015'),]$c_alta,
        df_total[which(df_total$ano == '2016'),]$c_alta,
        df_total[which(df_total$ano == '2017'),]$c_alta,
        df_total[which(df_total$ano == '2018'),]$c_alta
)



mes_a<-c(0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:nrow(df_total)){
  #if(df_total$ano[i]=='2018'){
    if(df_total$mes[i]=='janeiro'){
      mes_a[1]=mes_a[1]+df_total$c_alta[i]
    }
    else{
      if(df_total$mes[i]=='fevereiro'){
        mes_a[2]=mes_a[2]+df_total$c_alta[i]
      }
      else{
        if(df_total$mes[i]=='mar�o'){
          mes_a[3]=mes_a[3]+df_total$c_alta[i]
        }
        else{
          if(df_total$mes[i]=='abril'){
            mes_a[4]=mes_a[4]+df_total$c_alta[i]
          }
          else{
            if(df_total$mes[i]=='maio'){
              mes_a[5]=mes_a[5]+df_total$c_alta[i]
            }
            else{
              if(df_total$mes[i]=='junho'){
                mes_a[6]=mes_a[6]+df_total$c_alta[i]
              }
              else{
                if(df_total$mes[i]=='julho'){
                  mes_a[7]=mes_a[7]+df_total$c_alta[i]
                }
                else{
                  if(df_total$mes[i]=='agosto'){
                    mes_a[8]=mes_a[8]+df_total$c_alta[i]
                  }
                  else{
                    if(df_total$mes[i]=='setembro'){
                      mes_a[9]=mes_a[9]+df_total$c_alta[i]
                    }
                    else{
                      if(df_total$mes[i]=='outubro'){
                        mes_a[10]=mes_a[10]+df_total$c_alta[i]
                      }
                      else{
                        if(df_total$mes[i]=='novembro'){
                          mes_a[11]=mes_a[11]+df_total$c_alta[i]
                        }
                        else{
                          if(df_total$mes[i]=='dezembro'){
                            mes_a[12]=mes_a[12]+df_total$c_alta[i]
                          }
                        }}}}}}}}}}}
 # }
}
meses = c('jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez')
barplot(mes_a, col =  heat.colors(12), legend.text = meses, args.legend = list(x = "topright"))


######################## Parte 2 ###########################
#Correla��o

#0 mes
df_cor = df_total_s[c(1:127),c(6:13)]
lista = c(df_total_s$c_alta[c(1:127)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]

#um mes
df_cor = df_total_s[c(1:126),c(6:13)]
lista = c(df_total_s$c_alta[c(2:127)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]

#dois meses
df_cor = df_total_s[c(1:125),c(6:13)]
lista = c(df_total_s$c_alta[c(3:127)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]

#tr�s meses
df_cor = df_total_s[c(1:124),c(6:13)]
lista = c(df_total_s$c_alta[c(4:127)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]

#por dias
df_total = as.data.frame(df_total)
df_cor_aux=df_total[c(1),c(5,6,9,12,15,18,21,24,27)]
df_cor_aux = as.data.frame(df_cor_aux)
aux=1
aux2=1
for(i in 1:nrow(df_total_s)){
  df_cor_aux[c(aux),]=df_total[c(aux2),c(5,6,9,12,15,18,21,24,27)]
  aux=aux+1
  df_cor_aux[c(aux),]=df_total[c(aux2),c(5,7,10,13,16,19,22,25,28)]
  aux=aux+1
  df_cor_aux[c(aux),]=df_total[c(aux2),c(5,8,11,14,17,20,23,26,29)]
  aux=aux+1
  aux2=aux2+1
}

#10 dias
df_cor = df_cor_aux[c(1:380),c(2:9)]
lista = c(df_cor_aux$c_alta[c(2:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")

#20 dias
df_cor = df_cor_aux[c(1:379),c(2:9)]
lista = c(df_cor_aux$c_alta[c(3:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")

#40 dias
df_cor = df_cor_aux[c(1:377),c(2:9)]
lista = c(df_cor_aux$c_alta[c(5:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")

#50 dias
df_cor = df_cor_aux[c(1:376),c(2:9)]
lista = c(df_cor_aux$c_alta[c(6:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")

#70 dias
df_cor = df_cor_aux[c(1:374),c(2:9)]
lista = c(df_cor_aux$c_alta[c(8:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")

#80 dias
df_cor = df_cor_aux[c(1:373),c(2:9)]
lista = c(df_cor_aux$c_alta[c(9:381)])
df_cor$c_alta = lista
df_cor = df_cor[,c(9,1:8)]
names(df_cor)<-c("c_alta","P","UR","TMAX","TMIN","NDR.1mm","NDR.10mm","NRH.80","NRH.90")


#Correla��o
cor(df_cor)
teste = data.frame(df_total$c_alta, df_total$P, df_total$UR, df_total$TMAX, df_total$TMIN, df_total$NDR.1mm, df_total$NDR.10mm, df_total$NRH80, df_total$NRH90)

M <- cor(teste)
corrplot(M, method = "circle")

library(psych)
pairs.panels(teste)

#Regress�o linear
#0 meses - TMAX
modelo = lm(c_alta ~  TMAX, data = df_cor)
summary(lm)
# Res�duos do modelo (diferen�a entre os dados reais e a linha de melhor ajuste)




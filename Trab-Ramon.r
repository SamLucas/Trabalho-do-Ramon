# Nesse trabalho vocês deverão formar 4 grupos, de até 4 alunos. Cada grupo deverá selecionar uma das regiões conforme dataset disponibilizado em anexo:

# Acesse o documento compartilhado a seguir para definição dos grupos
# Após selecionar a Região no arquivo do DataSet, o grupo deverá realizar sistematicamente as seguintes análises:
# Cada dataset de cada região na verdade se tornará 2 datasets "c_alta" e "c_baixa". Faça as análises a seguir considerando e comparando os datasets "c_alta" e "c_baixa".

df_total_alta = read.csv(file = 'alta-regiao-2-alta - Página1.csv')
df_total_baixa = read.csv(file = 'alta-regiao-2-baixa - Página1.csv')

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

df_total_sA = df_total[,c(1:5,30:37)]
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


# 1) Análise de frequências (1,0 ponto):
# - a) Em quais anos a doença_café foi mais incidente? comente.
# - b) Em quais meses a doença_café foi mais incidente? Houve variação por ano? (ex: a doença geralmente ocorreu nos meses de dezembro, janeiro e fevereiro, porém nos anos de X e Y ocorrem em maior incidência nos meses de abril e maio).

# 2) Análise de correlação e regressão linear (3,0 pontos)
# Nas análises de correlação e regressão linear, você deve:
# - a) Gerar correlações entre a variável doença_café e as demais variáveis.
# - b) Gerar modelos de regressão linear simples e múltipla para previsão da doença do café.

# 3) Nas análises de correlação e regressão linear, você deve considerar os 10 cenários a seguir:
# a) Fazer testes considerando a média mensal e a soma das variáveis independentes considerando 1 e 2 meses de antecedência.
# b) Fazer testes considerando antecedência de:
# - 20 dias de antecedência (ex: doenca_cafe de abril, com os dados de março (11_20))
# - 30 dias de antecedência (ex: doenca_cafe de abril, com os dados de março (1_10))
# - 40 dias de antecedência (ex: doenca_cafe de abril, com os dados de fevereiro (21_30))
# - 50 dias de antecedência (ex: doenca_cafe de abril, com os dados de fevereiro (11_20))
# - 60 dias de antecedência (ex: doenca_cafe de abril, com os dados de fevereiro (1_10))
# - 70 dias de antecedência (ex: doenca_cafe de abril, com os dados de janeiro (21_30)

# 4) Perguntas a serem respondidas sobre a análise de correlação e regressão linear:
# - a) Para cada um dos 10 cenários, qual ou quais variáveis independentes apresentaram melhor correlação com a variável dependente (doença_café)? Após analisar os 10 cenários qual ou quais cenários apresentaram os melhores resultados? Quais variáveis se destacaram na análise de correlação?
# - b) Para cada um dos 10 cenários, qual o melhor modelo de regressão linear simples e/ou múltipla para prever a variável dependente (doença_café)? Após analisar os 10 cenários qual ou quais cenários apresentaram os melhores resultados? Qual ou quais os melhores modelos de regressão linear simples ou múltipla foram identificados?

# O trabalho deverá ser preparado em slides e enviado no questionário da atividade até o dia 06/05/2020.
# Valor da atividade: 4,0 pontos
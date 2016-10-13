############## Ler e manipular dados vetoriais - shapefile

###### Objetivo: Abrir dados de pontos shapefile e manipular dados de atributos.
###### Explorar dados dos atributos por meio de Histogramas

##### Os exemplos abaixos tem como dados de entrada os focos ativos satélite MODIS, dos anos de 2000 a 2014

#####Pacotes necessários

library(sp)
library(maptools)  

######## Abrir arquivo shapefile e examinar tabela de atributos

pts <- readShapePoints("Focos_MG2.shp") ###### Lêr arquivo shapefile de pontos
names(pts) ##### Nome dos atributos
summary(pts) ###### Estatísticas dos atributos

####### Histograma de frequencia de um atributo ligado ao arquivo shapefile
###### ex: CONFIDENCE

hist(pts$CONFIDENCE,col = rgb(0.8, 0.2, 0.2, 0.5),breaks="scott",
     main="Histograma",
     xlab="Nivel de Confiança",ylab="Frequência") ##### Histograma de um dos atributos
box()

##### Criar uma coluna de atributos e indexar a tabela do shapefile

Df=data.frame(pts) ####### Criar data frame de Focos

######## Extrair string da data para separar mes, dia e ano.
Df=cbind(Df, MES = as.numeric(substring(Df$ACQ_DATE, 6, 7)))   ####### Criar coluna com dados dos MESES
Df=cbind(Df, DIA = as.numeric(substring(Df$ACQ_DATE, 9, 10))) ###### Criar coluna com dados dos dias
Df=cbind(Df, ANO = as.numeric(substring(Df$ACQ_DATE, 1, 4))) ###### Criar coluna com dados dos dias

names(Df)

hist(Df$ANO,breaks="scott") ###### Histograma da Confiabilidade
histinfo = hist(Df$ANO,breaks="scott")

##### Histograma de Frequencia Mensal de Focos Ativos

histinfo = hist(Df$MES,axes = FALSE,col = rgb(0.8, 0.2, 0.2, 0.5),xlab="Mes", ylab="Frequencia",main="Distribuição Mensal de Focos Ativos")
hrange=range(0,max(histinfo$counts))
axis(side=1, at = 1: 12, lab =c("Jan","Fev", "Mar", "Abr","Mai","Jun","Jul","Ago","Set", "Out", "Nov","Dez"),cex.axis=0.8) 
axis(side=2,at = 0: 8 * 10000,cex.axis=0.8)

######### axis
####side - lado do eixo. 1 é abaixo, 2 a esquerda, 3 a direita e 4 acima.
#### at - 
######cex.axis - Ajusta o tamanho da Fonte do Gráfico
##### dist.axis  

################# Gráficos pacote ggplot2  

library(ggplot2)

qplot(Df$MES,
      geom="histogram",
      binwidth = 1,  
      main = "Distribuição Mensal de Focos Ativos", 
      xlab = "Mes",  
      fill=I("blue"), 
      col=I("purple"), 
      alpha=I(.2),
)

######## Gráfico Com valores de frequencia em legenda

bp2=ggplot(data=Df, aes(Df$MES)) + 
  geom_histogram(binwidth=1,
                 col="black", 
                 aes(fill=..count..))+
scale_fill_gradient("Focos Ativos")+ 
scale_x_discrete(limits=c("Jan","Fev", "Mar", "Abr","Mai","Jun","Jul","Ago","Set", "Out", "Nov", "Dez"))+
  xlab("")+
  ylab("Frequencia")
#stat_bin(geom="text", aes(label=..count.., vjust=-0.5))


##### Mais opçoes de histogramas ggplot

bp=ggplot(data=Df, aes(Df$MES)) + 
  geom_histogram(breaks=seq(0, 12, by =1), 
                 col="black", 
                 aes(fill=..count..))+
  scale_fill_gradient("Focos Ativos", low = "blue", high = "orange")

######### Informações adicionais 

bp + scale_x_discrete(labels=c("Jan","Fev", "Mar", "Abr","Mai","Jun","Jul","Ago","Set", "Out", "Nov", "Dez"))+
  xlab("")+
  ylab("Frequencia") 

################

labels <-paste0(c(seq(2000,2014, by = 1)))
bp2=ggplot(data=Df, aes(ANO)) + 
  geom_histogram( binwidth=1,
                 col="black", 
                 aes(fill=..count..))+
  scale_fill_gradient("Focos Ativos")+
  xlab(" ")+
  ylab("Frequencia") #

  scale_x_discrete(label = c(labels))#+
  stat_bin(geom="text", aes(label=..count.., vjust=-0.2))
 
￼ggplot(data=Df, aesANO) ) + geom_density(adjust = 0.25)

labels <-paste0(c(seq(2000,2014, by = 1)))
                  +
                scale_x_discrete(labels = labels)

############

bp2 + scale_x_discrete(limits=labels)

#ES
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2/shapes/es_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_es=readShapePoly("32MU500G.shp")

#Visualizacao dos dados que estao dentro do shape
mapa_es@data

#Plot do shape
plot(mapa_es)

##MG
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2/shapes/mg_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_mg=readShapePoly("31MU500G.shp")

#Visualizacao dos dados que estao dentro do shape
mapa_mg@data

#Plot do shape
plot(mapa_mg)

##RJ
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2/shapes/rj_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_rj=readShapePoly("33MU500G.shp")

#Visualizacao dos dados que estao dentro do shape
mapa_rj@data

#Plot do shape
plot(mapa_rj)

##SP
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2/shapes/sp_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_sp=readShapePoly("35MU500G.shp")

#Visualizacao dos dados que estao dentro do shape
mapa_sp@data

#Plot do shape
plot(mapa_sp)

##Mapa Geral

mapa_geral_al=rbind(mapa_es,mapa_rj,mapa_sp,mapa_mg)
plot(mapa_geral_al)
mapa_geral_al@data

##Indicador Composto
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2")
require(readxl)
composto2000=read_xlsx("T2000.xlsx",sheet="Planilha2")
composto2010=read_xlsx("T2010.xlsx",sheet="Planilha3")

n=nrow(mapa_geral_al@data)
n

banco2000=merge(mapa_geral_al@data,composto2000,by.x="CODIGO",by.y="codigo")
banco2010=merge(mapa_geral_al@data,composto2010,by.x="CODIGO",by.y="codigo")

mapa_geral_al@data$I2000<-banco2000$I2000
mapa_geral_al@data$I2010<-banco2010$I2010
spplot(mapa_geral_al, "I2000")
spplot(mapa_geral_al, "I2010")

#### Intervalo do mapa pelo quantil do Indicador ####
q_1=quantile(mapa_geral_al$I2000,0.25,na.rm = TRUE)
q_2=quantile(mapa_geral_al$I2000,0.5,na.rm=TRUE)
q_3=quantile(mapa_geral_al$I2000,0.75,na.rm=TRUE)
intervalo_quartis <- c(0,q_1,q_2,q_3,100)
q_1=quantile(mapa_geral_al$I2010,0.25,na.rm = TRUE)
q_2=quantile(mapa_geral_al$I2010,0.5,na.rm=TRUE)
q_3=quantile(mapa_geral_al$I2010,0.75,na.rm=TRUE)
intervalo_quartis <- c(0,q_1,q_2,q_3,100)

#Plotando o mapa com legenda do Indicador
cortes_1t<-cut(mapa_geral_al$I2010,intervalo_quartis,include.lowest=TRUE)
niveis_1t<-levels(cortes_1t)
rw.colors=colorRampPalette(c("pink","red"))
cores_1t<-rw.colors(length(niveis_1t))
levels(cortes_1t)<-cores_1t

plot(mapa_geral_al,lwd=.1,axes=FALSE,col=as.character(cortes_1t))
legend("left",niveis_1t,fill=cores_1t,bty="n",cex=0.5)


###Aula Estatistica em Epidemiologia
#### An??lise de Indicadores no Espa??o



#Para a instalacao dos pacotes
install.packages("sp")
install.packages("maptools")
install.packages("classInt")
install.packages("RColorBrewer")
install.packages("PBSmapping")
install.packages("spdep")
install.packages("tripack")
install.packages("rgdal")
install.packages("ggmap")

#Para ativar os pacotes
require(sp)
require(maptools)
require(classInt)
require(RColorBrewer)
require(PBSmapping)
require(spdep)
require(tripack)
require(rgdal)
require(ggmap)
require(readxl)

library(foreign)
#Leitura dos dados
dados=read.csv2("indicador.csv",header=T)
names(dados)

readShapePoly
#Leitura do shapefile (biblioteca maptools)
mapa=readShapePoly("32MU500G.shp")
mapa=readOGR(dsn ="31MU500G.shp")
mapa
#ou
#mapa=readOGR(".", "BRMIE250GC_SIR") # biblioteca rgdal

#dataProjected <- readOGR(dsn ="BRMIE250GC_SIR.shp", layer="BRMIE250GC_SIR")


#Visualizacao dos dados que estao dentro do shape
mapa@data

#Plot do shape
plot(mapa)



n <- nrow(mapa@data)

banco=merge(mapa@data,dados, by.x="CD_GEOCMI", by.y="Codigo")
mapa@data$Indicador<-banco$Indicador  ## incluindo a variavel de pesquisa no mapa

#dataProjected@data$Indicador<-banco$Indicador.x 
#head(dataProjected@data)
#dim(dataProjected@data)


spplot(mapa, "Indicador")
# Com 50 tons de cinza
spplot(mapa, "Indicador",  col.regions=grey.colors(50))



#### Intervalo do mapa pelo quantil do Indicador ####

q_1=quantile(mapa$Indicador,0.25)
q_2=quantile(mapa$Indicador,0.5)
q_3=quantile(mapa$Indicador,0.75)
intervalo_quartis <- c(0,q_1,q_2,q_3,1000) #Para mudar os intervalos


#Plotando o mapa com legenda do Indicador
cortes_1t<-cut(mapa$Indicador,intervalo_quartis,include.lowest=TRUE)
niveis_1t<-levels(cortes_1t)
rw.colors=colorRampPalette(c("pink","red"))
cores_1t<-rw.colors(length(niveis_1t))
levels(cortes_1t)<-cores_1t

plot(mapa,lwd=.1,axes=FALSE,col=as.character(cortes_1t),border="transparent")
legend("left",niveis_1t,fill=cores_1t,bty="n",cex=0.5)



############# ggplot


# add to data a new column termed "id" composed of the rownames of data
dataProjected@data$id <- rownames(dataProjected@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(dataProjected, region = "id")


# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")


head(watershedDF)


##################### Mapa com titulo ######################

ggWatershed <- ggplot(watershedDF, aes(long, lat, group=group, fill = Indicador)) +
  geom_polygon(aes(fill = Indicador)) +
  coord_equal() +labs(x = "Longitude", 
                      y = "Latitude")
#, 
#title = "Mapa Geografico do Brasil por microrregiao", 
#subtitle = "Indicador de Sa??de e Ambiente para Doencas de Veiculacao Hidrica", 
#caption = "Fonte: Elabora????o do pr??prio autor com a utiliza????o do software R")

scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
  print(ggWatershed)


ggWatershed <- ggplot(watershedDF, aes(long, lat, group=group, fill = Indicador)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
  geom_polygon(aes(fill = Indicador)) +
  coord_equal() +labs(x = "Longitude", 
                      y = "Latitude")
print(ggWatershed)

https://stackoverflow.com/questions/23714052/ggplot-mapping-us-counties-problems-with-visualization-shapes-in-r


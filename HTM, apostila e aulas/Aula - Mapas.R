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

library(foreign)
#Leitura dos dados
dados=read.csv2("indicador.csv",header=T)
names(dados)

#Leitura do shapefile (biblioteca maptools)
mapa=readShapePoly("BRMIE250GC_SIR.shp")
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


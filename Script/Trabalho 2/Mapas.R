require(foreign)
require(readr)
require(rgdal)
#AL
#Leitura do shapefile (biblioteca maptools)
mapa_al=readOGR("Script/Trabalho 2/Shapes/al_municipios/27MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##BA
#Leitura do shapefile (biblioteca maptools)
mapa_ba=readOGR("Script/Trabalho 2/Shapes/ba_municipios/29MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##CE
#Leitura do shapefile (biblioteca maptools)
mapa_ce=readOGR("Script/Trabalho 2/Shapes/ce_municipios/23MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##MA
#Leitura do shapefile (biblioteca maptools)
mapa_ma=readOGR("Script/Trabalho 2/Shapes/ma_municipios/21MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PB
#Leitura do shapefile (biblioteca maptools)
mapa_pb=readOGR("Script/Trabalho 2/Shapes/pb_municipios/25MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PE
#Leitura do shapefile (biblioteca maptools)
mapa_pe=readOGR("Script/Trabalho 2/Shapes/pe_municipios/26MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PI
#Leitura do shapefile (biblioteca maptools)
mapa_pi=readOGR("Script/Trabalho 2/Shapes/pi_municipios/22MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##RN
#Leitura do shapefile (biblioteca maptools)
mapa_rn=readOGR("Script/Trabalho 2/Shapes/rn_municipios/24MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##SE
#Leitura do shapefile (biblioteca maptools)
mapa_se=readOGR("Script/Trabalho 2/Shapes/se_municipios/28MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##Mapa Geral

mapa_geral_al=rbind(mapa_pi,mapa_rn,mapa_pe,mapa_pb,mapa_ma,mapa_ce,mapa_ba,mapa_al,mapa_se)
plot(mapa_geral_al)
mapa_geral_al@data

##Indicador Composto

composto2000=read.csv2("Banco/Trabalho 2/Indicadores 2000.csv",header=T)
composto2010=read.csv2("Banco/Trabalho 2/Indicadores 2010.csv",header=T)

n=nrow(mapa_geral_al@data)
n

banco2000=merge(mapa_geral_al@data,composto2000,by.x="CD_GEOCODM",by.y="Código")
banco2010=merge(mapa_geral_al@data,composto2010,by.x="CD_GEOCODM",by.y="Código")

mapa_geral_al@data$I2000<-banco2000$Indicador_final
mapa_geral_al@data$I2010<-banco2010$Indicador_final
spplot(mapa_geral_al, "I2000")
spplot(mapa_geral_al, "I2010")

#### Intervalo do mapa pelo quantil do Indicador ####
q_1=quantile(mapa_geral_al$I2000,0.25,na.rm = TRUE)
q_2=quantile(mapa_geral_al$I2000,0.5,na.rm=TRUE)
q_3=quantile(mapa_geral_al$I2000,0.75,na.rm=TRUE)
intervalo_quartis <- c(0,q_1,q_2,q_3,100)

#Plotando o mapa com legenda do Indicador
cortes_1t<-cut(mapa_geral_al$I2000,intervalo_quartis,include.lowest=TRUE)
niveis_1t<-levels(cortes_1t)
rw.colors=colorRampPalette(c("#7475f2","#171854"))
cores_1t<-rw.colors(length(niveis_1t))
levels(cortes_1t)<-cores_1t

plot(mapa_geral_al,lwd=.1,axes=FALSE,col=as.character(cortes_1t), main="Indicador final 2000")
legend("bottomright",niveis_1t,fill=cores_1t,bty="n",cex=0.7, inset = c(.2,0.28))

help("legend")


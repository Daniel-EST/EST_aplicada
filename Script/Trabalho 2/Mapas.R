#AL
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/al_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_al=readOGR("27MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##BA
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/ba_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_ba=readOGR("29MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##CE
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/ce_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_ce=readOGR("23MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##MA
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/ma_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_ma=readOGR("21MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PB
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/pb_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_pb=readOGR("25MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PE
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/pe_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_pe=readOGR("26MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##PI
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/pi_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_pi=readOGR("22MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##RN
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/rn_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_rn=readOGR("24MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##SE
setwd("/Users/gabrielmizuno/Desktop/EST_aplicada/Script/Trabalho 2/Shapes/se_municipios")

#Leitura do shapefile (biblioteca maptools)
mapa_rn=readOGR("28MUE250GC_SIR.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)

##Mapa Geral

mapa_geral_al=rbind(mapa_pi,mapa_rn,mapa_pe,mapa_pb,mapa_ma,mapa_ce,mapa_ba,mapa_al)
plot(mapa_geral_al)
mapa_geral_al@data

##Indicador Composto
setwd("/home/116054012/Área de trabalho/Documentos/Estatística Aplicada/Trabalho 2")
require(readxl)
composto2000=read_xlsx("T2000.xlsx",sheet="Planilha2")
composto2010=read_xlsx("T2010.xlsx",sheet="Planilha3")

n=nrow(mapa_geral_al@data)
n

banco_2000=merge(mapa_geral_al@data,composto2000,by.x="CODIGO",by.y="codigo")
banco_2010=merge(mapa_geral_al@data,composto2010,by.x="CODIGO",by.y="codigo")

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

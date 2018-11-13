setwd("/home/115054012/Área de trabalho")
library(readr)
library(readxl)
library(dplyr)
base<-read_xlsx("base.xlsx")
base$estado[base$estado=="Santana Catarina"]="Santa Catarina"
base1<-base%>%select(c(sexo:dinheiro, requisitos))
base2<-base%>%select(c(jogabilidade,tempo_jogo,grafico,multiplayer,multiplayer_c, divertimento))

saveRDS(base1, "base1.rds")
saveRDS(base2,"base2.rds")

base$sexo<-base$sexo%>%as.factor()
base$escolaridade<-base$escolaridade%>%as.factor()
base$civil<-base$civil%>%as.factor()
base$estado<-base$estado%>%as.factor()
base$evento<-base$evento%>%as.factor()
base$horas_semana<-base$horas_semana%>%as.factor()
base$p_venda<-base$p_venda%>%as.factor()
base$preco_jogos<-base$preco_jogos%>%as.factor()
base$midia<-base$midia%>%as.factor()
base$requisitos<-base$requisitos%>%as.factor()
base$dinheiro<-base$dinheiro%>%as.factor()

summary(base1)


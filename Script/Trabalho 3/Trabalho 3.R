require(ggplot2); require(dplyr); library(DescTools); require(likert); require(magrittr)
require(rgdal); library(tidyr);require(scales)

#SO PARA O MIZUNO NÃO RODA ISSO, SE VC RODAR ISSO O PROBLEMA É SEU!!!!!!!!!
#Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")

base <- readRDS("Banco/Trabalho 3/base1.rds")
Likert <- readRDS("Banco/Trabalho 3/Likert.rds")

#Box plot por idadde, temos que usar ANOVA
summary(base$idade)
sd(base$idade)
ggplot(base, aes(x = sexo, y = idade)) + geom_boxplot(fill="#00bfc4") + ylab("Idade") + xlab("Gênero")+theme_light()+theme(legend.position = "none")


summary(base$estado)
length(base$estado)

#graficos de pizza
#basta trocar a variave em tb
tb<-round(prop.table(table(base$civil)),2)
dt<-as.data.frame(tb)

pizza<-ggplot(dt, aes(x="",y=Freq, fill=Var1))+
  geom_bar(width = 1, stat="identity")+
  coord_polar("y")

pizza+scale_fill_discrete(name="Estado Civil")+theme_void()+ 
  geom_text(aes(y = c(0.83,0.3, 0.5,0.1),
                label = percent(Freq,accuracy=1)), data = dt, size=4.6)+ylab("")+xlab("")

a<-as.data.frame(round(prop.table(table(base$escolaridade)),3))

#proporções
round(prop.table(table(base$dinheiro))*100,1)
5.2+2.6+1.3+0.7+0.3+0.3

table(base$horas_semana)


#Verificando Homocedacidade
LeveneTest(base$idade, base$sexo, center = "mean")

comparacao = aov(idade ~ sexo, data = base)
summary(comparacao)
PostHocTest(comparacao, method = "bonferroni")

mean(base$idade[base$sexo=="Masculino"])

#Estados
#Acho melhor passa um summary e fazer um tabela
summary(base$estado)

base2 <- group_by(base, estado) %>% summarise(n = length(estado))
base2$estado <-  as.character(base2$estado)
base2$estado[base2$estado=="Espírito Santo"] <- "Espirito Santo"
base2$estado <- toupper(base2$estado)
mapa_br <- readOGR("Banco/Trabalho 3/UFEBRASIL.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)
mapa_br@data$NM_ESTADO <-  as.factor(mapa_br@data$NM_ESTADO)
mapa_br@data <- inner_join(mapa_br@data,base2, by = c("NM_ESTADO" = "estado"))

spplot(mapa_br, "n",col.regions=grey.colors(500))

q_1=quantile(mapa_br@data$n,0.25,na.rm = TRUE)
q_2=quantile(mapa_br@data$n,0.5,na.rm=TRUE)
q_3=quantile(mapa_br@data$n,0.75,na.rm=TRUE)
q_4=quantile(mapa_br@data$n,0.85,na.rm=TRUE)
q_5=quantile(mapa_br@data$n,0.98, na.rm=TRUE)
q_6=quantile(mapa_br@data$n,1, na.rm=TRUE)
intervalo_quartis <- c(0,q_1,q_2,q_3,q_4,q_5,q_6)

#Mapa falta melhorar
cortes_1t<-cut(mapa_br@data$n,intervalo_quartis,include.lowest=TRUE)
niveis_1t<-levels(cortes_1t)
rw.colors=colorRampPalette(c("#b7e6e8","#016d70"))
cores_1t<-rw.colors(length(niveis_1t))
levels(cortes_1t)<-cores_1t

plot(mapa_br,lwd=.1,axes=FALSE,col=as.character(cortes_1t))
legend("bottomright",niveis_1t,fill=cores_1t,bty="n",cex=0.7, inset = c(0.2,0.2))


#Escala Likert
Likert <- Likert %>% 
  select(-c(idade.cat))


Likert$jogabilidade = ordered(Likert$jogabilidade, levels = c('Baixa Importância',
                                        'Média Importância',
                                        'Alta Importância'))

Likert$tempo_jogo = ordered(Likert$tempo_jogo, levels = c('Baixa Importância',
                                               'Média Importância',
                                               'Alta Importância'))
Likert$grafico = ordered(Likert$grafico, levels = c('Baixa Importância',
                                                                'Média Importância',
                                                                'Alta Importância'))
Likert$multiplayer = ordered(Likert$multiplayer, levels = c('Baixa Importância',
                                                  'Média Importância',
                                                  'Alta Importância'))

Likert$multiplayer_c = ordered(Likert$multiplayer_c, levels = c('Baixa Importância',
                                                  'Média Importância',
                                                  'Alta Importância'))

Likert$divertimento = ordered(Likert$divertimento, levels = c('Baixa Importância',
                                                  'Média Importância',
                                                  'Alta Importância'))

#aqui q eu troquei o nome das variaveis. se quiser ver o grafico como tava antes, não roda esse
names(Likert) <- c("Jogabilidade", "Tempo de Jogo", "Gráfico", "Multiplayer", "Multiplayer Competitivo","Divertimento") 
Likert %<>% as.data.frame() 
tabela_likert <- likert(Likert) 
c <- c("#f2766d","#00bfc4","#7cae00") 
plot(tabela_likert,color=c)+ ylab("Porcentagem")

#produtos por dinheiro

base4<-select(base, c(Skins:Outros_p))
base4<-apply(base4, 2, function(x)ifelse(x=="Sim",1,0 )) %>% as.data.frame()
base$quant_pro<- apply(base4, 1, sum)     

round(mean(base$quant_pro),2)

base$quant_pro<-base$quant_pro%>%as.factor()
base$quant_pro

base %>%
  ggplot(aes(x=dinheiro)) + 
  geom_bar(aes(fill=quant_pro), position = "fill")+ylab("Frequência")+scale_fill_discrete(name="Produtos")+xlab("Já gastou mais dinheiro do que deveria?")

summary(base$quant_pro)

#dinheiro por genero que joga
base$quant_jogos<-base$quant_jogos%>%as.factor()

base %>%
  ggplot(aes(x=dinheiro)) + 
  geom_bar(aes(fill=quant_jogos), position = "dodge")+ylab("Frequência")+scale_fill_discrete(name="Gênero de Jogo")+xlab("Já gastou mais dinheiro do que deveria?")

#dinheiro por plataforma
base$quant_plant<-base$quant_plant%>%as.factor()

base %>%
  ggplot(aes(x=dinheiro)) + 
  geom_bar(aes(fill=quant_plant), position = "dodge")+ylab("Frequência")+scale_fill_discrete(name="Plataforma")+xlab("Já gastou mais dinheiro do que deveria?")

#Tipo de plataforma por genero jogo 
base2 <- select(base,c(FPS:OUTROS))
base2 <- apply(base2, 2, function(x)ifelse(x=="Sim",1,0 )) %>% as.data.frame()
base$quant_jogos <- apply(base2, 1, sum)

mean(base$quant_jogos)

base3 <- select(base,c(PC:Outros_pla))
base3 <- apply(base3, 2, function(x)ifelse(x=="Sim",1,0 )) %>% as.data.frame()
base$quant_plant <- apply(base3, 1, sum)

quant <- cbind(base$quant_plant,base$quant_jogos)%>% as.data.frame()
colnames(quant) <- c("Plataforma","Genero")

table(quant)
quant$Plataforma=as.factor(quant$Plataforma)
quant$Genero=as.factor(quant$Genero)

#grafico

quant %>%
  ggplot(aes(x=Plataforma)) + 
  geom_bar(aes(fill=Genero), position = "fill")+ylab("Frequência")

quant %>%
  ggplot(aes(x=Plataforma)) + 
  geom_bar(aes(fill=Genero), position = "dodge")+ylab("Frequência")+scale_fill_discrete(name="Gênero")

#ESCOLARIDADE POR DINHEIRO GASTO

base%>%ggplot(aes(x=escolaridade))+geom_bar(aes(fill=dinheiro), position="fill")

tab1<-table(base$escolaridade,base$dinheiro)
tab1

chisq.test(tab1, correct=FALSE)

#PREÇO JOGOs POR PRE VENDA
base%>%ggplot(aes(x=p_venda))+geom_bar(aes(fill=preco_jogos), position="fill")

#DINHEIRO POR PREÇO JOGOS

base%>%ggplot(aes(x=dinheiro))+geom_bar(aes(fill=requisitos), position="fill")

#Genero

barplot(prop.table(table(base$sexo)),ylim = c(0, 1),main="Genero")
summary(base$sexo)

ggplot(base, aes(x=sexo))+
  geom_bar(fill=c("#f2766d", "#00bfc4", "#7cae00"))+
  theme_light()+
  theme(legend.position = "none")+
  xlab("Gênero")+ylab("Frequência")


ggplot(base, aes(x=escolaridade))+
  geom_bar(fill=c("#f2766d","#de8c00", "#b79f00","#7cae00", "#00ba38", "#00c08b", "#00bfc4"))+
  theme_light()+
  theme(legend.position = "none")+
  xlab("Escolaridade")+ylab("Frequência")
 
#Cat idade por tempo

tab1_hora_cat <- table(base$sexo, base$horas_semana) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base$horas_semana=base$horas_semana%>%ordered(levels=c("menos de 2 horas","de 2 à 4 horas", "de 4 à 6 horas", "mais de 6 horas"))%>%as.factor()

base %>%
  ggplot(aes(x=sexo)) + 
  geom_bar(aes(fill=horas_semana), position = "fill") + 
  ylab("Frequência")+theme_light()+xlab("Gênero")+
  scale_fill_discrete(name="Horas Semanais", labels=c("Menos de 2 horas","De 2 à 4 horas", "De 4 à 6 horas", "Mais de 6 horas"))
  
#Cat idade por preço

tab1_hora_cat <- table(base$idade.cat, base$preco_jogos) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=idade.cat)) + 
  geom_bar(aes(fill=preco_jogos), position = "fill") + 
  ylab("Frequência Relativa")+theme_light()+xlab("Categoria de Idade")+
  scale_fill_discrete(name="Preço Justo")

#Cart idade por + dinheiro

tab1_hora_cat <- table(base$idade.cat, base$dinheiro) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=idade.cat)) + 
  geom_bar(aes(fill=dinheiro), position = "fill") + 
  ylab("Frequência Relativa")+theme_light()+xlab("Categoria de Idade")+
  scale_fill_discrete(name="Dinheiro")

#Midias por preço

tab1_hora_cat <- table(base$midia, base$preco_jogos) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

round((prop.table(table(base$escolaridade)))*100,1)
10.21+89.45+0.33

base %>%
  ggplot(aes(x=midia)) + 
  geom_bar(aes(fill=preco_jogos), position = "fill") + 
  ylab("Frequência Relativa")+theme_light()+xlab("Tipo de Midia")+
  scale_fill_discrete(name="Preço Justo")

#Requisitos minimos

prop <- ifelse(base$requisitos=="Sim",1,0); mean(prop)
prop.test(sum(prop), length(prop), alternative = "less", p = 0.5, correct = FALSE)

ggplot(base, aes(x=requisitos))+
  geom_bar(fill="#00bfc4")+
  theme_light()+
  theme(legend.position = "none")+
  xlab("Já deixou de jogar por causa dos requisitos?")+ylab("Frequência")

ggplot(base, aes(x=requisitos))+
  geom_(fill="#00bfc4")+
  theme_light()+
  theme(legend.position = "none")+
  xlab("Já deixou de jogar por causa dos requisitos?")+ylab("Frequência")

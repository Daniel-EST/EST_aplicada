require(ggplot2); require(dplyr); library(DescTools); require(likert); require(magrittr)
require(rgdal); library(tidyr)
#SO PARA O MIZUNO NÃO RODA ISSO, SE VC RODAR ISSO O PROBLEMA É SEU!!!!!!!!!
#Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")

base <- readRDS("Banco/Trabalho 3/base1.rds")
Likert <- readRDS("Banco/Trabalho 3/Likert.rds")

#Box plot por idadde, temos que usar ANOVA
summary(base$idade)
sd(base$idade)
ggplot(base, aes(x = sexo, y = idade)) + geom_boxplot(color = "red",fill="blue") + ylab("Idade") + xlab("Genero")

#Verificando Homocedacidade
LeveneTest(base$idade, base$sexo, center = "mean")

comparacao = aov(idade ~ sexo, data = base)
summary(comparacao)
PostHocTest(comparacao, method = "bonferroni")

#Estados
#Acho melhor passa um summary e fazer um tabela
summary(base$estado)

base2 <- group_by(base, estado) %>% summarise(n = length(estado))
base2$estado <-  as.factor(base2$estado)
base2$estado[base2$estado=="Espírito Santo"] <- "Espirito Santo"
mapa_br <- readOGR("Banco/Trabalho 3/UFEBRASIL.shp",encoding ="UTF-8",use_iconv=TRUE, verbose=FALSE)
mapa_br@data$NM_ESTADO <-  as.factor(mapa_br@data$NM_ESTADO)
mapa_br@data <- inner_join(mapa_br@data,base2, by = c("NM_ESTADO" = "estado"))
spplot(mapa_br, "n")

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

Likert %<>%  as.data.frame()
tabela_likert <- likert(Likert)
plot(tabela_likert)

#Tipo de plataforma por genero jogo 

base2 <- select(base,c(FPS:OUTROS,PC:Outros_pla))

base2 <- apply(base2, 2, function(x)ifelse(x=="Sim",1,0 )) %>% as.data.frame()

PC <- filter(base2,PC==1) %>% select(c(FPS:OUTROS))
PC <- apply(PC, 2, sum) %>% as.data.frame()
colnames(PC) <- "PC"
PC <- t(PC)

Xbox <- filter(base2,Xbox==1) %>% select(c(FPS:OUTROS))
Xbox <- apply(Xbox, 2, sum) %>% as.data.frame()
colnames(Xbox) <- "Xbox"
Xbox <- t(Xbox)

PS <- filter(base2,PS==1) %>% select(c(FPS:OUTROS))
PS <- apply(PS, 2, sum) %>% as.data.frame()
colnames(PS) <- "PS"
PS <- t(PS)

Wii <- filter(base2,Wii==1) %>% select(c(FPS:OUTROS))
Wii <- apply(Wii, 2, sum) %>% as.data.frame()
colnames(Wii) <- "Wii"
Wii <- t(Wii)

Celular <- filter(base2,Celular==1) %>% select(c(FPS:OUTROS))
Celular <- apply(Celular, 2, sum) %>% as.data.frame()
colnames(Celular) <- "Celular"
Celular <- t(Celular)

Outros_pla <- filter(base2,Outros_pla==1) %>% select(c(FPS:OUTROS))
Outros_pla <- apply(Outros_pla, 2, sum) %>% as.data.frame()
colnames(Outros_pla) <- "Outros_pla"
Outros_pla <- t(Outros_pla)

juntos<- rbind(PC,Xbox,PS,Wii,Celular,Outros_pla)%>% as.data.frame()
Total <- apply(juntos,1,sum)
juntos <- cbind(juntos,Total)

#Genero

barplot(prop.table(table(base$sexo)),ylim = c(0, 1),main="Genero")
summary(base$sexo)
 
#Cat idade por tempo

tab1_hora_cat <- table(base$idade.cat, base$horas_semana) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=idade.cat)) + 
  geom_bar(aes(fill=horas_semana), position = "fill") + 
  labs(title="Horas jogadas por categoria das idades", 
       y="Frequencia relativa") 

#Cat idade por preço

tab1_hora_cat <- table(base$idade.cat, base$preco_jogos) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=preco_jogos)) + 
  geom_bar(aes(fill=idade.cat), position = "fill") + 
  labs(title="Preco dos jogos por categoria de idades", 
       y="Frequencia relativa") 

#Cart idade por + dinheiro

tab1_hora_cat <- table(base$idade.cat, base$dinheiro) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=idade.cat)) + 
  geom_bar(aes(fill=dinheiro), position = "fill") + 
  labs(title="Preco dos jogos por categoria das idades", 
       y="Frequencia relativa") 

#Midias por preço

tab1_hora_cat <- table(base$midia, base$preco_jogos) %>% as.matrix(); tab1_hora_cat
chisq.test(tab1_hora_cat,correct=FALSE)

base %>%
  ggplot(aes(x=midia)) + 
  geom_bar(aes(fill=preco_jogos), position = "fill") + 
  labs(title="Horas jogadas por categoria das idades", 
       y="Frequencia relativa") 

#Requisitos minimos

prop <- ifelse(base$requisitos=="Sim",1,0); mean(prop)
prop.test(sum(prop), length(prop), alternative = "less", p = 0.5, correct = FALSE)

barplot(prop.table(table(base$requisitos)),ylim = c(0, 1),main="Deixou de jogar por cuasa dos requisitos?")

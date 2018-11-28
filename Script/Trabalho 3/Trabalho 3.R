require(ggplot2); require(dplyr); library(DescTools); require(likert); require(magrittr)
require(rgdal)
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

#Genero

barplot(prop.table(table(base$sexo)),ylim = c(0, 1),main="Genero")

#Requisitos minimos

prop <- ifelse(base$requisitos=="Sim",1,0); mean(prop)
prop.test(sum(prop), length(prop), alternative = "less", p = 0.5, correct = FALSE)

barplot(prop.table(table(base$requisitos)),ylim = c(0, 1),main="Deixou de jogar por cuasa dos requisitos?")

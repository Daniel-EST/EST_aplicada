require(ggplot2); require(dplyr); library(DescTools); require(likert)
#SO PARA O MIZUNO NÃO RODA ISSO, SE VC RODAR ISSO O PROBLEMA É SEU!!!!!!!!!
Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")
base <- readRDS("Banco/Trabalho 3/base1.rds")
Likert <- readRDS("Banco/Trabalho 3/likert.rds")

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
ggplot(base, aes(x=estado)) + geom_bar() 
summary(base$estado)

require(readxl) ; require(dplyr) ; require(readr) ; require(ggplot2) ; library(sjPlot)

# Diret?rio ###
getwd()
dir()

# Tratar base ###
# Leitura ###
dadosM <- read_xls("Perfil Homens e Mulheres - Bancos.xls", sheet = "homens")
dadosF <- read_xls("Perfil Homens e Mulheres - Bancos.xls", sheet = "mulheres")

dadosM$sexo <- 1
dadosF$sexo <- 0

# Usar fatores ###
dados$sexo <- factor(dados$sexo, labels = c("f","m"))
dados$estciv <- factor(dados$estciv, labels = c("casado","solteiro","viuvo", "outros"))
dados$possuibem <- factor(dados$possuibem, labels = c("s","n"))
dados$serasa <- factor(dados$serasa, labels = c("s","n"))
dados$regproc <- factor(dados$regproc, labels = c("capital","nao-capital"))
dados$banco <- factor(dados$banco, labels = c("bb", "itau", "bradesco", "caixa", "santander", "outros"))
dados$bankline <- factor(dados$bankline, labels = c("s"))
dados$cxeletro <- factor(dados$cxeletro, labels = c("s","n"))
dados$layout <- factor(dados$layout, labels = c("confuso", "indiferente", "bom"))
dados$cheque <- factor(dados$cheque, labels = c("s","n"))
dados$poupanca <- factor(dados$poupanca, labels = c("s","n"))
dados$cartcred <- factor(dados$cartcred, labels = c("s","n"))
dados$fundinvest <- factor(dados$fundinvest, labels = c("s","n"))
dados$telefone <- factor(dados$telefone, labels = c("s","n"))
dados$gerente <- factor(dados$gerente, labels = c("s","n"))
dados$limite <- factor(dados$limite, labels = c("s","n"))
dados$satisflimite <- factor(dados$satisflimite, labels = c("s","n"))
dados$maisdeumban <- factor(dados$maisdeumban, labels = c("s","n"))
dados$previdencia <- factor(dados$previdencia, labels = c("s","n"))
dados$seguro <- factor(dados$seguro, labels = c("s","n"))

#Tempo de uso do bankline por semana 

dados$bankantes %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                             ,sd=sd(.))
dados$bankdepois %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                            ,sd=sd(.))

#Nao tem distribuicao NORMAL (DEU RUIM)

#Layout 

table(dados$layout)
t <- matrix(c(96,53,41))
chisq.test(t,p=c(1/3,1/3,1/3),correct = FALSE)
ggplot(dados,aes(dados$layout))+geom_bar()
 
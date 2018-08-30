require(readxl) ; require(dplyr) ; require(readr) ; require(ggplot2) ; library(sjPlot) ; library(tidyr)

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

#Fazendo grafico

bank = dados %>% gather(key = Curso, value = Tempo, bankantes, bankdepois) %>% select(Curso, Tempo)
bank$Curso <- factor(bank$Curso,levels = c('bankantes',"bankdepois"),
                     labels = c("Antes","Depois"))
ggplot(dados,aes(sexo,bankantes))+geom_boxplot(fill='red')
ggplot(dados,aes(sexo,bankdepois))+geom_boxplot(fill='red')

ggplot(bank,aes(Curso,Tempo))+geom_boxplot(fill='red')

#Tempo de uso do bankline por semana 

dados$bankantes %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                             ,sd=sd(.))
dados$bankdepois %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                            ,sd=sd(.))

var.test(dados$bankantes,dados$bankdepois,ratio=1,alternative = "two.sided")

t.test(dados$bankantes,dados$bankdepois,var.equal = FALSE,paired = TRUE,alternative = "less")

#Assuminindo normalidade assitotica

#Layout 

t <- matrix(c(96,53,41))
chisq.test(t,p=c(1/3,1/3,1/3),correct = FALSE)

#Fazendo grafico

#Peguei a matriz transposta pq nao estava dando certo
lay <- t(table(dados$layout));lay

# Escolhendo cores
lay_cor <- c("#54b0f7","#ff91f7","red")

# Calculando as propor??es
#Troquei para 1 pq nao estava dando certo
lay <- apply(lay, 1, function(x){x/sum(x,na.rm=T)}) ; lay
lay <- t(lay)

par(mar = c(5.1, 4.1, 4.1, 2.1))

barplot(lay, col=lay_cor, 
        border="white", 
        xlab="Categorias", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Entendimento do layout")
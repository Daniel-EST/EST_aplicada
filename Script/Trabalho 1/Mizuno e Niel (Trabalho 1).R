# Pacotes ####
require(readxl) ; require(dplyr) ; require(readr) ; require(ggplot2) ; library(sjPlot)

# Diret?rio ###
getwd()
dir()

# Tratar base ###
# Leitura ###
dadosM <- read_xls("bd/Trabalho I/Perfil Homens e Mulheres - Bancos.xls", sheet = "homens")
dadosF <- read_xls("bd/Trabalho I/Perfil Homens e Mulheres - Bancos.xls", sheet = "mulheres")

dadosM$sexo <- 1
dadosF$sexo <- 0

dados <- rbind(dadosF,dadosM) ; dados
dados <- dados %>% 
  select(sexo, everything()) ; dados 
rm(list = c("dadosM","dadosF"))

# Usar fatores ###
dados$sexo <- factor(dados$sexo, labels = c("f","m"))
dados$estciv <- factor(dados$estciv, labels = c("casado","solteiro","viuvo", "outros"))
dados$possuibem <- factor(dados$possuibem, labels = c("n","s"))
dados$serasa <- factor(dados$serasa, labels = c("n","s"))
dados$regproc <- factor(dados$regproc, labels = c("capital","nao-capital"))
dados$banco <- factor(dados$banco, labels = c("bb", "itau", "bradesco", "caixa", "santander", "outros"))
dados$bankline <- factor(dados$bankline, labels = c("s"))
dados$cxeletro <- factor(dados$cxeletro, labels = c("n","s"))
dados$layout <- factor(dados$layout, labels = c("confuso", "indiferente", "bom"))
dados$cheque <- factor(dados$cheque, labels = c("n","s"))
dados$poupanca <- factor(dados$poupanca, labels = c("n","s"))
dados$cartcred <- factor(dados$cartcred, labels = c("n","s"))
dados$fundinvest <- factor(dados$fundinvest, labels = c("n","s"))
dados$telefone <- factor(dados$telefone, labels = c("n","s"))
dados$gerente <- factor(dados$gerente, labels = c("n","s"))
dados$limite <- factor(dados$limite, labels = c("n","s"))
dados$satisflimite <- factor(dados$satisflimite, labels = c("não","sim"))
dados$maisdeumban <- factor(dados$maisdeumban, labels = c("n","s"))
dados$previdencia <- factor(dados$previdencia, labels = c("n","s"))
dados$seguro <- factor(dados$seguro, labels = c("n","s"))

ggplot(dados,aes(x=banco))+geom_bar(aes(fill=banco))+ylab("Frequência")+xlab("Bancos")+
  theme(legend.position = "none")
summary(dados)

#PERGUNTA 1

#PERGUNTA 1#####

#Fazer descritivas da idade por sexo#####

#TH para normalidade de dados$sexo=="f"
dados$idade[dados$sexo=="f"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                         ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
par(mfrow = c(1,2))
qqnorm(dados$idade[dados$sexo=="f"], main = "Q-Q Plot \nIdade feminino", sub = "Verificando normalidade", col = "red")
qqline(dados$idade[dados$sexo=="f"])

#TH para normalidade de dados$sexo=="m"
dados$idade[dados$sexo=="m"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                         ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$sexo=="m"], main = "Q-Q Plot \nIdade masculino", sub = "Verificando normalidade", col = "red")
qqline(dados$idade[dados$sexo=="m"])

#TH para igualdade de variancias
var.test(dados$idade[dados$sexo=="f"],dados$idade[dados$sexo=="m"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$idade[dados$sexo=="f"],dados$idade[dados$sexo=="m"],var.equal=FALSE,
       alternative ="two.sided")

#Fazendo grafico

#Idade
hist(dados$idade,xlab="Idade",ylab="Porcentagem",freq=FALSE,
     main="Histograma da idade")
curve(dnorm(x,mean(dados$idade),sd(dados$idade)),add=T)

# Boxplot da idade por sexo
ggplot(dados,aes(x=sexo,y=idade))+geom_boxplot()


#Idade do sexo feminino

par(mfrow=c(1,2))
hist(dados$idade[dados$sexo=="f"],xlab="Idade",ylab="Porcentagem"
     ,freq=FALSE,main="Histograma da idade do sexo feminino",ylim = c(0, 0.15))
curve(dnorm(x,mean(dados$idade[dados$sexo=="f"]),sd(dados$idade[dados$sexo=="m"])),add=T)

#Idade do sexo masculino
hist(dados$idade[dados$sexo=="m"],xlab="Idade",ylab="Porcentagem",freq=FALSE,
     main="Histograma da idade do sexo masculino",ylim = c(0, 0.15))
curve(dnorm(x,mean(dados$idade[dados$sexo=="m"]),sd(dados$idade[dados$sexo=="m"])),add=T)
par(mfrow = c(1,1))

#Fazer descritivas do n?mero de dependentes por sexo (DEU RUIM)#####

#TH para normalidade de dados$sexo=="f"
dados$numdep[dados$sexo=="f"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                          ,sd=sd(.))
#P-valor<Alfa rejeito Ho (O Q FAZER)

#TH para normalidade de dados$sexo=="m"
dados$numdep[dados$sexo=="m"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                          ,sd=sd(.))
#P-valor<Alfa rejeito Ho (O Q FAZER)

#TH para igualdade de variancias
var.test(dados$numdep[dados$sexo=="f"],dados$numdep[dados$sexo=="m"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$numdep[dados$sexo=="f"],dados$numdep[dados$sexo=="m"],var.equal=TRUE,
       alternative ="two.sided")

#Fazer descritivas do SERASA por sexo####
sjt.xtab(dados$sexo,dados$serasa,show.summary=F,var.labels = c("Sexo","Possui nome no SERASA")
         ,show.row.prc = T,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(56,37,34,63),nrow=2,ncol=2)
chisq.test(m,correct=F)

#Fazendo grafico

serasa_sexo <- table(dados$serasa,dados$sexo)

# Nomeando colunas  e linhas (respectivamente)
colnames(serasa_sexo) <- c("Feminino", "Masculino")
rownames(serasa_sexo) <- c("Sim", "Não")

# Escolhendo cores
sexo_serasa_cor <- c("#54b0f7","#ff91f7")

# Calculando as proporções
serasa_sexo <- apply(serasa_sexo, 2, function(x){x/sum(x,na.rm=T)}) ; serasa_sexo

par(mar = c(5, 4, 4, 8))

barplot(serasa_sexo, col=sexo_serasa_cor, 
        border="white", 
        xlab="Sexo", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Nome no SERASA por sexo")
legend(x = "topright", 
       legend = c("Satisfeito", "Não satisfeito"), 
       fill = sexo_serasa_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.40,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))

#Fazer descritivas do estado civil por sexo####
sjt.xtab(dados$sexo,dados$estciv,show.summary=F,var.labels = c("Sexo","Possui nome no SERASA")
         ,show.row.prc = T,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,34,31,28,23,15,13,23),nrow=2,ncol=2)
chisq.test(m,correct=F)

#Fazendo grafico

estciv_sexo <- table(dados$estciv,dados$sexo)

# Nomeando colunas  e linhas (respectivamente)
colnames(estciv_sexo) <- c("Feminino", "Masculino")
rownames(estciv_sexo) <- c("Casado", "Solteiro","Viuvo","Outros")

# Escolhendo cores
estciv_sexo_cor <- RColorBrewer::brewer.pal(4, "Set2")

# Calculando as proporções
estciv_sexo <- apply(estciv_sexo, 2, function(x){x/sum(x,na.rm=T)}) ; estciv_sexo

par(mar = c(5, 4, 4, 8))

barplot(estciv_sexo, col=estciv_sexo_cor, 
        border="white", 
        xlab="Sexo", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Estado civil por sexo")
legend(x = "topright", 
       legend = c("Casado", "Solteiro","Viuvo","Outros"), 
       fill = estciv_sexo_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.40,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))

#PERGUNTA 2


#PERGUNTA 2######

#Fazer descritivas da idade por satisfac?o#####

#TH para normalidade de dados$satisflimite=="s"
dados$idade[dados$satisflimite=="s"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                 ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
par(mfrow = c(1,2))
qqnorm(dados$idade[dados$satisflimite=="s"], col = "red", main = "Q-Q Plot \nIdade x Satisfeito com o limite", sub = "Verificando normalidade")
qqline(dados$idade[dados$satisflimite=="s"], col = "black")

ggplot(dados,aes(x=satisflimite,y=idade))+geom_boxplot(fill='blue')+ggtitle("Satisfação por idade")+xlab('Satisfção com o limite')

#TH para normalidade de dados$sexo=="m"
dados$idade[dados$satisflimite=="n"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                 ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$satisflimite=="n"], col = "red", main = "Q-Q Plot \nIdade x Não satisfeito com o limite", sub = "Verificando normalidade")
qqline(dados$idade[dados$satisflimite=="n"])

#TH para igualdade de variancias
var.test(dados$idade[dados$satisflimite=="s"],dados$idade[dados$satisflimite=="n"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$idade[dados$satisflimite=="s"],dados$idade[dados$satisflimite=="n"],var.equal=TRUE,
       alternative ="less")

#Fazer descritivas do satisfacao por serasa####
sjt.xtab(dados$serasa,dados$satisflimite,show.summary=F,var.labels = c("Possui nome no SERASA","Satisfeito com limite?")
         ,show.row.prc = T,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,70,28,69),nrow=2,ncol=2)
chisq.test(m,correct=F)

#Fazendo grafico

satis_serasa <- table(dados$satisflimite,dados$serasa)

# Nomeando colunas  e linhas (respectivamente)
colnames(satis_serasa) <- c("Não", "Sim")
rownames(satis_serasa) <- c("Não", "Sim")

# Escolhendo cores
satis_serasa_cor <- c("deepskyblue","firebrick3")

# Calculando as proporções
satis_serasa <- apply(satis_serasa, 2, function(x){x/sum(x,na.rm=T)}) ; satis_serasa

par(mar = c(5, 4, 4, 8))

barplot(satis_serasa, col=satis_serasa_cor, 
        border="white", 
        xlab="Satisfeito com limite?", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Satisfeito com o limite")
legend(x = "topright", 
       legend = c("Não tem nome \nno SERASA", "Nome no \nSERASA"), 
       fill = satis_serasa_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.75,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))

ggplot(dados,aes(x=satisflimite))+geom_bar(aes(fill=serasa))+ylab("Frequ?ncia")+
  xlab("Possui nome no SERASA")

#Fazer descritivas do satifacao por banco####
sjt.xtab(dados$banco,dados$satisflimite,show.summary=F,var.labels = c("Satisfeito com limite?","Banco")
         ,show.row.prc = T,show.col.prc = T)
sjt.xtab(dados$banco,dados$satisflimite,show.summary=F,var.labels = c("Satisfeito com limite?","Banco")
         ,show.cell.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(17,36,16,43,2,8,8,10,4,10,4,32),nrow=2,ncol=6)
chisq.test(m,correct=F)
ggplot(dados,aes(x=satisflimite))+geom_bar(aes(fill=serasa))+ylab("Frequ?ncia")+
  xlab("Possui nome no SERASA")

#Fazer descritivas do satifacao por tempo####

#TH para normalidade de dados$satisflimite=="s"
dados$tempocliente[dados$satisflimite=="s"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                        ,sd=sd(.))

# P-valor<Alfa rejeito Ho (FALAR COM LUDMILLA)
# par(mforw = c(1,1))
# qqnorm(dados$tempocliente[dados$satisflimite=="s"])
# qqline(dados$tempocliente[dados$satisflimite=="s"])

#TH para normalidade de dados$satisflimite=="n"
dados$tempocliente[dados$satisflimite=="n"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                        ,sd=sd(.))

# P-valor>Alfa nao rejeito Ho
# qqnorm(dados$tempocliente[dados$satisflimite=="n"])
# qqline(dados$tempocliente[dados$satisflimite=="n"])

#TH para igualdade de variancias
var.test(dados$tempocliente[dados$satisflimite=="n"],dados$tempocliente[dados$satisflimite=="s"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$tempocliente[dados$satisflimite=="n"],dados$tempocliente[dados$satisflimite=="s"],
       var.equal=TRUE,alternative ="less")

ggplot(dados,aes(x=satisflimite,y=tempocliente))+geom_boxplot(fill='green')+ggtitle("Satisfação por tempo")+xlab('Satisfção com o limite')

#Fazer descritivas do gerente por satisfa??o####
sjt.xtab(dados$gerente,dados$satisflimite,show.summary=F,var.labels = c("Falou com gerente","Satisfeito com limite?")
         ,show.row.prc = T,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,28,79,60),nrow=2,ncol=2)
chisq.test(m,correct=F)

gerente_satis <- table(dados$gerente,dados$satisflimite)

# Nomeando colunas  e linhas (respectivamente)
colnames(gerente_satis) <- c("Sim", "Não")
rownames(gerente_satis) <- c("Sim", "Não")

# Escolhendo cores
gerente_satis_cor <- c("#54b0f7","#ff91f7")

# Calculando as proporções
gerente_satis <- apply(gerente_satis, 2, function(x){x/sum(x,na.rm=T)}) ; gerente_satis

par(mar = c(5, 4, 4, 8.9))

barplot(gerente_satis, col=gerente_satis_cor, 
        border="white", 
        xlab="Sexo", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Gerente x Sastisfação")
legend(list(x=2.45,y=0.8), 
       legend = c("Falou gerente", "Não falou gerente"), 
       fill = sexo_serasa_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.40,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))

#Fazer descritivas do satifacao por tempo#########
sjt.xtab(dados$sexo,dados$satisflimite,show.summary=F,var.labels = c("Sexo","Satisfeito com limite?")
         ,show.row.prc = T,show.col.prc = T)

m <- matrix(c(34,17,56,83),nrow=2,ncol=2)
chisq.test(m,correct=F)
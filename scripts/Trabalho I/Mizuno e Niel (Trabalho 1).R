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

ggplot(dados,aes(x=banco))+geom_bar(fill="blue")+ylab("Frequ?ncia")+xlab("Bancos")

#Fazer descritivas da idade por sexo#####

#TH para normalidade de dados$sexo=="f"
dados$idade[dados$sexo=="f"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                         ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$sexo=="f"])
qqline(dados$idade[dados$sexo=="f"])

#TH para normalidade de dados$sexo=="m"
dados$idade[dados$sexo=="m"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                         ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$sexo=="m"])
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
ggplot(dados,aes(x=sexo,y=idade))+geom_boxplot()
curve(dnorm(x,mean(dados$idade),sd(dados$idade)),add=T)

par(mfrow = c(1,2))
#Idade do sexo feminino
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
qqnorm(dados$numdep[dados$sexo=="f"])
qqline(dados$numdep[dados$sexo=="f"])

#TH para normalidade de dados$sexo=="m"
dados$numdep[dados$sexo=="m"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                          ,sd=sd(.))
#P-valor<Alfa rejeito Ho (O Q FAZER)
qqnorm(dados$numdep[dados$sexo=="m"])
qqline(dados$numdep[dados$sexo=="m"])

#TH para igualdade de variancias
var.test(dados$numdep[dados$sexo=="f"],dados$numdep[dados$sexo=="m"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$numdep[dados$sexo=="f"],dados$numdep[dados$sexo=="m"],var.equal=FALSE,
       alternative ="two.sided")

#Fazer descritivas do SERASA por sexo####
sjt.xtab(dados$sexo,dados$serasa,show.summary=F,var.labels = c("Sexo","Possui nome no SERASA")
         ,show.row.prc = T)
sjt.xtab(dados$sexo,dados$serasa,show.summary=F,var.labels = c("Sexo","Possui nome no SERASA")
         ,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(56,37,34,63),nrow=2,ncol=2)
chisq.test(m,correct=F)
ggplot(dados,aes(x=serasa))+geom_bar(aes(fill=sexo))+ylab("Frequência")+xlab("Possui nome no SERASA")

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
         ,show.row.prc = T)
sjt.xtab(dados$sexo,dados$estciv,show.summary=F,var.labels = c("Sexo","Possui nome no SERASA")
         ,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,34,31,28,23,15,13,23),nrow=2,ncol=2)
chisq.test(m,correct=F)

ggplot(dados,aes(x=estciv))+geom_bar(aes(fill=sexo))+ylab("Frequ?ncia")+xlab("Estado Civil")

#Fazendo grafico

estciv_sexo <- table(dados$estciv,dados$sexo)

# Nomeando colunas  e linhas (respectivamente)
colnames(estciv_sexo) <- c("Feminino", "Masculino")
rownames(estciv_sexo) <- c("Casado", "Solteiro","Viuvo","Outros")

# Escolhendo cores
estciv_sexo_cor <- c("#54b0f7","#ff91f7","red","green")

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
       fill = sexo_serasa_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.40,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))
#Fazer descritivas da idade por satisfac?o#####

#TH para normalidade de dados$satisflimite=="s"
dados$idade[dados$satisflimite=="s"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                 ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$satisflimite=="s"])
qqline(dados$idade[dados$satisflimite=="s"])

#TH para normalidade de dados$sexo=="m"
dados$idade[dados$satisflimite=="n"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                 ,sd=sd(.))
#P-valor>Alfa nao rejeito Ho
qqnorm(dados$idade[dados$satisflimite=="n"])
qqline(dados$idade[dados$satisflimite=="n"])

#TH para igualdade de variancias
var.test(dados$idade[dados$satisflimite=="s"],dados$idade[dados$satisflimite=="n"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$idade[dados$satisflimite=="s"],dados$idade[dados$satisflimite=="n"],var.equal=FALSE,
       alternative ="two.sided")

#Fazer descritivas do satisfacao por serasa####
sjt.xtab(dados$satisflimite,dados$serasa,show.summary=F,var.labels = c("Satisfeito com limite?","Possui nome no SERASA")
         ,show.row.prc = T)
sjt.xtab(dados$satisflimite,dados$serasa,show.summary=F,var.labels = c("Satisfeito com limite?","Possui nome no SERASA")
         ,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,70,28,69),nrow=2,ncol=2)
chisq.test(m,correct=F)
ggplot(dados,aes(x=satisflimite))+geom_bar(aes(fill=serasa))+ylab("Frequ?ncia")+
  xlab("Possui nome no SERASA")

#Fazer descritivas do satifacao por banco####
sjt.xtab(dados$satisflimite,dados$banco,show.summary=F,var.labels = c("Satisfeito com limite?","Banco")
         ,show.row.prc = T)
sjt.xtab(dados$satisflimite,dados$banco,show.summary=F,var.labels = c("Satisfeito com limite?","Banco")
         ,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(17,36,16,43,2,8,8,10,4,10,4,32),nrow=2,ncol=6)
chisq.test(m,correct=F)
ggplot(dados,aes(x=satisflimite))+geom_bar(aes(fill=serasa))+ylab("Frequ?ncia")+
  xlab("Possui nome no SERASA")

#Fazer descritivas do satifacao por tempo####

#TH para normalidade de dados$satisflimite=="s"
dados$tempocliente[dados$satisflimite=="s"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                        ,sd=sd(.))

#P-valor<Alfa rejeito Ho (FALAR COM LUDMILLA)
qqnorm(dados$tempocliente[dados$satisflimite=="s"])
qqline(dados$tempocliente[dados$satisflimite=="s"])

#TH para normalidade de dados$satisflimite=="n"
dados$tempocliente[dados$satisflimite=="n"] %>% ks.test(x=.,"pnorm",alternative="two.sided",mean=mean(.)
                                                        ,sd=sd(.))

#P-valor>Alfa nao rejeito Ho
qqnorm(dados$tempocliente[dados$satisflimite=="n"])
qqline(dados$tempocliente[dados$satisflimite=="n"])

#TH para igualdade de variancias
var.test(dados$tempocliente[dados$satisflimite=="n"],dados$tempocliente[dados$satisflimite=="s"],ratio=1,
         alternative ="two.sided")

#TH para media
t.test(dados$tempocliente[dados$satisflimite=="n"],dados$tempocliente[dados$satisflimite=="s"],
       var.equal=TRUE,alternative ="two.sided")

#Fazer descritivas do gerente por satisfa??o####
sjt.xtab(dados$gerente,dados$satisflimite,show.summary=F,var.labels = c("Falou com gerente","Satisfeito com limite?")
         ,show.row.prc = T)
sjt.xtab(dados$gerente,dados$satisflimite,show.summary=F,var.labels = c("Falou com gerente","Satisfeito com limite?")
         ,show.col.prc = T)

#Fazendo teste do chi-quadrado
m <- matrix(c(23,28,79,60),nrow=2,ncol=2)
chisq.test(m,correct=F)
ggplot(dados,aes(x=satisflimite))+geom_bar(aes(fill=gerente))+ylab("Frequ?ncia")+
  xlab("Possui nome no SERASA")

gerente_satis <- table(dados$gerente,dados$satisflimite)

# Nomeando colunas  e linhas (respectivamente)
colnames(gerente_satis) <- c("Sim", "Não")
rownames(gerente_satis) <- c("Sim", "Não")

# Escolhendo cores
gerente_satis_cor <- c("#54b0f7","#ff91f7")

# Calculando as proporções
gerente_satis <- apply(gerente_satis, 2, function(x){x/sum(x,na.rm=T)}) ; gerente_satis

par(mar = c(5, 4, 4, 8))

barplot(gerente_satis, col=gerente_satis_cor, 
        border="white", 
        xlab="Sexo", 
        ylim = c(0,1.19),
        ylab = "Proporção",
        main = "Falou com gerente por sexo")
legend(list(x=2.3,y=0), 
       legend = c("Usou gerente", "Não usou gerente"), 
       fill = sexo_serasa_cor, 
       title = "Legenda",
       xpd = TRUE,
       inset = c(-0.40,0.30))

par(mar = c(5.1, 4.1, 4.1, 2.1))

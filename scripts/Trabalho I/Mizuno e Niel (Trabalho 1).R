# Pacotes ####
require(readxl) ; require(dplyr) ; require(readr) ; require(ggplot2)

# Diretório ###
getwd()
dir()

# Tratar base ###
# Leitura ###
dadosM <- read_xls("bd\\Trabalho I\\Perfil Homens e Mulheres - Bancos.xls", sheet = "homens")
dadosF <- read_xls("bd\\Trabalho I\\Perfil Homens e Mulheres - Bancos.xls", sheet = "mulheres")

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

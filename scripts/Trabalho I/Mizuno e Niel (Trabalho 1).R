# Pacotes ####
require(readxl) ; require(dplyr) ; require(readr) ; require(ggplot2)

# Diretório ###
setwd("/home/216054054/Área de trabalho/Trabalho 1")
dir()

# Tratar base ###
# Leitura ###
dadosM <- read_xls("Perfil Homens e Mulheres - Bancos.xls", sheet = "homens")
dadosF <- read_xls("Perfil Homens e Mulheres - Bancos.xls", sheet = "mulheres")

dadosM$sexo <- 1
dadosF$sexo <- 0

dados <- rbind(dadosF,dadosM) ; dados
dados <- dados %>% 
  select(sexo, everything()) ; dados 
rm(list = c("dadosM","dadosF"))

# Usar fatores ###
class(dados$sexo)
dados$sexo <- factor(dados$sexo, labels = c("f","m"))
dados$estciv <- factor(dados$estciv, labels = c("casado","solteiro","viuvo", "outros"))
dados$possuibem <- factor(dados$possuibem, labels = c("s","n"))
dados$serasa <- factor(dados$serasa, labels = c("s","n"))
dados$regproc <- factor(dados$regproc, labels = c("capital","nao-capital"))
dados$banco <- factor(dados$banco, labels = c("itau", "bradesco", "caixa", "santander", "outros"))
dados$bankline <- factor(dados$bankline, labels = c("s","n"))
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


table(dados$sexo, dados$estciv) %>% prop.table()
table(dados$sexo, dados$possuibem) %>% prop.table()
table(dados$sexo, dados$serasa) %>% prop.table()
table(dados$sexo, dados$regproc) %>% prop.table()
table(dados$sexo, dados$banco) %>% prop.table()
table(dados$sexo, dados$bankline) %>% prop.table()
table(dados$sexo, dados$cxeletro) %>% prop.table()
table(dados$sexo, dados$layout) %>% prop.table()
table(dados$sexo, dados$cheque) %>% prop.table()
table(dados$sexo, dados$poupanca) %>% prop.table()
table(dados$sexo, dados$fundinvest) %>% prop.table()
table(dados$sexo, dados$cartcred) %>% prop.table()
table(dados$sexo, dados$telefone) %>% prop.table()
table(dados$sexo, dados$gerente) %>% prop.table()
table(dados$sexo, dados$limite) %>% prop.table()
table(dados$sexo, dados$satisflimite) %>% prop.table()
table(dados$sexo, dados$maisdeumban) %>% prop.table()
table(dados$sexo, dados$previdencia) %>% prop.table()
table(dados$sexo, dados$seguro) %>% prop.table()
 










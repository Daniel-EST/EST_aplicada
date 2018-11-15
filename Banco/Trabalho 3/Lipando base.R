library(readr); library(readxl); library(dplyr); library(stringr); require(foreign)
base<-read_xlsx("Banco/Trabalho 3/base.xlsx")
#SO PARA O MIZUNO NÃO RODA ISSO, SE VC RODAR ISSO O PROBLEMA É SEU!!!!!!!!!
#Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")


#Transformando as variaveis
base$sexo<-base$sexo%>%as.factor()
base$escolaridade<-base$escolaridade%>%as.factor()
base$civil<-base$civil%>%as.factor()
base$estado<-base$estado%>%as.factor()
base$evento<-base$evento%>%as.factor()
base$horas_semana<-base$horas_semana%>%as.factor()
base$p_venda<-base$p_venda%>%as.factor()
base$preco_jogos<-base$preco_jogos%>%as.factor()
base$requisitos<-base$requisitos%>%as.factor()
base$dinheiro<-base$dinheiro%>%as.factor()

#Mexendo na coluna de generos de jogos
FPS <- base$genero_jogo %>% str_detect('FPS') 
RPG <- base$genero_jogo %>% str_detect('RPG')
RTS <- base$genero_jogo %>% str_detect('RTS')
LUTA <- base$genero_jogo %>% str_detect('Luta')
ACAO <- base$genero_jogo %>% str_detect('Ação')
PUZZ <- base$genero_jogo %>% str_detect('Puzzle')
OUTROS <- base$genero_jogo %>% str_detect('Outros')
MOBA <- base$genero_jogo %>% str_detect('MOBA')
SIM <- base$genero_jogo %>% str_detect('Simuladores')
ESPORTES <- base$genero_jogo %>% str_detect('Esportes')
ROYALE <- base$genero_jogo %>% str_detect('Battle Royale')

#Colocando Sim e Nao nos generos de jogos
base$FPS <- ifelse(FPS,"Sim","Nao")
base$RPG <- ifelse(RPG,"Sim","Nao")
base$RTS <- ifelse(RTS,"Sim","Nao")
base$LUTA <- ifelse(LUTA,"Sim","Nao")
base$ACAO <- ifelse(ACAO,"Sim","Nao")
base$PUZZ <- ifelse(PUZZ,"Sim","Nao")
base$OUTROS <- ifelse(OUTROS,"Sim","Nao")
base$MOBA <- ifelse(MOBA,"Sim","Nao")
base$SIM <- ifelse(SIM,"Sim","Nao")
base$ESPORTES <- ifelse(ESPORTES,"Sim","Nao")
base$ROYALE <- ifelse(ROYALE,"Sim","Nao")


#Mexendo na coluna produtos
Mecha<-base$produtos%>%str_detect("Merchandising")
Skins<-base$produtos%>%str_detect("Skin")
DLC<-base$produtos%>%str_detect("DLC")
Apri<-base$produtos%>%str_detect("XP")
Nenhum<-base$produtos%>%str_detect("Nenhu")
Outros_p<-base$produtos%>%str_detect("Outr")

#Colocando Sim e Nao nos produtos
base$Mecha<-ifelse(Mecha, "Sim", "Nao")
base$Skins<-ifelse(Skins, "Sim", "Nao")
base$DLC<-ifelse(DLC, "Sim", "Nao")
base$Apri<-ifelse(Apri, "Sim", "Nao")
base$Nenhum<-ifelse(Nenhum, "Sim", "Nao")
base$Outros_p<-ifelse(Outros_p, "Sim", "Nao")

#Mexendo na coluna plataforma
PC<-base$plataforma%>%str_detect("PC")
PS<-base$plataforma%>%str_detect("PS")
Xbox<-base$plataforma%>%str_detect("Xbox")
Celular<-base$plataforma%>%str_detect("Cel")
Wii<-base$plataforma%>%str_detect("Wii")

#ainda n terminei plataforma


#encontrando quem respondeu "nenhum" e outros produtos
a<-base%>%filter(Nenhum=="Sim")%>%select(Nenhum, DLC, Mecha,Skins, Apri, Outros_p)%>%group_by(Nenhum,DLC,Mecha,Apri, Skins, Outros_p)%>%summarise(freq=n())


#Criando bases para outras analises e salvando
base$estado[base$estado=="Santana Catarina"]="Santa Catarina"
base1<-base%>%select(c(sexo:dinheiro, requisitos))
base2<-base%>%select(c(jogabilidade,tempo_jogo,grafico,multiplayer,multiplayer_c, divertimento))

saveRDS(base1, "Banco/Trabalho 3/base1.rds")
saveRDS(base2,"Banco/Trabalho 3/Likert.rds")


summary(base1)

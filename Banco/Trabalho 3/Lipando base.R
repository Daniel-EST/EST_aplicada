library(readr); library(readxl); library(dplyr); library(stringr); require(foreign)
base<-read_xlsx("Banco/Trabalho 3/base.xlsx")
#SO PARA O MIZUNO NÃO RODA ISSO, SE VC RODAR ISSO O PROBLEMA É SEU!!!!!!!!!
#Sys.setlocale("LC_ALL", "pt_BR.ISO8859-1")

#alterando midia
base$midia[base$midia=="Física, Digital"]="Ambos"

#alterando santa catarina
base$estado[base$estado=="Santana Catarina"]="Santa Catarina"


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
base$midia<-base$midia%>%as.factor()

#Mexendo na coluna de generos de jogos
FPS <- base$genero_jogo %>% str_detect('FPS') 
RPG <- base$genero_jogo %>% str_detect('RPG')
RTS <- base$genero_jogo %>% str_detect('RTS')
MMORPG<-base$genero_jogo%>%str_detect("MMORPG")
MOBA <- base$genero_jogo %>% str_detect('MOBA')
ROYALE <- base$genero_jogo %>% str_detect('Battle Royale')
SIM <- base$genero_jogo %>% str_detect('Simuladores')
ESPORTES <- base$genero_jogo %>% str_detect('Esportes')
LUTA <- base$genero_jogo %>% str_detect('Luta')
ACAO <- base$genero_jogo %>% str_detect('Ação')
PUZZ <- base$genero_jogo %>% str_detect('Puzzle')
OUTROS <- base$genero_jogo %>% str_detect('Outros')


#Colocando Sim e Nao nos generos de jogos
base$FPS <- ifelse(FPS,"Sim","Nao")
base$RPG <- ifelse(RPG,"Sim","Nao")
base$RTS <- ifelse(RTS,"Sim","Nao")
base$MMORPG<- ifelse(MMORPG,"Sim", "Nao")
base$MOBA <- ifelse(MOBA,"Sim","Nao")
base$ROYALE <- ifelse(ROYALE,"Sim","Nao")
base$SIM <- ifelse(SIM,"Sim","Nao")
base$ESPORTES <- ifelse(ESPORTES,"Sim","Nao")
base$LUTA <- ifelse(LUTA,"Sim","Nao")
base$ACAO <- ifelse(ACAO,"Sim","Nao")
base$PUZZ <- ifelse(PUZZ,"Sim","Nao")
base$OUTROS <- ifelse(OUTROS,"Sim","Nao")

#Mexendo na coluna produtos
DLC<-base$produtos%>%str_detect("DLC")
Skins<-base$produtos%>%str_detect("Skin")
Apri<-base$produtos%>%str_detect("XP")
Mecha<-base$produtos%>%str_detect("Merchandising")
Nenhum<-base$produtos%>%str_detect("Nenhu")
Outros_p<-base$produtos%>%str_detect("Outr")

#Colocando Sim e Nao nos produtos
base$DLC<-ifelse(DLC, "Sim", "Nao")
base$Skins<-ifelse(Skins, "Sim", "Nao")
base$Apri<-ifelse(Apri, "Sim", "Nao")
base$Mecha<-ifelse(Mecha, "Sim", "Nao")
base$Nenhum<-ifelse(Nenhum, "Sim", "Nao")
base$Outros_p<-ifelse(Outros_p, "Sim", "Nao")

#Mexendo na coluna plataforma
PC<-base$plataforma%>%str_detect("PC")
Xbox<-base$plataforma%>%str_detect("Xbox")
PS<-base$plataforma%>%str_detect("PS")
Wii<-base$plataforma%>%str_detect("Wii")
Celular<-base$plataforma%>%str_detect("Cel")
Outros_pla<-base$plataforma%>%str_detect("Outro")


#Colocando Sim e Nao nas plataformas
base$PC<-ifelse(PC,"Sim", "Nao")
base$Xbox<- ifelse(Xbox, "Sim", "Nao")
base$PS<-ifelse(PS, "Sim", "Nao")
base$Wii<- ifelse(Wii, "Sim", "Nao")
base$Celular<-ifelse(Celular, "Sim", "Nao")
base$Outros_pla<-ifelse(Outros_pla, "Sim", "Nao")

#encontrando quem respondeu "nenhum" e outros produtos
a<-base%>%filter(Nenhum=="Sim")%>%select(Nenhum, DLC, Mecha,Skins, Apri, Outros_p)%>%group_by(Nenhum,DLC,Mecha,Apri, Skins, Outros_p)%>%summarise(freq=n())

which(base$Nenhum=="Sim"&base$DLC=="Sim")
which(base$Nenhum=="Sim"&base$Mecha=="Sim")
which(base$Nenhum=="Sim"&base$Apri=="Sim")
which(base$Nenhum=="Sim"&base$Skins=="Sim")
which(base$Nenhum=="Sim"&base$Outros_p=="Sim")

#removendo as linhas do pessoal errado
base<-base[-c(167,658,771,1200,1241,133,277,1295,1524,1550,1661,1744,352,751,1036,1490,1671),]


#removendo colunas desnecessarias
base<-base%>%select(-c(produtos,plataforma,genero_jogo))


#Criando bases para outras analises e salvando
base1<-base%>%select(c(sexo:dinheiro, requisitos, FPS:Outros_pla))
base2<-base%>%select(c(jogabilidade,tempo_jogo,grafico,multiplayer,multiplayer_c, divertimento))

saveRDS(base1, "Banco/Trabalho 3/base1.rds")
saveRDS(base2,"Banco/Trabalho 3/Likert.rds")


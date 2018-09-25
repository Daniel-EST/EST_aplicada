library(dplyr)
library(readxl)
require(stringr)

base <- readRDS("Base_modificada2.rds")

base$Estado <- base$Estado %>% 
  factor()
base$Espacialidades <- base$Espacialidades %>% 
  factor()

base2 <- base %>% select(-c(Espacialidades,Estado,Código,`População rural 2000`
                           ,`População rural 2010`,`População rural 2000`
                           ,`População rural 2010`,`População urbana 2000`,
                           `População total 2000`,`População total 2010`,
                           `População urbana 2010`, `% dos ocupados com superior completo - 18 anos ou mais 2000`,
                           `% dos ocupados com superior completo - 18 anos ou mais 2010`))

names(base2)

# Tiramos a linha do Brasil
base2 <- base2[-1,]

#Padronizando e multiplicando por 100
padronizacao <- function(a){
  x <- (max(a)-a)/(max(a)-min(a))
}

base2 <- base2 %>% mutate(`Esperança de vida ao nascer 2000`=100*padronizacao(`Esperança de vida ao nascer 2000`),
                        `Esperança de vida ao nascer 2010`=100*padronizacao(`Esperança de vida ao nascer 2010`),
                        `Renda per capita dos extremamente pobres 2000`=100*padronizacao(`Renda per capita dos extremamente pobres 2000`),
                        `Renda per capita dos extremamente pobres 2010`=100*padronizacao(`Renda per capita dos extremamente pobres 2010`),
                        `Renda per capita dos pobres 2000`=100*padronizacao(`Renda per capita dos pobres 2000`),
                        `Renda per capita dos pobres 2010`=100*padronizacao(`Renda per capita dos pobres 2010`),
                        `Renda per capita dos vulneráveis à pobreza 2000`=100*padronizacao(`Renda per capita dos vulneráveis à pobreza 2000`),
                        `Renda per capita dos vulneráveis à pobreza 2010`=100*padronizacao(`Renda per capita dos vulneráveis à pobreza 2010`),
                        `Mortalidade infantil 2010`=100*padronizacao(`Mortalidade infantil 2010`),
                        `Mortalidade infantil 2000`=100*padronizacao(`Mortalidade infantil 2000`))
              
#Deixando de 0 a 100
base2 <- base2 %>% mutate(`Índice de Gini 2000`=100*`Índice de Gini 2000`,
                        `Índice de Gini 2010`=100*`Índice de Gini 2010`,
                        `IDHM 2000`=100*`IDHM 2000`,
                        `IDHM 2010`=100*`IDHM 2010`)

#Mudando sentido
base2 <- base2 %>% mutate(`Mortalidade infantil 2000`=100-`Mortalidade infantil 2000`,
                          `Mortalidade infantil 2010`=100-`Mortalidade infantil 2010`,
                          `Taxa de analfabetismo - 15 anos ou mais 2000`=100-`Taxa de analfabetismo - 15 anos ou mais 2000`,
                          `Taxa de analfabetismo - 15 anos ou mais 2010`=100-`Taxa de analfabetismo - 15 anos ou mais 2010`,
                          `% de extremamente pobres 2000`=100-`% de extremamente pobres 2000`,
                          `% de extremamente pobres 2010`=100-`% de extremamente pobres 2010`,
                          `% de pobres 2000`=100-`% de pobres 2000`,
                          `% de pobres 2010`=100-`% de pobres 2010`,
                          `% de vulneráveis à pobreza 2000`=100-`% de vulneráveis à pobreza 2000`,
                          `% de vulneráveis à pobreza 2010`=100-`% de vulneráveis à pobreza 2010`)
base_2000 <- base2 %>% 
  select(which(str_sub(names(base2),start = -4) == "2000"))
  
base_2010 <- base2 %>% 
  select(which(str_sub(names(base2),start = -4) == "2010"))                 

# Matriz de covariancia e correlação
m_2000 <- structure(base_2000)
m_2010 <- structure(base_2010)

mat_cor_2000 <- round(cor(m_2000),2)
mat_cov_2000 <- cov(m_2000)

mat_cor_2010 <- round(cor(m_2010),2)
mat_cov_2010 <- cov(m_2010)

write.csv2(mat_cor_2000,"Matriz Correlação 2000.csv",row.names=FALSE)
write.csv2(mat_cov_2000,"Matriz Covariancia 2000.csv",row.names=FALSE)

write.csv2(mat_cor_2010,"Matriz Correlação 2010.csv",row.names=FALSE)
write.csv2(mat_cov_2010,"Matriz Covariancia 2010.csv",row.names=FALSE)


base2 %>% 
  filter(Espacialidades == c("São João do Sabugi", "Santa Luzia", "São Mamede")) %>% 
  select(Espacialidades, `% de 18 a 24 anos no fundamental REGULAR SERIADO 2010`,`% de 18 a 24 anos no médio REGULAR SERIADO 2010`)
mean(c(8.07,5.26,3.41))

summary(base2)

saveRDS(base2, "Banco/Trabalho 2/Uteis/Base_sem_na.rds")
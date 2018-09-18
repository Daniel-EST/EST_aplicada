library(dplyr)

base <- readRDS('Banco/Trabalho 2/Uteis/Base_modificada.rds')

base$Estado <- base$Estado %>% 
  factor()
base$Espacialidades <- base$Espacialidades %>% 
  factor()

base2 <- base %>% select(-c(Espacialidades,Estado,Código,`População rural 2000`
                           ,`População rural 2010`,`População rural 2000`
                           ,`População rural 2010`,`População urbana 2000`,
                           `População urbana 2010`))

summary(base)

# Tiramos a linha do Brasil
base <- base[-1,] 

#Padronizando e multiplicando por 100
padronizacao <- function(a){
  x <- (max(a)-a)/(max(a)-min(a))
}

base2 <- base %>% mutate(`Esperança de vida ao nascer 2000`=100*padronizacao(base$`Esperança de vida ao nascer 2000`),
                        `Esperança de vida ao nascer 2010`=100*padronizacao(base$`Esperança de vida ao nascer 2010`),
                        `Renda per capita dos extremamente pobres 2000`=100*padronizacao(`Renda per capita dos extremamente pobres 2000`),
                        `Renda per capita dos extremamente pobres 2010`=100*padronizacao(`Renda per capita dos extremamente pobres 2010`),
                        `Renda per capita dos pobres 2000`=100*padronizacao(`Renda per capita dos pobres 2000`),
                        `Renda per capita dos pobres 2010`=100*padronizacao(`Renda per capita dos pobres 2010`),
                        `Renda per capita dos vulneráveis à pobreza 2000`=100*padronizacao(`Renda per capita dos vulneráveis à pobreza 2000`),
                        `Renda per capita dos vulneráveis à pobreza 2010`=100*padronizacao(`Renda per capita dos vulneráveis à pobreza 2010`),
                        `Mortalidade infantil 2010`=100*padronizacao(`Mortalidade infantil 2010`),
                        `Mortalidade infantil 2000`=100*padronizacao(`Mortalidade infantil 2000`))
              
#Deixando de 0 a 100
base2 <- base %>% mutate(`Índice de Gini 2000`=100*`Índice de Gini 2000`,
                        `Índice de Gini 2010`=100*`Índice de Gini 2010`,
                        `IDHM 2000`=100*`IDHM 2000`,
                        `IDHM 2010`=100*`IDHM 2010`)

#Matriz de covariancia e correlação
m <- structure(base2)
mat_cor <- cor(m)
mat_cov <- cov(m)
write.csv(mat_cor,"Matriz Correlação.csv",row.names=FALSE)
write.csv(mat_cov,"Matriz Covariancia.csv",row.names=FALSE)



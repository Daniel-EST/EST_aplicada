library(dplyr)
library(readxl)
require(stringr)
require(ggplot2)

base <- readRDS("Banco/Trabalho 2/Base_modificada2.rds")

base$Estado <- base$Estado %>% 
  factor()
base$Espacialidades <- base$Espacialidades %>% 
  factor()

base2 <- base %>% select(c(Espacialidades,Estado,Código,`Esperança de vida ao nascer 2000`,`Esperança de vida ao nascer 2010`,
                           `Mortalidade infantil 2000`,`Mortalidade infantil 2010`,`Taxa de analfabetismo - 15 anos ou mais 2000`,
                            `Taxa de analfabetismo - 15 anos ou mais 2010`,`% de pobres 2000`,`% de pobres 2010`,`% de vulneráveis à pobreza 2000`,
                           `% de vulneráveis à pobreza 2010`,`IDHM 2000`,`IDHM 2010`))
base2 <- base2[-1,]


base_2000 <- base2 %>% 
  select(c(Espacialidades,Estado,Código),which(str_sub(names(base2),start = -4) == "2000"))

base_2010 <- base2 %>% 
  select(c(Espacialidades,Estado,Código),which(str_sub(names(base2),start = -4) == "2010")) 

padronizar <- function(a) return((max(a)-a)/(max(a)- min(a)))

base_2000 <- mutate(base_2000,`Esperança de vida ao nascer 2000` = 1- padronizar(`Esperança de vida ao nascer 2000`),
                    `Mortalidade infantil 2000` = padronizar(`Mortalidade infantil 2000`),
                    `Taxa de analfabetismo - 15 anos ou mais 2000` = padronizar(`Taxa de analfabetismo - 15 anos ou mais 2000`),
                    `% de pobres 2000` = padronizar(`% de pobres 2000`),
                    `% de vulneráveis à pobreza 2000` = padronizar(`% de vulneráveis à pobreza 2000`),
                    `IDHM 2000` = padronizar(`IDHM 2000`)) ; head(base_2000,4)

base_2010 <- mutate(base_2010,`Esperança de vida ao nascer 2010` = 1 - padronizar(`Esperança de vida ao nascer 2010`),
                    `Mortalidade infantil 2010` = padronizar(`Mortalidade infantil 2010`),
                    `Taxa de analfabetismo - 15 anos ou mais 2010` = padronizar(`Taxa de analfabetismo - 15 anos ou mais 2010`),
                    `% de pobres 2010` = padronizar(`% de pobres 2010`),
                    `% de vulneráveis à pobreza 2010` = padronizar(`% de vulneráveis à pobreza 2010`),
                    `IDHM 2010` = padronizar(`IDHM 2010`)) ; head(base_2010,4)

mat_cor_2000 <- base_2000 %>% select(-c(Espacialidades,Estado,Código)) %>% 
  structure() %>% 
  cor() %>% 
  round(2)

mat_cor_2010 <- base_2010 %>% select(-c(Espacialidades,Estado,Código)) %>% 
  structure() %>% 
  cor() %>% 
  round(2)

base_2000 <- mutate(base_2000,Indicador_final=(`Esperança de vida ao nascer 2000`+`Mortalidade infantil 2000`+`Taxa de analfabetismo - 15 anos ou mais 2000`+
                                                 `% de pobres 2000`+`% de vulneráveis à pobreza 2000`)/5)
base_2010 <- mutate(base_2010,Indicador_final=(`Esperança de vida ao nascer 2010`+`Mortalidade infantil 2010`+`Taxa de analfabetismo - 15 anos ou mais 2010`+
                                                 `% de pobres 2010`+`% de vulneráveis à pobreza 2010`)/5)

base_2000$Indicador_final <- round(base_2000$Indicador_final,3)
base_2010$Indicador_final <- round(base_2010$Indicador_final,3)

base_2010 %>% select(c(`IDHM 2010`,Indicador_final)) %>% 
  structure() %>% 
  cor() %>% 
  round(2)

lm(base_2000$`IDHM 2000`~base_2000$Indicador_final)

plot(base_2000$Indicador_final, base_2000$`IDHM 2000`, main="IDHM & Indicador final 2000", xlab="Indicador Final", ylab = "IDHM")
abline(lm(base_2000$`IDHM 2000`~base_2000$Indicador_final), col = "#b7792d")

lm(base_2000$`IDHM 2010`~base_2010$Indicador_final)

plot(base_2010$Indicador_final, base_2010$`IDHM 2010`,main="IDHM & Indicador final 2010", xlab="Indicador Final", ylab = "IDHM")
abline(lm(base_2010$`IDHM 2010`~base_2010$Indicador_final), col = "#b7792d")


write.csv2(base_2000,"Indicadores 2000.csv",row.names=FALSE)
write.csv2(base_2010,"Indicadores 2010.csv",row.names=FALSE)

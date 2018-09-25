library(dplyr)
library(readxl)
require(stringr)

base <- readRDS("Base_modificada2.rds")

base$Estado <- base$Estado %>% 
  factor()
base$Espacialidades <- base$Espacialidades %>% 
  factor()
base2 <- base %>% select(c(Espacialidades,Estado,Código,`Esperança de vida ao nascer 2000`,`Esperança de vida ao nascer 2010`,
                           `Mortalidade infantil 2000`,`Mortalidade infantil 2010`,`Taxa de analfabetismo - 15 anos ou mais 2000`,
                            `Taxa de analfabetismo - 15 anos ou mais 2010`,`% de pobres 2000`,`% de pobres 2010`,`% de vulneráveis à pobreza 2000`,
                           `% de vulneráveis à pobreza 2010`))
base2 <- base2[-1,]


base_2000 <- base2 %>% 
  select(c(Espacialidades,Estado,Código),which(str_sub(names(base2),start = -4) == "2000"))


base_2010 <- base2 %>% 
  select(c(Espacialidades,Estado,Código),which(str_sub(names(base2),start = -4) == "2010")) 

write.csv2(base_2000,"Indicadores 2000.csv",row.names=FALSE)
write.csv2(base_2010,"Indicadores 2010.csv",row.names=FALSE)

base_2000 <- mutate(base_2000,Indicador_final=(`Esperança de vida ao nascer 2000`+`Mortalidade infantil 2000`+`Taxa de analfabetismo - 15 anos ou mais 2000`+
                                                 `% de pobres 2000`+`% de vulneráveis à pobreza 2000`)/5)
base_2010 <- mutate(base_2010,Indicador_final=(`Esperança de vida ao nascer 2010`+`Mortalidade infantil 2010`+`Taxa de analfabetismo - 15 anos ou mais 2010`+
                                                 `% de pobres 2010`+`% de vulneráveis à pobreza 2010`)/5)




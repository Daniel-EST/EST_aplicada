library(dplyr)

base <- readRDS('Base_modificada.rds')

base <- base %>% select(-c(Espacialidades,Estado,Código,`População rural 2000`
                           ,`População rural 2010`,`População rural 2000`
                           ,`População rural 2010`,`População urbana 2000`,
                           `População urbana 2010`))

base$Estado <- base$Estado %>% 
  factor()
base$Espacialidades <- base$Espacialidades %>% 
  factor()

base <- base[-1,] # Tiramos a linha do Brasil
m <- structure(base)
mat_cor <- cor(m)
mat_cov <- cov(m)
base$`asdfa asdfas asdf ad `
write.csv(mat_cor,"Matriz Correlação.csv",row.names=FALSE)
write.csv(mat_cov,"Matriz Covariancia.csv",row.names=FALSE)

nomes <- names(base)

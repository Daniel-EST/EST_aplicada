library(dplyr)
base <- read_xlsx("Banco/Trabalho 2/base2.xlsx")
base <- mutate(base, Estado = substring(Código,1,2) )
base <- base %>% select(Código,Espacialidades,Estado,everything())
base$Estado[base$Estado==21]="MA"
base$Estado[base$Estado==22]="PI"
base$Estado[base$Estado==23]="CE"
base$Estado[base$Estado==24]="RN"
base$Estado[base$Estado==25]="PB"
base$Estado[base$Estado==26]="PE"
base$Estado[base$Estado==27]="AL"
base$Estado[base$Estado==28]="SE"
base$Estado[base$Estado==29]="BA"
saveRDS(base, "Banco/Trabalho 2/Base_modificada2.rds")

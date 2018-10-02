library(dplyr)
library(readxl)
require(stringr)
require(ggplot2)

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

padronizar <- function(a) return((max(a)-a)/(max(a)- min(a)))

base_2000 <- mutate(base_2000,`Esperança de vida ao nascer 2000` = 1- padronizar(`Esperança de vida ao nascer 2000`),
                    `Mortalidade infantil 2000` = padronizar(`Mortalidade infantil 2000`),
                    `Taxa de analfabetismo - 15 anos ou mais 2000` = padronizar(`Taxa de analfabetismo - 15 anos ou mais 2000`),
                    `% de pobres 2000` = padronizar(`% de pobres 2000`),
                    `% de vulneráveis à pobreza 2000` = padronizar(`% de vulneráveis à pobreza 2000`))

base_2000 <- base_2000 %>% select(-c(Espacialidades,Estado,Código))

base_2000 <- base_2000 %>% rename(Esperança=`Esperança de vida ao nascer 2000`,`Mortalidade infantil`=`Mortalidade infantil 2000`,
                                  `Taxa de analfabetismo`=`Taxa de analfabetismo - 15 anos ou mais 2000`,`% de pobres`=`% de pobres 2000`,
                                  `% de vulneráveis à pobreza`=`% de vulneráveis à pobreza 2000`)

base_2010<- base_2010 %>% select(-c(Espacialidades,Estado,Código))

base_2010 <- mutate(base_2010,`Esperança de vida ao nascer 2010` = 1- padronizar(`Esperança de vida ao nascer 2010`),
                    `Mortalidade infantil 2010` = padronizar(`Mortalidade infantil 2010`),
                    `Taxa de analfabetismo - 15 anos ou mais 2010` = padronizar(`Taxa de analfabetismo - 15 anos ou mais 2010`),
                    `% de pobres 2010` = padronizar(`% de pobres 2010`),
                    `% de vulneráveis à pobreza 2010` = padronizar(`% de vulneráveis à pobreza 2010`))

base_2010 <- base_2010 %>% rename(Esperança=`Esperança de vida ao nascer 2010`,`Mortalidade infantil`=`Mortalidade infantil 2010`,
                                  `Taxa de analfabetismo`=`Taxa de analfabetismo - 15 anos ou mais 2010`,`% de pobres`=`% de pobres 2010`,
                                  `% de vulneráveis à pobreza`=`% de vulneráveis à pobreza 2010`)


# 2000 --------------------------------------------------------------------

cormat <- round(cor(base_2000),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Comentario --------------------------------------------------------------

# Heatmap
#library(ggplot2)
#ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
 # geom_tile(color = "white")+
 #  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      # midpoint = 0, limit = c(-1,1), space = "Lab", 
                       #name="Pearson\nCorrelation") +
  #theme_minimal()+ 
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   #size = 12, hjust = 1))+
  #coord_fixed()

# comentario --------------------------------------------------------------


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap1 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ggtitle("2000")
# Print the heatmap
print(ggheatmap)

#Colocando numero

ggheatmap1 + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


# 2010 --------------------------------------------------------------------

cormat <- round(cor(base_2010),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap2 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+ggtitle("2010")
# Print the heatmap
print(ggheatmap)

#Colocando numero

ggheatmap2 + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


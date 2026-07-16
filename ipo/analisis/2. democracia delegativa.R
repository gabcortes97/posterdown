# Librerías

library(tidyverse)
library(psych)


# Cargar datos

wvs <- read.csv("ipo/input/Datos/wvs.csv", header = T, sep = ";")

df <- wvs %>% dplyr::select(c(D_INTERVIEW, N_REGION_ISO,
                              Q27, Q48, Q109, Q182, Q185, 
                              Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178, #LCA
                              Q45, Q46, Q106, Q131, Q176, Q199, Q222, Q240, # otras variables
                              Q235, Q236, Q238, Q250, # democracia
                              Q260, Q262, Q274, Q275, Q281, Q288, Q289, G_TOWNSIZE2,
                              H_SETTLEMENT)) %>% #demográficos
  mutate(across(c(Q48, Q27, Q109, Q182, Q185, 
                            Q186, Q246, Q247, Q249, Q257, Q150, Q177, Q178,
                            Q45, Q46, Q106, Q131, Q176, Q199, Q222,
                            Q235, Q236, Q238, Q250,
                            Q260, Q262, Q274, Q275, Q281, Q288, Q289),~ifelse(.<0, NA, .))) 

# Análisis descriptivo

df$Q235 <- factor(df$Q235, levels=(c(1,2,3,4)), labels=c("Muy Bueno", "Bueno", "Malo", "Muy Malo"))


lf <- df %>% group_by(Q235) %>% filter(Q235!=is.na(Q235)) %>% summarise(D_INTERVIEW= n()) %>%
  mutate("Líder Fuerte"= round((D_INTERVIEW/sum(D_INTERVIEW)*100),2)) %>% rename("Categoría" = Q235)


# Guardar

saveRDS(df, "ipo/input/Datos/df.rds")


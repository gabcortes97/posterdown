library(tidyverse)
library(performance)

# Cargar datos
datos <- readRDS("ipo/input/Datos/data.rds")

# Recodificar algunas variables
datos$clase <- factor(datos$clase, levels = c(1,2,3,4),
                      labels= c("Individidualismo Autoritario",
                                "Individualismo Conservador",
                                "Individualismo Liberal",
                                "Individualismo Estratégico"))
table(datos$clase)

table(datos$Q235)

datos <- datos %>%
  mutate(lider_recod= ifelse(Q235=="Muy Malo"|
                               Q235=="Malo", 0, 1))

table(datos$lider_recod)
mean(datos$lider_recod, na.rm=T)

datos$genero <- factor(datos$Q260, levels=c(1,2),
                       labels= c("Hombre", "Mujer"))

datos <- datos %>% mutate(id_pol= case_when(Q240<3 & Q240>0~ "Izquierda",
                                            Q240==3 | Q240==4 ~"Centro Izquierda",
                                            Q240==5 ~ "Centro",
                                            Q240>=6 & Q240<=8  ~"Centro Derecha",
                                            Q240>8 ~"Derecha",
                                            Q240<0 ~ "Ninguna"))
datos$id_pol <- factor(datos$id_pol, levels = c("Izquierda",
                                                "Centro Izquierda",
                                                "Centro",
                                                "Centro Derecha",
                                                "Derecha", 
                                                "Ninguna"))
datos$id_pol <- relevel(datos$id_pol, ref="Ninguna")

datos <- datos %>%
  mutate(religion= case_when(Q289== 0 ~ "Sin religión",
                             Q289== 1 ~ "Católica",
                             Q289==2 | Q289==8 ~ "Evangélica",
                             Q289>2 ~ "Otra"))

datos$religion <- factor(datos$religion, levels= c("Sin religión",
                                                   "Católica",
                                                   "Evangélica",
                                                   "Otra"))
datos <- datos %>%
  mutate(tipo_ciudad= case_when(G_TOWNSIZE2==1~ "Rural",
                                G_TOWNSIZE2==2|G_TOWNSIZE2==3~ "Menos de 100.000",
                                G_TOWNSIZE2==4 ~ "Sobre 100.000",
                                G_TOWNSIZE2==5 ~ "Santiago"))
datos$tipo_ciudad <- factor(datos$tipo_ciudad, levels = c("Rural",
                                                          "Menos de 100.000",
                                                          "Sobre 100.000",
                                                          "Santiago"))
datos$tipo_ciudad <- relevel(datos$tipo_ciudad, ref= "Santiago")

datos <- datos %>% mutate(trabajo = case_when(Q281== 0 ~ NA_character_,
                                              Q281==1 | Q281==2 ~ "Clase de Servicio",
                                              Q281>=3 & Q281<=5 ~ "Clase Media",
                                              Q281>5 ~ "Clase Trabajadora"))
datos$trabajo <- factor(datos$trabajo, levels= c("Clase de Servicio",
                                                 "Clase Media",
                                                 "Clase Trabajadora"))
datos$trabajo <- relevel(datos$trabajo, ref = "Clase de Servicio")


# Análisis Descriptivo

t_ad <- datos %>% group_by(clase) %>% summarise(Lider= mean(lider_recod, na.rm=T)) %>%
  mutate("Perfiles"= clase,
         "Apoyo a Lideres Fuertes"= round((Lider*100), 2)) %>%
  dplyr::select(c("Perfiles", "Apoyo a Lideres Fuertes"))

t_ad

### Regresiones logísticas
datos$clase <- relevel(datos$clase, ref = "Individualismo Conservador")

m4 <- glm(lider_recod ~ clase + genero + Q262 + id_pol + Q288 + religion + tipo_ciudad + trabajo, data=datos, family = "binomial")
summary(m4) # Modelo completo

r2_tjur(m4)

m4a <- glm(lider_recod ~ clase, data=datos, family = "binomial")
summary(m4a) #Modelo solo con individualismo

r2_tjur(m4a)


m4b <- glm(lider_recod ~genero + Q262 + id_pol + Q288 + religion + tipo_ciudad + trabajo, data=datos, family = "binomial")
summary(m4b) # Modelo sin individualismo

r2_tjur(m4b)
saveRDS(datos, "ipo/input/Datos/datosLM.rds")
saveRDS(m4, "ipo/output/m4.rds")
saveRDS(m4a, "ipo/output/m4a.rds")
saveRDS(m4b, "ipo/output/m4b.rds")


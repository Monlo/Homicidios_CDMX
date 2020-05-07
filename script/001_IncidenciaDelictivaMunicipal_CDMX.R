rm(list=ls())
setwd("~")

#### Calcular tasa de homicidios dolosos en la Ciudad de México por alcaldía

# Fuente de Datos: Incidencia delictiva del fuero comun municipal
# Liga: https://www.gob.mx/sesnsp/acciones-y-programas/incidencia-delictiva-87005?idiom=es 

# Cargar paquetes
# install.packages("pacman")
library("pacman")
p_load(tidyverse, readr, moderndive, janitor, rgdal, rgeos, ggrepel, maptools, ggplot2, reshape2)

# Cargar diccionarios
datos = "/Users/monlopez/Documents/MON/Programming/Projects/homicidios_cdmx/data"
out = "/Users/monlopez/Documents/MON/Programming/Projects/homicidios_cdmx/graphs"

# Cargar base de datos
inci_deli <- read.csv(paste(datos,"Municipal-Delitos-2015-2020_mar2020.csv", sep="/"), fileEncoding = "latin1") %>% 
  clean_names()

# Calcular totales anuales. Crear una nueva columna con la suma del total de cada mes 
inci_deli$total_delito <- rowSums(inci_deli[10:21])

# Filtrar datos de homicidios y feminicidios en la Ciudad de México y quitar 2020 (para hacer comparación anual)
cdmx <- inci_deli %>% 
  filter(clave_ent == 9, subtipo_de_delito == "Homicidio doloso" | subtipo_de_delito == "Feminicidio",
         ano != 2020) 

# Agrupar por tipo de delito y año
cdmx_tipo_delito <-  
  cdmx %>% 
  group_by(ano, tipo_de_delito) %>% 
  summarise(total = sum(total_delito, na.rm=TRUE)) %>% 
  ungroup()

# Graficar por homicidios y feminicidios por año
ggplot(cdmx_tipo_delito) +
  geom_line(aes(x=ano, y=total, color=tipo_de_delito)) +
  geom_point(aes(x=ano, y=total), color="black", size=2, shape=16) +
  geom_text_repel(aes(ano, total, label = total), force=1, 
                  vjust=0,
                  direction='y',
                  nudge_x=0.,
                  segment.size=0.5) +
  scale_color_manual(values=c("#a50f15","#225ea8")) +
  labs(title ="Evolución histórica de los homicidios dolosos y feminicidios", 
       subtitle="Ciudad de México (2015-2019)", x = "", 
       y = "", caption = "Fuente: SESNSP", color ="Delito") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(from=0, to=4000, by=500))  +
  theme(axis.text.x = element_text(face="bold", size = 10),
        title  = element_text(face="bold", size=14))

ggsave("evolucion.png")

##### Población Municipal de CONAPO

# Fuente de datos: Proyecciones de población de CONAPO
# Disponible en: https://datos.gob.mx/busca/dataset/proyecciones-de-la-poblacion-de-mexico-y-de-las-entidades-federativas-2016-2050

pob1 <- read.csv("/Users/monlopez/Documents/MON/Programming/Projects/homicidios_cdmx/data/poblacion_conapo/base_municipios_final_datos_01.csv", stringsAsFactors=FALSE, fileEncoding="latin1") %>% clean_names()
pob2 <- read.csv("/Users/monlopez/Documents/MON/Programming/Projects/homicidios_cdmx/data/poblacion_conapo/base_municipios_final_datos_02.csv", stringsAsFactors=FALSE, fileEncoding="latin1")  %>% clean_names()

# Las junto
pob <- rbind(pob1,pob2) %>% 
  clean_names()

# Sumo poblacion por municipio y año
pob_total = group_by(pob, clave_ent, clave, mun, ano) %>% 
  summarize(pob = sum(pob, na.rm=T))

##### Juntar homicidios cdmx y población municipal

# Agrupar por municipio y total de delitos de homicidio
cdmx_homicidios <-  
  cdmx %>% 
  group_by(ano, clave_ent, cve_municipio, municipio) %>% 
  summarise(total_delito = sum(total_delito, na.rm=TRUE)) %>% 
  ungroup()

# Le cambio los nombres a las varoables
nombres<- c("cve_ent", "cve_municipio", "municipio", "ano", "pob")
names(pob_total)<- nombres

# Juntar homicidios y población de la cdmx por año
cdmx_homicidios<- cdmx_homicidios  %>% 
  left_join(pob_total, by=c("cve_municipio", "municipio" , "ano", "clave_ent" = "cve_ent") )

# Calcular homicidios dolosos por cien mil habitantes
cdmx_homicidios$tasa_homicidios <- (cdmx_homicidios$total_delito/cdmx_homicidios$pob) * 100000

####### Hacer mapa de tasa de homicidios municipal para 2019

## Directorio
setwd("/Users/monlopez/Documents/MON/Programming/Projects/homicidios_cdmx/data/cdmx_shapefile")

# Ciudad de México homicidios 2019
cdmx_homi_2019 <- filter(cdmx_homicidios, ano=="2019")

## Cargar el shapefile
cdmx_shape<- readOGR("cdmx.shp")

# Fortify (GgPlot2)
cdmx_muni <- fortify(cdmx_shape, region="CVEGEO") 
cdmx_muni$id<-as.numeric(cdmx_muni$id)

# Juntar base "fortificada" (shapefile) y la tasa de homicidios en la cdmx
junta <- inner_join(cdmx_muni, cdmx_homi_2019, by= c("id" = "cve_municipio"))

# Aggregar datos
cnames <- aggregate(cbind(long, lat) ~ municipio.x, data=junta, FUN=function(x) mean(range(x)))
cnames$municipio.x = gsub("Cuajimalpa de Morelos", "Cuajimalpa", cnames$municipio.x)

# Mapa de tasa de homicidios dolosos por cien mil habitantes
ggplot(junta) +
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=tasa_homicidios))+
  geom_path(data = junta, aes(x = long, 
                              y = lat, 
                              group = group), 
            color = "white") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar", name = "Tasa") +
  coord_equal() +
  theme_void()+
  theme(legend.position = "right") +
  geom_text(data=cnames, aes(long, lat, label = municipio.x), size=3, fontface="bold") +
  labs(x = NULL, 
       y = NULL, 
       title = "Tasa por cien mil habitantes de homicidios dolosos", size=35, hjust = 0.5, 
       subtitle = "Ciudad de México (2015-2019)", size=25, hjust = 0.5, 
       caption = "Fuente: SESNSP") +
  theme(title  = element_text(face="bold", size=14))

ggsave("homicidios_cdmx.png", width = 12, height = 10)

#### Comparación temporal entre alcaldías de la tasa de homicidios dolosos

# Graficar tasa de homicidios dolosos por alcaldía
cdmx_homicidios %>%
  filter(ano < 2020,
         cve_municipio < 9998) %>%
  ggplot(aes(x=ano,
             y=tasa_homicidios,
             group=municipio)) +
  geom_point(aes(colour=tasa_homicidios,
                 alpha=0.5)) +
  geom_line(aes(colour=tasa_homicidios,
                alpha=0.2)) +
  scale_fill_manual(values= "#a50f15") +
  theme_minimal() +
  theme(legend.position = "none")+
  xlab("Año") +
  ylab("Tasa de Homicidios") +
  labs(title="Tasa de homicidios dolosos por Alcaldía",
       subtitle="Ciudad de México (2015-2019)", x = "", 
       y = "", caption = "Fuente: SESNSP") +
  theme(title  = element_text(face="bold", size=14)) +
  facet_wrap(~municipio)

ggsave("delegacion.png", width = 11, height = 8)

# Guardar tabla de tasa de homicidios por alcaldía y año
write.csv(cdmx_homicidios, paste(out, "tasa_homicidios.csv", sep="/"), row.names = F, fileEncoding ="UTF-8")

#### Comparación de la tasa de homicidios CDMX con la nacional

### Calculo tasas de homicidios dolosos y feminicidios anuales para la cdmx

# Agrupar por año, municipio y subtipo de delito
cdmx_homicidios <-  
  cdmx %>% 
  group_by(ano) %>% 
  summarise(total_delito = sum(total_delito, na.rm=TRUE)) %>% 
  ungroup()

# Filtro para la cdmx
pob_cdmx <- pob %>% 
  filter(clave_ent == 9) %>% 
  group_by(ano) %>% 
  summarise(total_poblacion = sum(pob, na.rm=TRUE)) %>% 
  ungroup()

#### Calcular homicidios dolosos nacionales

# Filtro por delito y año
homicidios_nacional <- inci_deli %>% 
  filter(subtipo_de_delito == "Homicidio doloso" | subtipo_de_delito == "Feminicidio",
         ano != 2020) 

# Agrupar homicidios nacionales por año
homicidios_nacional <-  
  homicidios_nacional %>% 
  group_by(ano) %>% 
  summarise(total_delito = sum(total_delito, na.rm=TRUE)) %>% 
  ungroup()

# Agrupar población por año
poblacion_nacional <-  
  pob %>% 
  group_by(ano) %>% 
  summarise(total_poblacion = sum(pob, na.rm=TRUE)) %>% 
  ungroup()

# Juntar homicidios y población de la cdmx por año
junta_nacional <- homicidios_nacional  %>% 
  left_join(poblacion_nacional, by=c("ano") )

# Calcular homicidios dolosos por cien mil habitantes
junta_nacional = mutate(junta_nacional, tasa_homicidios_nacional = round(total_delito / total_poblacion * 100000, digits =1))

# Juntar nacional y cdmx
junta <- junta_nacional %>% 
  left_join(junta_cdmx, by=c("ano") )

# Seleccionar variables de interés 
junta2 <- select(junta, ano,  tasa_homicidios_nacional, tasa_homicidios_cdmx)

# Le cambio los nombres
nombres <- c("ano","Nacional","Ciudad de México")
names(junta2) <- nombres

# Reshape
md <- melt(junta2, id= "ano")

# Graficar
ggplot(md) +
  geom_line(aes(x=ano, y=value, color=variable)) +
  geom_point(aes(x=ano, y=value), color="black", size=2, shape=16) +
  geom_text_repel(aes(ano, value, label = value), force=1, 
                  vjust=0,
                  direction='y',
                  nudge_x=0.,
                  segment.size=0.5) +
  scale_color_manual(values=c("#a50f15","#225ea8")) +
  labs(title ="Tasa de homicidios dolosos totales", 
       subtitle="Nacional vs Ciudad de México (2015-2019)", x = "", 
       y = "", caption = "Fuente: SESNSP", color ="Tasa") +
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size = 10),
        title  = element_text(face="bold", size=14))
ggsave("comparacion.png")



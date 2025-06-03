####################################
#########  SETEO BASICO  ###########
############  BY LF  ###############
####################################


#######################
###  SETEO GENERAL  ###
#######################


#INFO DEL USUARIO
USER=Sys.info()
USER=USER[7]

#LIBRERIA
# LIBRERIA=c(paste("C:/Users/",USER,"/Google Drive/4) R/3) Library",sep = ""))
# LIBRERIA=c(paste("G:/Mi unidad/4) R/3) Library",sep = ""))
# .libPaths(LIBRERIA)
# .libPaths(.libPaths()[1])


#DEFINIR LOS SUBPAQUETES
pack_textos = c("shadowtext", "mgsub")
pack_graficos = c("ggridges", "fmsb", 
                  # "networkD3",  
                  # "gganimate", "webshot", "ggblanket",
                  "formattable", "packcircles")
pack_mapas = c("mapproj", "sf")
pack_dashboards = c("plotly", "flexdashboard")
pack_tidy = c("tidyverse")
pack_datas = c("xts", "zoo", "safer", "reshape2", "lubridate", "scales", 
               "readxl", "data.table", "openxlsx", "WriteXLS" )  
# pack_conexion = c("Rserve", "rsconnect", "foreign", "gargle")
pack_analisis = c("plm", "lmtest", "forecast", "vars")


#UNIR SUBPAQUETES
pqt<- c(#pack_textos,
  pack_graficos, 
  #pack_mapas,
  #pack_dashboards,
  pack_tidy,
  pack_datas,
  # pack_conexion,
  pack_analisis)


#ACTIVAR LOS PAQUETES SIGUIENTES
library(easypackages)
libraries(pqt)



#install.packages(pqt, lib=LIBRERIA)

######################################
###########  COLORES_OEFA  ###########
######################################

OEFA.AZUL1<-c("#144AA7")
OEFA.AZUL2<-c("#1d85bf")
OEFA.TURQUEZA<-c("#0BC7E0")
OEFA.JADE<-c("#44bfb5")
OEFA.VERDE<-c("#8CCD3A")
OEFA.GRIS<-c("#696A6A")
OEFA.MOSTAZA<-c("#FFB500")

OEFA.HUMO<-c("#BCBCBC")
OEFA.LIMON<-c("#D5CB00")
OEFA.MORADO<-c("#4F4898")
OEFA.FUCSIA<-c("#E83670")
OEFA.NARANJA<-c("#EF7911")
OEFA.ROJO<-c("#EF0C33")

PALETA.PRINCIPAL<-c(OEFA.AZUL1,OEFA.AZUL2,OEFA.TURQUEZA,OEFA.JADE,OEFA.VERDE,OEFA.GRIS,OEFA.MOSTAZA)
PALETA.SECUNDARIA<-c(OEFA.HUMO,OEFA.LIMON,OEFA.MORADO,OEFA.FUCSIA,OEFA.NARANJA,OEFA.ROJO)




rm(pack_textos,
   pack_graficos, 
   pack_mapas,
   pack_dashboards,
   pack_tidy,
   pack_datas,
   pack_conexion,
   pack_analisis,
   pqt, LIBRERIA,
   OEFA.AZUL1,
   OEFA.AZUL2,
   OEFA.TURQUEZA,
   OEFA.JADE,
   OEFA.VERDE,
   OEFA.GRIS,
   OEFA.MOSTAZA,
   OEFA.HUMO,
   OEFA.LIMON,
   OEFA.MORADO,
   OEFA.FUCSIA,
   OEFA.NARANJA,
   OEFA.ROJO)

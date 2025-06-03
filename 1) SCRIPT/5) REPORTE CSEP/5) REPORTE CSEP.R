######################################################
######################################################
######################################################


########################## ###
##    PARAMETROS Y SETEO   ####
########################## ###

rm(list=ls())
source('1) SCRIPT/1) BASE_LF.R')
library("kableExtra")
# library("emayili")
# library("keyring")
# library("gmailr")

RES=250
ALTO=1400 #1000
ANCHO=2600 #1200
ESCAL_CONV=0.026458333

# RUTA_STOCK = "2) BDs/4) REPORTES/1) INAF/1) STOCK/"
# RUTA_DOCUMENTOS = "2) BDs/4) REPORTES/1) INAF/2) DOCUMENTOS/"


#Funciones creadas: Eliminar tilder y caracteres raros
DEL_TILD <- function( s ) {
  chartr("áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
         "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
         s );
}


#Funciones creadas: Crear calendario
FERIADOS = c("2023-01-01", "2023-01-02", "2023-04-06", "2023-04-07", "2023-05-01",
             "2023-06-29", "2023-07-23", "2023-07-28", "2023-07-29", "2023-08-06", 
             "2023-08-30", "2023-10-08", "2023-11-01", "2023-12-07", "2023-12-08", 
             "2023-12-09", "2023-12-25", "2023-12-26"
             
)

CALENDARIO = bizdays::create.calendar("CAL_2023", 
                                    weekdays = c("saturday", "sunday"),
                                    holidays = FERIADOS)


#Función para el calculo del tamaño de muestra por proporciones
T_MUESTRA_P = function(N, p , conf, e) {
  ceiling((N*p*(1-p))/((N-1)*(e^2)/(qnorm(1-(1-conf)/2,0,1)^2)+p*(1-p)))
}

#Parámetros estandar para proporciones
P = 0.5           # % de éxito
CONF = 0.95       # Nivel de confianza
E = 0.05          # Error máximo permitible
T_NR = 0.0       # Tasa de no respuesta



##############################


######################## ###
##    PARAMETROS DRIVE   ####
######################## ###

#Actualizar los accesos
library(googlesheets4)
googlesheets4::gs4_auth(email = "analisisdedatos-dfai@oefa.gob.pe")


############################


###################### ###
##    CARGA DE DATOS   ####
###################### ###

# Seteo
URL <- "https://docs.google.com/spreadsheets/d/1LP94c7V8pxB55C4zJcxVfOFnuJwohRL2WBrqx8z2vjQ/edit?usp=sharing"
INPUT <- "PAS"
OUTPUT <- "2"


# Read the data
BD_INPUT <- read_sheet(URL, INPUT) 


BD_STOCK_EXP = BD_INPUT %>% 
  rename(ASIG = 3,
         EXP = 6,
         ADM = 7,
         UF = 8,
         SECT = 9,
         F_INI_SUP = 10,
         AÑO_SUP = 11,
         F_RECEP = 12,
         COMP = 13,
         F_COMIS = 14,
         F_PRESC = 15,
         F_EMIS_INICIO = 16,
         F_NOT_INICIO = 17,
         CADUC = 18,
         CONFLIC = 19,
         DPTO_ORIG = 24,
         PROV_ORIG = 25,
         DIST_ORIG = 26,
         PROG = 27,
         PRODUCT = 28) %>% 
  mutate(F_INI_SUP_2 = as.Date(F_INI_SUP),
         F_RECEP_2 = as.Date(F_RECEP),
         F_INI_AUX = case_when(is.na(F_RECEP_2) == F ~ F_RECEP_2,
                               is.na(F_INI_SUP_2) == F ~ F_INI_SUP_2,
                               T ~ NA))



##########################


########################## ###
##    CREACION DE TABLAS   ####
########################## ###



##############################






















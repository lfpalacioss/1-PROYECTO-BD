######################################################
######################################################
######################################################


########################## ###
##    PARAMETROS Y SETEO   ####
########################## ###

rm(list=ls())
source('1) SCRIPT/1) BASE_LF.R')
# library("kableExtra")
# library("emayili")
# library("keyring")
# library("gmailr")

RES=250
ALTO=1400 #1000
ANCHO=2600 #1200
ESCAL_CONV=0.026458333

RUTA_RUIAS = "2) BDs/3) RUIAS/"

#Funciones creadas: Eliminar tilder y caracteres raros
DEL_TILD <- function(s) {
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


################# ###
##    LEER DATA   ####
################# ###

BD_RUIAS = read_xlsx(path = paste0(RUTA_RUIAS,"Sustento - Tablas dinámicas 1, 2, 3 y 4.xlsx"), 
                     sheet = "Sustento Tabla 1 y 2", 
                     skip = 2,
                     col_types = c(rep("text",132))
                     ) %>%
  subset(select = c(1:60))
  #ACA ARREGLAR DPTO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # subset(select = c(5,9,2,15,12,16)) %>% 
  # unique()

#Tamaño de muestra
# TAM_MUESTRA = floor(T_MUESTRA_P(nrow(BD_STOCK), P, CONF, E)/(1-T_NR))



#####################


#################### ###
##   TRABAJAR DATA   ####
#################### ###

# A nivel de infracción
TAB_RUIAS_INF = BD_RUIAS %>% 
  subset(SECT == "HIDROCARBUROS MENORES") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  group_by(DPTO, EXP, NUM) %>% 
  summarise(MONT_INFRAC = mean(MULT_FIN_INFRAC, na.rm = T)) %>% 
  subset(MONT_INFRAC > 0) 

ESTAD = TAB_RUIAS_INF %>% 
  group_by() %>% 
  summarise("Promedio" = mean(MONT_INFRAC, na.rm = T),
            "Mediana" = quantile(MONT_INFRAC, 0.5),
            "Percentil 75" = quantile(MONT_INFRAC, 0.75),
            "Percentil 90" = quantile(MONT_INFRAC, 0.90),
            "Percentil 95" = quantile(MONT_INFRAC, 0.95),
            "Percentil 99" = quantile(MONT_INFRAC, 0.99)) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()


G_1 = TAB_RUIAS_INF %>% 
  ggplot(aes(x = MONT_INFRAC)) +
  geom_histogram(binwidth = 5, fill = "blue") + 
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw()
  
G_1


G_1_REG = TAB_RUIAS_INF %>% 
  subset(DPTO == "Cusco" | DPTO == "Lima" | DPTO == "Moquegua" ) %>% 
  ggplot(aes(x = MONT_INFRAC)) +
  geom_histogram(binwidth = 5, fill = "blue") + 
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() +
  facet_grid(DPTO ~ ., switch = "y")

G_1_REG


# Por tipo de infracción
TAB_1 = BD_RUIAS %>% 
  subset(SECT == "HIDROCARBUROS MENORES") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  # subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(DET_INF_C19) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T)) %>% 
  arrange(desc(MONT_INFRAC_MEDIA))

TAB_1


G_2 = BD_RUIAS %>% 
  subset(SECT == "HIDROCARBUROS MENORES") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(DET_INF_C19 == "Abandono o remediación ambiental" | DET_INF_C19 == "Incumplimientos formales" | DET_INF_C19 == "Incumplimiento de IGA y/o normas ambientales") %>%
  
  ggplot(aes(x = MULT_FIN_INFRAC)) +
  geom_histogram(binwidth = 5, fill = "blue") + 
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_grid(DET_INF_C19 ~ ., switch = "y")

G_2



# Por region
TAB_2 = BD_RUIAS %>% 
  subset(SECT == "HIDROCARBUROS MENORES") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  # subset(DPTO == "Lambayeque" | DPTO == "Lima" | DPTO == "Puno" | DPTO == "Loreto" | DPTO == "Ayacucho" ) %>% 
  
  # subset(P4 == "TRUE") %>% 
  # subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(DPTO) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_P50 = quantile(MULT_FIN_INFRAC, 0.5, na.rm = T),
            MONT_INFRAC_P90 = quantile(MULT_FIN_INFRAC, 0.90, na.rm = T)) %>% 
  arrange(desc(MONT_INFRAC_Q))

TAB_2



G_3 = BD_RUIAS %>% 
  subset(SECT == "HIDROCARBUROS MENORES") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(P4 == "TRUE") %>% 
  subset(DPTO == "Cusco" | DPTO == "Cajamarca" | DPTO == "La Libertad" ) %>% 
  
  ggplot(aes(x = MULT_FIN_INFRAC)) +
  geom_histogram(binwidth = 5, fill = "blue") + 
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_grid(DPTO ~ ., switch = "y")

G_3



########################

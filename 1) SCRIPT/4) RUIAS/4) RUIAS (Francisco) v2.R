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

BD_RUIAS = read_xlsx(path = paste0(RUTA_RUIAS,"00. RUIAS al XX de XXX de 2024 (LF) 10-03-25.xlsx"), 
                     sheet = "RUIAS", 
                     skip = 2,
                     col_types = c(rep("text",157))
                     ) 



#####################


####################### ###
##   TRABAJAR DATA 19   ####
####################### ###

# Por tipo de infracción y subsector (multa en niveles)
TAB_19_NIV_E = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(SECT, DET_INF_C19) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T)) 

TAB_19_NIV_E %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()


# Por tipo de infracción y subsector (multa en niveles)
TAB_19_NIV_G = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(DET_INF_C19) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T)) 

TAB_19_NIV_G %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()





# Por tipo de infracción y subsector (multa en logaritmo)
TAB_19_LOG_E = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  mutate(MULT_FIN_INFRAC = log(MULT_FIN_INFRAC)) %>% 
  group_by(SECT, DET_INF_C19) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MEDIA_LOG = mean(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIANA_LOG = median(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_SD_LOG = sd(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_KURTOSIS_LOG = e1071::kurtosis(MULT_FIN_INFRAC, na.rm = T)) %>% 
  mutate(MONT_INFRAC_MEDIA = exp(MONT_INFRAC_MEDIA_LOG),
         MONT_INFRAC_MEDIANA = exp(MONT_INFRAC_MEDIANA_LOG))

TAB_19_LOG_E



# Por tipo de infracción y subsector (multa en logaritmo)
TAB_19_LOG_G = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  mutate(MULT_FIN_INFRAC = log(MULT_FIN_INFRAC)) %>% 
  group_by(DET_INF_C19) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MEDIA_LOG = mean(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIANA_LOG = median(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_SD_LOG = sd(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_KURTOSIS_LOG = e1071::kurtosis(MULT_FIN_INFRAC, na.rm = T))

TAB_19_LOG_G %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()



#Gráfico de densidad
G_19 = BD_RUIAS %>%
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 7,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19



# #Gráfico de densidad (ACÁ INCLUIR DENTRO DE CADA GRAFICO LA DIFERENCIACION POR CADA SUBSECTOR (GROUP = SECT))
# G_19_X = BD_RUIAS %>%
#   mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>%
#   subset(MULT_FIN_INFRAC > 0) %>%
#   ggplot(aes(color = SECT,
#              fill = SECT,
#              alpha =0.75,
#              adjust = 0.75)) +
#   geom_density(aes(x = log(MULT_FIN_INFRAC)))+
# 
#   labs(title = "Distribución de los montos de sanción (por infracción)",
#        x = "Montos de sanción",
#        y = "Frecuencia") +
#   theme_bw() +
#   facet_wrap(DET_INF_C19 ~ .,
#              ncol = 5,
#              strip.position = "left")+
#   ggeasy::easy_rotate_x_labels(angle = 90,
#                                side = "right")+
#   ggeasy::easy_remove_legend()
# 
# G_19_X




#Gráfico de densidad _Agricultura
G_19_agr = BD_RUIAS %>%
  subset(SECT == "Agricultura") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_agr



#Gráfico de densidad Consultoras
G_19_cons = BD_RUIAS %>%
  subset(SECT == "Consultoras Ambientales") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_cons



#Gráfico de densidad Electricidad
G_19_elect = BD_RUIAS %>%
  subset(SECT == "Electricidad") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_elect



#Gráfico de densidad Hidrocarburos
G_19_hid = BD_RUIAS %>%
  subset(SECT == "Hidrocarburos") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_hid



#Gráfico de densidad Industria
G_19_ind = BD_RUIAS %>%
  subset(SECT == "Industria") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_ind



#Gráfico de densidad Minería
G_19_min = BD_RUIAS %>%
  subset(SECT == "Minería") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_min



#Gráfico de densidad Pesquería
G_19_pesq = BD_RUIAS %>%
  subset(SECT == "Pesquería") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_pesq



#Gráfico de densidad RRSS
G_19_rrss = BD_RUIAS %>%
  subset(SECT == "Residuos Sólidos") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C19 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_19_rrss





###########################


####################### ###
##   TRABAJAR DATA 11   ####
####################### ###

# Por tipo de infracción y subsector (multa en niveles)
TAB_11_NIV_E = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(SECT, DET_INF_C11) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T)) 

TAB_11_NIV_E


# Por tipo de infracción y subsector (multa en niveles)
TAB_11_NIV_G = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  group_by(DET_INF_C11) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MIN = min(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MAX = max(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIA = mean(MULT_FIN_INFRAC, na.rm = T)) 

TAB_11_NIV_G %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()



# Por tipo de infracción y subsector (multa en logaritmo)
TAB_11_LOG_E = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  mutate(MULT_FIN_INFRAC = log(MULT_FIN_INFRAC)) %>% 
  group_by(SECT, DET_INF_C11) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MEDIA_LOG = mean(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIANA_LOG = median(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_SD_LOG = sd(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_KURTOSIS_LOG = e1071::kurtosis(MULT_FIN_INFRAC, na.rm = T))  


TAB_11_LOG_E



# Por tipo de infracción y subsector (multa en logaritmo)
TAB_11_LOG_G = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  mutate(MULT_FIN_INFRAC = log(MULT_FIN_INFRAC)) %>% 
  group_by(DET_INF_C11) %>% 
  summarise(MONT_INFRAC_Q = n(),
            MONT_INFRAC_MEDIA_LOG = mean(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_MEDIANA_LOG = median(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_SD_LOG = sd(MULT_FIN_INFRAC, na.rm = T),
            MONT_INFRAC_KURTOSIS_LOG = e1071::kurtosis(MULT_FIN_INFRAC, na.rm = T))

TAB_11_LOG_G %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()



#Gráfico de densidad
G_11 = BD_RUIAS %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 1)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 5,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11



#Gráfico de densidad _Agricultura
G_11_agr = BD_RUIAS %>%
  subset(SECT == "Agricultura") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_agr



#Gráfico de densidad Consultoras
G_11_cons = BD_RUIAS %>%
  subset(SECT == "Consultoras Ambientales") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_cons



#Gráfico de densidad Electricidad
G_11_elect = BD_RUIAS %>%
  subset(SECT == "Electricidad") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_elect



#Gráfico de densidad Hidrocarburos
G_11_hid = BD_RUIAS %>%
  subset(SECT == "Hidrocarburos") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_hid



#Gráfico de densidad Industria
G_11_ind = BD_RUIAS %>%
  subset(SECT == "Industria") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_ind



#Gráfico de densidad Minería
G_11_min = BD_RUIAS %>%
  subset(SECT == "Minería") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_min



#Gráfico de densidad Pesquería
G_11_pesq = BD_RUIAS %>%
  subset(SECT == "Pesquería") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_pesq



#Gráfico de densidad RRSS
G_11_rrss = BD_RUIAS %>%
  subset(SECT == "Residuos Sólidos") %>% 
  mutate(MULT_FIN_INFRAC = as.numeric(MULT_FIN_INFRAC)) %>% 
  subset(MULT_FIN_INFRAC > 0) %>% 
  ggplot() +
  geom_density(aes(x = log(MULT_FIN_INFRAC)), 
               alpha =0.75, 
               color = "black", 
               fill = "blue",
               adjust = 0.75)+
  
  labs(title = "Distribución de los montos de sanción (por infracción)",
       x = "Montos de sanción",
       y = "Frecuencia") +
  theme_bw() + 
  facet_wrap(DET_INF_C11 ~ .,
             ncol = 4,
             strip.position = "left")+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()

G_11_rrss






###########################





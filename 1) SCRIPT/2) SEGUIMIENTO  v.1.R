######################################################
######################################################
######################################################


########################## ###
##    PARAMETROS Y SETEO   ####
########################## ###

rm(list=ls())
source('1) SCRIPT/1) BASE_LF.R')
library("kableExtra")
library("emayili")
library("keyring")
library("mailR")
library("gmailr")

RES=250
ALTO=1400 #1000
ANCHO=2600 #1200
ESCAL_CONV=0.026458333

RUTA_STOCK = "2) BDs/4) REPORTES/1) INAF/1) STOCK/"
RUTA_DOCUMENTOS = "2) BDs/4) REPORTES/1) INAF/2) DOCUMENTOS/"

#Plazos legales y consensuados
P_RSD_d = 200
P_IFI_m = 5
P_RD_m = 3
P_EXT_m = 3
P_SUSP_d = 25
P_CAD_m = 9
P_PRESC_a = 4

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

CAL_2023 = bizdays::create.calendar("CAL_2023", 
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
##    BD STOCK    ####
################# ###

BD_STOCK = read_xlsx(path = paste0(RUTA_STOCK,"STOCK.xlsx"), 
               sheet = "STOCK",
               skip = 8) %>% 
  rename(NUM = 1,
         JEFE_EQ = 2,
         ANALISTA = 3,
         NUM_INF = 4,
         NUM_EXP = 5,
         ADM = 7,
         UF = 8,
         SECT_SUP = 9,
         SUB_SECT_SUP = 10,
         SUB_SECT_FISC = 11,
         F_INI_SUP = 12,
         AÑO_INI_SUB = 13,
         COMPLEJ = 14,
         F_COMISION = 15,
         F_PRESC = 16,
         NUM_RSD_INI = 17,
         F_RSD_INI = 18,
         F_NOTIF_INI = 19,
         F_CADUC = 20,
         NUM_REG_RSD_INI = 21,
         ESTADO = 22,
         CONFLICTO = 23,
         PRIOR = 24) %>%
  subset(select = c(5,9,2,15,12,16))

#Tamaño de muestra
TAM_MUESTRA = floor(T_MUESTRA_P(nrow(BD_STOCK), P, CONF, E)/(1-T_NR))

#Guardar la data
# openxlsx::write.xlsx(list("1" = BD_STOCK),
#                      file="3) OUTPUT/1) REPORTE_EXP.xlsx")
  


#####################


###################### ###
##    BD DOCUMENTOS    ####
###################### ###
  
BD_DOCS = read_xlsx(path = paste0(RUTA_DOCUMENTOS,"DOCUMENTOS.xlsx"), 
                    sheet = "DOCS",
                    skip = 8,
                    col_types = c(rep("guess",5),
                                  "text", "text",
                                  rep("guess",23))
                    ) %>% 
  rename(NUM = 1,
         NUM_DOC = 2,
         NUM_EXP = 3,
         F_EMI = 4,
         T_DOC = 5,
         SENTIDO = 6,
         F_NOTIF = 7,
         PROCED = 8,
         T_PAS = 9,
         REINCID = 10,
         SUBSECT = 11,
         T_INF = 12,
         NUM_INF_SUP = 13,
         F_INI_SUP = 14,
         COD_ADM = 16,
         ADM = 17,
         COD_UF = 18,
         UF = 19,
         DPTO = 20,
         PROV = 21,
         DIST = 22,
         EQUIPO = 23,
         DOC_JEFE_EQ = 24,
         JEFE_EQ = 25,
         DOC_ANALIST = 26,
         ANALIST = 27,
         F_PRESC = 28,
         OBS = 29,
         F_REG = 30) 

BD_DOCS_X = BD_DOCS %>% 
  mutate(DUPS = duplicated(NUM_INF_SUP),
         F_EMI = as.Date(F_EMI, format = "%d/%m/%Y"),
         F_REG = as.Date(F_REG, format = "%d/%m/%Y")) %>% #Setea los formatos de fecha
  group_by(NUM_INF_SUP) %>%   #Agrupamiento por numero de informe de supervisión
  mutate(DUPLICADOS = if_else( n() > 1, 1,0),   #Cálculo de duplicados
         T_DOC = case_when(T_DOC == "RESOLUCIÓN SUB DIRECTORAL" ~ "RSD",     #Crear tipo de documento
                           T_DOC == "INFORME FINAL DE INSTRUCCIÓN" ~ "IFI",
                           T_DOC == "RESOLUCIÓN DIRECTORAL" ~ "RD",
                           T ~ T_DOC),
         COD_DOC = case_when(T_DOC == "RSD" ~ 1,   #Crear recodificación
                             T_DOC == "IFI" ~ 2,
                             T_DOC == "RD" ~ 3,
                             T ~ 99),
         SELEC_TDOC = case_when(T_DOC == "RSD" ~ 1,   #Crear variable de selección por tipo de documento
                           T_DOC == "IFI" ~ 1,
                           T_DOC == "RD" ~ 1,
                           T ~ 0),
         SELEC_SENT = case_when(T_DOC == "RSD" & (SENTIDO == "INICIO" | SENTIDO == "NO INICIO") ~ 1,   #Crear variable de selección por sentidos
                                T_DOC == "IFI" & (SENTIDO == "ARCHIVO" | SENTIDO == "RESPONSABILIDAD") ~ 1,
                                T_DOC == "RD" & (SENTIDO == "RESPONSABILIDAD ADMINISTRATIVA SIN MEDIDA CORRECTIVA" | SENTIDO == "RESPONSABILIDAD ADMINISTRATIVA CON MEDIDA CORRECTIVA" | SENTIDO == "ARCHIVO")  ~ 1,
                                T ~ 0)) %>% 
  subset(SELEC_TDOC == 1) %>%   #Seleccionar por tipo de documentos
  subset(SELEC_SENT == 1) %>%   #Seleccionar por sentidos
  group_by(NUM_EXP, T_DOC) %>%   #Agrupar por numero de expediente y tipo de documento y nos quedamos con los últimos actuados
  filter(F_EMI == max(F_EMI)) %>% 
  ungroup() %>% 
  
  group_by(NUM_EXP, T_DOC) %>%   #Agrupar por numero de expediente y tipo de documento y nos quedamos con los ultimos actuados emitidos (de acuerdo al código del registro)
  filter(F_REG == max(F_REG)) %>% 
  
  group_by(NUM_EXP) %>%   #Agrupar por número de expediente y calcular la etapa por la media del código de documento de los números más altos
  # mutate(ETAPA = mean(COD_DOC[F_EMI == max(F_EMI)])) %>% 
  mutate(ETAPA = max(COD_DOC[F_REG == max(F_REG)])) %>% 
  
  group_by(NUM_EXP, T_DOC) %>%   #Agrupar por número de expediente y calcular la etapa por la media del código de documento de los números más altos
  filter(NUM == max(NUM)) %>%   #Eliminar los casos extraños
  
  filter(COD_DOC <= ETAPA) %>% 
  ungroup() 


# openxlsx::write.xlsx(list("1" = BD_DOCS_X),
#                      file="3) OUTPUT/2) PRUEBAS1.xlsx")

BD_DOCS_Y = BD_DOCS_X %>% 
  pivot_wider(
    id_cols = c(NUM_EXP, SUBSECT, T_INF, NUM_INF_SUP, RUC, COD_ADM, ADM, UF, COD_UF, ETAPA),
    names_from = T_DOC,
    values_from = c(F_EMI, NUM_DOC,SENTIDO)
  ) 
  
# openxlsx::write.xlsx(list("1" = BD_DOCS_Y),
#                      file="3) OUTPUT/2) REPORTE_DOCS.xlsx")
  



##########################


################### ###
##    CRUCE BD's    ####
################### ###

BD_MERGE = merge(BD_STOCK, 
                 BD_DOCS_Y,
                 by.x = "NUM_EXP",
                 by.y = "NUM_EXP",
                 all.x = TRUE) %>% 
  subset(select = -c(SUBSECT)) %>% 
  mutate(F_COMISION = as.Date(F_COMISION, format = "%d/%m/%Y"),
         F_INI_SUP = as.Date(F_INI_SUP, format = "%d/%m/%Y"),
         F_PRESC = as.Date(F_PRESC, format = "%d/%m/%Y")) %>% 
  
  mutate(F_LIM_RSD = F_COMISION + days(P_RSD_d),
         F_LIM_IFI = F_EMI_RSD + months(P_IFI_m),
         F_LIM_RD = F_EMI_IFI + months(P_RD_m),
         
         F_CAD_RD = F_EMI_RSD + months(P_CAD_m),
         F_PRESC_RD = F_COMISION + year(P_PRESC_a),
         
         PROG_RSD = floor_date(F_LIM_RSD - months(1),"month"),
         PROG_IFI = floor_date(F_LIM_IFI - months(1),"month"),
         PROG_RD = floor_date(F_LIM_RD - months(1),"month")) %>% 
  
  mutate(NUM_DOC_RSD = str_remove(NUM_DOC_RSD, "RESOLUCIÓN\\sN°\\s"),
         NUM_DOC_IFI = str_remove(NUM_DOC_IFI, "INFORME\\sN°\\s"),
         NUM_DOC_RD = str_remove(NUM_DOC_RD, "RESOLUCIÓN\\sN°\\s")) %>% 
  
  mutate(ESTADO_RSD = case_when(is.na(F_EMI_RSD) == F ~ "ANTENDIDO",
                                T ~ "PENDIENTE"),
         ESTADO_IFI = if_else(is.na(F_EMI_RSD) == T,
                              NA,
                              case_when(SENTIDO_RSD == "NO INICIO" ~ "ARCHIVADO",
                                        SENTIDO_RSD == "INICIO" & is.na(F_EMI_IFI) == F ~ "ATENDIDO",
                                        SENTIDO_RSD == "INICIO" & is.na(F_EMI_IFI) == T ~ "PENDIENTE",
                                        T ~ "XXXXXXXX")),
         ESTADO_RD = if_else(is.na(F_EMI_RSD) == T | is.na(F_EMI_IFI) == T,
                              NA,
                              case_when(SENTIDO_RSD == "NO INICIO" ~ "ARCHIVADO",
                                        SENTIDO_RSD == "INICIO" & is.na(F_EMI_RD) == F ~ "ATENDIDO",
                                        SENTIDO_RSD == "INICIO" & is.na(F_EMI_RD) == T ~ "PENDIENTE",
                                        T ~ "XXXXXXXX")),
         F_NOT_RSD = NA,
         F_NOT_IFI = NA,
         F_NOT_RD = NA,
         EXT_PLAZO = NA,
         SUSP_PLAZO = NA) %>% 
  mutate(ETAPA = if_else(is.na(ETAPA) == T,
                         0,
                         ETAPA)) %>% 
  subset(select = c(1:6,  #Info general
                    24,29,19,16,35,32,22,  #Info RSD
                    25,30,18,15,36,33,  #Info IFI
                    38,39,27,28,26,31,20,17,37,34,   #Info RD
                    14)) %>%   #Etapa
  mutate(AUX = runif(n(),0,1),
         SELECCION = ifelse(rank(desc(AUX), ties.method = "random") <= TAM_MUESTRA, 
                            "seleccionado", 
                            NA),
         REVISION = NA,
         OBS = NA)


  

#######################


######################## ###
##    GUARDAR PRUEBAS    ####
######################## ###



# openxlsx::write.xlsx(list("1.1 STOCK" = BD_STOCK,
#                           "1.2 DOCUMENTOS" = BD_DOCS,
#                           "1.3 DOCUMENTOS" = BD_DOCS_X),
#                      file="3) OUTPUT/1) PRUEBAS2.xlsx")



############################


# ################### ###
# ##    SUBIR DATA    ####
# ################### ###
# 
# URL <- "https://docs.google.com/spreadsheets/d/1SYGEtKL2LtN_QeN4WkErb1kO5icRWcTUnexDvH0Hx-M/edit?usp=sharing"
# # INPUT <- "1"
# OUTPUT <- "X) PROPUESTA"
# 
# 
# googlesheets4::write_sheet(BD_MERGE,
#                            ss = URL,
#                            sheet = OUTPUT)
# 
# 
# 
# #######################


#################### ###
##    INDICADORES    ####
#################### ###

# Ingresos de casos
TAB_1 = BD_MERGE %>% 
  mutate(PERIODO_PRESC = as.yearqtr(F_PRESC),
         PERIODO_INF_SUP = as.yearqtr(F_INI_SUP)) %>% 
  group_by(PERIODO_INF_SUP, SECT_SUP) %>% 
  summarise(Q_EXP = n())

G_1 = TAB_1 %>% 
  subset(PERIODO_INF_SUP >= "2018 Q1") %>% 
  mutate(SECT_SUP = if_else(SECT_SUP == "AGRICULTURA,CONSULTORAS AMBIENTALES,ELECTRICIDAD,HIDROCARBUROS,INDUSTRIA,MINERÍA,PESQUERÍA,RESIDUOS SÓLIDOS",
                            "OTROS (AGRUPADOS)",
                            SECT_SUP)) %>%
  
  ggplot(aes(x = PERIODO_INF_SUP, 
             y = Q_EXP)) +
  geom_line() +
  labs(title = "Informes de supervisión, por subsector y trimestre",
       x = "Fecha de inicio de la supervisión",
       y = "Cantidad de informes",
       caption = "Desde el primer trimestre de 2018\nNota: Las categorías NA y OTROS (AGRUPADOS) pudieran deberse a errores en el registro")+
  scale_x_yearqtr(format = "%Y q%q") +
  theme_bw()+
  facet_wrap(~ SECT_SUP)+
  ggeasy::easy_rotate_x_labels(angle = 90)


  ggsave("3) OUTPUT//3) GRAFICOS//G_1.jpg", width = 6, height = 4)



# Estado de los casos
TAB_2 = BD_MERGE %>% 
  mutate(PERIODO_PRESC = year(F_PRESC),
         PERIODO_INF_SUP = year(F_INI_SUP)) %>% 
  subset(PERIODO_PRESC<2025) %>% 
  # group_by(PERIODO_INF_SUP, SECT_SUP) %>% 
  group_by(SECT_SUP) %>% 
  summarise(Q_EXP = n(),
            Q_PREL = sum(ETAPA == 0),
            Q_RSD = sum(ETAPA == 1),  #Ajustar por sentido
            Q_IFI = sum(ETAPA == 2),
            Q_RD = sum(ETAPA == 3),
            PEND_RSD = Q_PREL,
            PEND_IFI = sum(ETAPA == 1 & SENTIDO_RSD == "INICIO"),
            PEND_RD = Q_IFI,
            TASA_PEND_RSD = PEND_RSD/Q_EXP,
            TASA_PEND_IFI = PEND_IFI/Q_EXP,
            TASA_PEND_RD = PEND_RD/Q_EXP,
            TASA_PEND_TOT = (PEND_RSD+PEND_IFI+PEND_RD)/Q_EXP)


# Tabla de emisiones
TAB_2.1 = TAB_2 %>%
  subset(select = -c(7:13)) %>%
  mutate(SECT_SUP = if_else(SECT_SUP == "AGRICULTURA,CONSULTORAS AMBIENTALES,ELECTRICIDAD,HIDROCARBUROS,INDUSTRIA,MINERÍA,PESQUERÍA,RESIDUOS SÓLIDOS",
                            "OTROS (AGRUPADOS)",
                            SECT_SUP)) %>% 
  mutate(SECT_SUP = str_to_title(SECT_SUP)) %>% 
  kableExtra::kable(col.names = c("Subsector", 
                                  "Cantidad de expedientes", 
                                  "En análisis de inicio",
                                  "RSD emitidos",
                                  "IFI emitidos",
                                  "RD emitidos"),
                    caption = "Emisión de documentos") %>% 
  kableExtra::footnote(number_title= "Consideraciones",
                       number = c("Fuente: Reporte de Stock y Documentos (INAF)",
                                  "Fecha de corte: 14/03/2025",
                                  "Expedientes con fecha de prescipción hasta el 31/12/2025"),
                       title_format = c("italic", "bold")) %>% 
  
kableExtra::kable_styling() %>% 
  save_kable(file = "3) OUTPUT//2) TABLAS//TAB_2.1.html",
             density = 150)

# Tabla de avances
TAB_2.2 = TAB_2 %>%
  subset(select = -c(3:6)) %>%
  mutate(SECT_SUP = if_else(SECT_SUP == "AGRICULTURA,CONSULTORAS AMBIENTALES,ELECTRICIDAD,HIDROCARBUROS,INDUSTRIA,MINERÍA,PESQUERÍA,RESIDUOS SÓLIDOS",
                            "OTROS (AGRUPADOS)",
                            SECT_SUP)) %>% 
  mutate(SECT_SUP = str_to_title(SECT_SUP),
         TASA_PEND_RSD = scales::percent(TASA_PEND_RSD, accuracy = .1),
         TASA_PEND_IFI = scales::percent(TASA_PEND_IFI, accuracy = .1),
         TASA_PEND_RD = scales::percent(TASA_PEND_RD, accuracy = .1),
         TASA_PEND_TOT = scales::percent(TASA_PEND_TOT, accuracy = .1)) %>% 
  kableExtra::kable(col.names = c("Subsector", 
                                  "Cantidad de expedientes", 
                                  "RSD pendientes",
                                  "IFI pendientes",
                                  "RD pendientes",
                                  "Tasa de RSD pendientes",
                                  "Tasa de IFI pendientes",
                                  "Tasa de RD pendientes",
                                  "Tasa de actos pendientes"),
                    caption = "Emisión de documentos") %>% 
  kableExtra::column_spec(6, 
                          color = "white",
                          background = case_when(TAB_2$TASA_PEND_RSD <= 0.05 ~ "blue",
                                                    TAB_2$TASA_PEND_RSD > 0.05 & TAB_2$TASA_PEND_RSD <= 0.10 ~ "red",
                                                    T ~ "black")) %>% 
  kableExtra::column_spec(7, 
                          color = "white",
                          background = case_when(TAB_2$TASA_PEND_IFI <= 0.05 ~ "blue",
                                                 TAB_2$TASA_PEND_IFI > 0.05 & TAB_2$TASA_PEND_IFI <= 0.10 ~ "red",
                                                 T ~ "black")) %>% 
  kableExtra::column_spec(8, 
                          color = "white",
                          background = case_when(TAB_2$TASA_PEND_RD <= 0.05 ~ "blue",
                                                 TAB_2$TASA_PEND_RD > 0.05 & TAB_2$TASA_PEND_RD <= 0.10 ~ "red",
                                                 T ~ "black")) %>% 
  kableExtra::column_spec(9, 
                          color = "white",
                          background = case_when(TAB_2$TASA_PEND_TOT <= 0.05 ~ "blue",
                                                 TAB_2$TASA_PEND_TOT > 0.05 & TAB_2$TASA_PEND_TOT <= 0.10 ~ "red",
                                                 T ~ "black")) %>% 
  
  kableExtra::footnote(number_title= "Consideraciones",
                       number = c("Fuente: Reporte de Stock y Documentos (INAF)",
                                    "Fecha de corte: 14/03/2025",
                                    "Expedientes con fecha de prescipción hasta el 31/12/2025"),
                       # general_title = "Consideraciones", 
                       # general = c("Expedientes con fecha de prescipción hasta el 31/12/2025"),
                       alphabet_title= "Leyenda",
                       alphabet = c("Azul: Menos de 5%", "Rojo: Entre 5 y 10%", "Negro: Más de 10%"),
                       title_format = c("italic", "bold")) %>% 
  
  kableExtra::kable_styling() %>% 
  save_kable(file = "3) OUTPUT//2) TABLAS//TAB_2.2.html",
             density = 150)


  

########################


######################### ###
##    ENVIO DE CORREOS    ####
######################### ###

#Cargar destinatarios
DESTINATARIOS = list(DFAI = list("analisisdedatos-dfai@oefa.gob.pe", 
                                 "lfpalacioss@outlook.com",
                                 "cadoigrgmail.com"),
                     SFIS = list("lfpalacioss@hotmail.com"),
                     SFEM = list("lfpalacioss@pucp.edu.pe"),
                     SFAP = list("lfpalacioss@uni.pe"))
# DESTINATARIOS_DFAI = list(CORREOS = c("analisisdedatos-dfai@oefa.gob.pe", 
#                                  "lfpalacioss@outlook.com"))
# DESTINATARIOS_DFAI$CORREOS


#Cargar contenido de los correos
CUERPO_REPORTE = rmarkdown::render("3) OUTPUT//1) CORREO//Correo-Reporte.Rmd",
                                # output_dir = "3) Html (Output)",
                                output_file = "Correo-Reporte.html")


# #Generando el correo con el cuerpo ya definido
# email_REPORTE = envelope(
#   to = DESTINATARIOS$DFAI,
#   from = "analisisdedatos-dfai@oefa.gob.pe",
#   subject = "CORREO DE PRUEBA") %>%
#   render(CUERPO_REPORTE)

# # Conectando servidor
# smtp <- server(
#   host = "smtp.gmail.com",
#   # host = "smtp.oefa.gob.pe",
#   port = 587,
#   # port = 465,
#   username = "analisisdedatos-dfai@oefa.gob.pe",
#   # password = "etuv fkte govm tquv",
#   # password = "20092176hH",
#   keyring::key_get("GOOGLE_APP", keyring = "GOOGLE_APP_PASS"),
#   tls = TRUE
# )
# 
# 
# 
# my_gmail_connection = gmail(
#   username = "analisisdedatos-dfai@oefa.gob.pe",
#   password = "skva zysz hrju khaw"
# )
# 
# 
# my_gmail_connection = gmail(
#   username = "analisisdedatos-dfai@oefa.gob.pe",
#   password = "20092176Hh"
# )
# 
# 
# #Enviando correo
# smtp(email_REPORTE, verbose = TRUE)
# 
# # my_gmail_connection(email_REPORTE)
# 
# 
# 
# library(rsconnect)
# library(Rserve)
# library(foreign)










# library(blastula)
# 
# # Configura las credenciales de Gmail
# gmail_user <- "analisisdedatos-dfai@oefa.gob.pe" # Reemplaza con tu correo
# gmail_password <- "jpub ului puus ajnb" # Reemplaza con la contraseña de aplicación
# 
# # Renderizar el R Markdown a HTML
# # rmarkdown::render("email_body.Rmd", output_file = "email_body.html")
# 
# # Leer el contenido del archivo HTML
# email_body_html <- readLines("3) OUTPUT//1) CORREO//Correo-Reporte.html") |> paste(collapse = "\n")
# 
# # Crear el correo electrónico
# email <- compose_email(
#   body = md(email_body_html) # Usar md() para incluir el HTML como Markdown
# )
# 
# # Configurar las credenciales de Gmail
# CREDENCIALES <- create_smtp_creds_key(
#   id = "gmail_creds",
#   user = "analisisdedatos-dfai@oefa.gob.pe", # Reemplaza con tu correo
#   provider = "gmail"
# )
# 
# # Enviar el correo
# email |>
#   smtp_send(
#     to = "analisisdedatos-dfai@oefa.gob.pe", # Reemplaza con el destinatario
#     from = "analisisdedatos-dfai@oefa.gob.pe", # Reemplaza con tu correo
#     subject = "Correo generado desde R con blastula",
#     credentials = creds_key("gmail_creds")
#   )















# 
# 
# # Create a new keyring or use the default one
# # keyring_name <- "my_keyring"
# keyring::keyring_create("GOOGLE_APP", keyring = "GOOGLE_APP_PASS")
# 
# # Set the password for the keyring
# keyring::key_set("GOOGLE_APP", keyring = "GOOGLE_APP_PASS")
# 
# # recuperar the password from the keyring
# password <- keyring::key_get("GOOGLE_APP", keyring = "GOOGLE_APP_PASS")
# 
# # Print the password
# print(password)
# 
# # keyring::keyring_delete("GOOGLE_APP_PASS")











path_old <- "~/Downloads/client_secret_835842225840-p2jabpue2al2gjjimg5for7apbskgsm3.apps.googleusercontent.com.json"
d <- fs::dir_create(rappdirs::user_data_dir("gmailr"), recurse = TRUE)
fs::file_move(path_old, d)

rappdirs::user_data_dir("gmailr") %>% 
  list.files()


gm_auth_configure()
gm_oauth_client()
gm_auth("analisisdedatos-dfai@oefa.gob.pe")
# gm_token_write(
#   path = ".secrets/gmailr-token.rds",
#   key = "SUPER_SECRET_ENCRYPTION_KEY"
# )



# Path to your HTML file
# html_file_path <- "path/to/your/email.html" # Replace with your HTML file path

# Read the HTML file
# html_body <- readLines(html_file_path) %>% paste(collapse = "\n")
email_body_html <- readLines("3) OUTPUT//1) CORREO//Correo-Reporte.html") %>%  paste(collapse = "\n")




msg = gm_mime() %>% 
  gm_to("analisisdedatos-dfai@oefa.gob.pe") %>% 
  gm_from("analisisdedatos-dfai@oefa.gob.pe") %>% 
  gm_subject("PRUEBASSSSSSS") %>%
  gm_html_body(email_body_html,
               content_type = "text/html",
               charset = "utf-8",
               encoding = "base64")

gm_send_message(msg)




########################







DESTINAT$





















## OTROS (Para explotar)


  
  
  mutate(MES = ifelse(MES == "Setiembre", "Septiembre", MES),
         HOR_SIN_SIST = as.numeric(HOR_SIN_SIST),
         HOR_TOT = as.numeric(HOR_TOT)) %>% 
  
  
  mutate(MES = case_when(MES == "Enero" ~ 1,
                         MES == "Febrero" ~ 2,
                         MES == "Marzo" ~ 3,
                         MES == "Abril" ~ 4,
                         MES == "Mayo" ~ 5,
                         MES == "Junio" ~ 6,
                         MES == "Julio" ~ 7,
                         MES == "Agosto" ~ 8,
                         MES == "Septiembre" | MES == "Setiembre" ~ 9,
                         MES == "Octubre" ~ 10,
                         MES == "Noviembre" ~ 11,
                         MES == "Diciembre" ~ 12)) %>% 
  group_by(AÑO, MES) %>%
  summarise(HOR_SIN_SIST = sum(HOR_SIN_SIST, na.rm = T),
            HOR_TOT = sum(HOR_TOT, na.rm = T)) %>% 
  mutate(F_AUX = make_date(AÑO, MES, 01),
         CSA = HOR_SIN_SIST/HOR_TOT) %>% 
  mutate(PAND = ifelse(F_AUX >= "2020-03-01" & F_AUX <= "2021-04-01", 1,0))



graf_CSA = DATA_CSA %>% 
  ggplot(aes(x = F_AUX, y = CSA, group = NA))+
  geom_line(aes(color = "CSA"), size = 1, alpha = 0.8)+
  scale_color_manual(values = c("CSA" = PALETA.PRINCIPAL[1]))+
  geom_smooth(span = 0.5, aes(x = F_AUX, y = CSA),colour=PALETA.PRINCIPAL[3], fill = "gray80") +
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_y_continuous(labels=scales::percent_format(accuracy = NULL), limits = c(-0.005,0.075))+
  scale_x_date(labels = date_format("%b %y"), breaks = date_breaks("4 month"))+
  theme_minimal()+
  ggeasy::easy_remove_legend()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")
graf_CSA

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.1) CSA-MKT.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)



graf_CSA_X = DATA_CSA %>% 
  subset(PAND == 0) %>% 
  ggplot(aes(x = F_AUX, y = CSA, group = NA))+
  geom_line(aes(color = "CSA"), size = 1, alpha = 0.8)+
  scale_color_manual(values = c("CSA" = PALETA.PRINCIPAL[1]))+
  geom_smooth(span = 0.5, aes(x = F_AUX, y = CSA),colour=PALETA.PRINCIPAL[3], fill = "gray80") +
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_y_continuous(labels=scales::percent_format(accuracy = NULL), limits = c(-0.005,0.01))+
  scale_x_date(labels = date_format("%b %y"), breaks = date_breaks("2 month"))+
  theme_minimal()+
  ggeasy::easy_remove_legend()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")
graf_CSA_X

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.1) CSA-MKT-PAN.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)





# Densidad
graf_CSA2 = DATA_CSA %>% 
  ggplot()+
  geom_density(aes(x = CSA, alpha =0.75, color = "red", fill = "red"))+
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_x_continuous(labels=scales::percent_format(accuracy = NULL), 
                     limits = c(0,0.005),
                     breaks = seq(0, 0.005, by = 0.0005))+
  theme_minimal()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()


graf_CSA2

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.2) CSA-KER-EMP.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)





#######################


















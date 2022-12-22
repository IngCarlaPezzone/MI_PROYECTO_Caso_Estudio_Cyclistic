### Divvy: Ejercicio de Análisis  ###

# Este análisis se basa en el estudio de caso de Divvy "'Sophisticated, Clear, 
# and Polished': Divvy and Data Visualization" escrito por Kevin Hartman 
# (se encuentra aquí: https://artscience.blog/home/divvy-dataviz-case-study). 
# Los datos usados son los cuatro trimestre de 2019
#
# El objetivo de este guión es consolidar los datos descargados de Divvy en un único 
# marco de datos y, a continuación, realizar un análisis sencillo para ayudar 
# a responder a la pregunta clave: "¿En qué se diferencian los miembros y 
# los usuarios ocasionales en el uso de las bicicletas Divvy?

# # # # # # # # # # # # # # # # # # # # # # # 
# Instalar los paquetes requeridos
# tidyverse para importar y manejar datos
# lubridate para funciones de fecha
# ggplot para visualización
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #ayuda a manejar datos
library(lubridate)  #ayuda a manejar atributos de fecha
library(ggplot2)    #ayuda a visualizar datos
getwd()             #muestra su directorio de trabajo
setwd("E:/Programacion/Curso Data Analitycs Google/Proyecto_Final") # Mi directorio

#=====================
# PASO 1: COLECTAR LOS DATOS
#=====================
# Descargar los datasets Divvy aqui (archivos csv)
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")


#====================================================
# PASO 2: REAGRUPAR DATOS Y COMBINARLOS EN UN ÚNICO ARCHIVO
#====================================================
# Comparar los nombres de las columnas de cada uno de los ficheros
# Aunque los nombres no tienen que estar en el mismo orden, SÍ tienen que 
# coincidir perfectamente antes de que podamos utilizar un comando para unirlos 
# en un solo archivo
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)


# Cambiar el nombre de las columnas para que sean coherentes con q1_2020 
# (ya que este será el supuesto diseño de la tabla para Divvy).

(q2_2019 <- rename(q2_2019
                   ,trip_id           = "01 - Rental Details Rental ID"                   
                   ,start_time        = "01 - Rental Details Local Start Time"            
                   ,end_time          = "01 - Rental Details Local End Time"              
                   ,bikeid            = "01 - Rental Details Bike ID"                     
                   ,tripduration      = "01 - Rental Details Duration In Seconds Uncapped"
                   ,from_station_id   = "03 - Rental Start Station ID"                    
                   ,from_station_name = "03 - Rental Start Station Name"                  
                   ,to_station_id     = "02 - Rental End Station ID"                      
                   ,to_station_name   = "02 - Rental End Station Name"                    
                   ,usertype          = "User Type"                                       
                   ,gender            = "Member Gender"                                   
                   ,birthyear         = "05 - Member Details Member Birthday Year"))

# Inspeccione los marcos de datos y busque incongruencias
str(q1_2019)
str(q2_2019)
str(q3_2019)
str(q4_2019)

# Convertir trip_id y bikeid en caracteres para que puedan apilarse correctamente.
q1_2019 <-  mutate(q1_2019, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)) 
q2_2019 <-  mutate(q2_2019, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)) 
q3_2019 <-  mutate(q3_2019, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)) 
q4_2019 <-  mutate(q4_2019, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)) 

# Juntar el marcos de datos trimestrales individuales en un marco de datos grande
all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)


#======================================================
# PASO 3: LIMPIAR Y AÑADIR DATOS PARA PREPARARLOS PARA EL ANÁLISIS
#======================================================
# Inspeccione la nueva tabla que se ha creado
colnames(all_trips)  #lista de nombres de columnas
nrow(all_trips)  #Cantidad de filas
dim(all_trips)  #Dimensión del data frame
head(all_trips)  #Ver las primeras 6 filas.  También: tail(all_trips)
str(all_trips)  #Ver la lista de columnas y tipos de datos
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Hay algunos problemas que tendremos que solucionar:
# (1) Los datos sólo pueden agregarse a nivel de trayecto, lo cual es demasiado granular.
# Querremos añadir algunas columnas adicionales de datos -como día, mes, año- que 
# proporcionen oportunidades adicionales para agregar los datos.

## Para el incio
all_trips$date <- as.Date(all_trips$start_time) #Formato por defecto yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


## para el fin
all_trips$date_end <- as.Date(all_trips$end_time) #Formato por defecto yyyy-mm-dd
all_trips$month_end <- format(as.Date(all_trips$date), "%m")
all_trips$day_end <- format(as.Date(all_trips$date), "%d")
all_trips$year_end <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week_end <- format(as.Date(all_trips$date), "%A")


# (2) Queremos añadir un campo calculado para la duración del trayecto en minutos
#porque estan en segundos

all_trips$tripduration_min <- with(all_trips, tripduration/60)

# Inspeccionar la estructura de todas las columnas
str(all_trips)

# Convertir from_station_id y to_station_id en caracter

all_trips <-  mutate(all_trips
                     , from_station_id = as.character(from_station_id)
                     , to_station_id   = as.character(to_station_id)) 



#=====================================
# PASO 4: REALIZAR UN ANÁLISIS DESCRIPTIVO
#=====================================
# Análisis descriptivo en tripduration_min 
mean(all_trips$tripduration_min) #Media lineal (longitud total del recorrido / recorrido)
median(all_trips$tripduration_min) #número del punto medio en la matriz ascendente de longitudes de trayecto
max(all_trips$tripduration_min) #trayecto mas largo
min(all_trips$tripduration_min) #trayecto mas corto

# Puede condensar las cuatro líneas anteriores en una sola línea utilizando 
# summary() en el atributo específico
summary(all_trips$tripduration_min)

# Reviso un subdataset con valores de duración de viaje mayor que 24 ha=1440 min
# son 1848 observaciones que superan las 24 hs de uso

all_trips_raros <- subset(all_trips, tripduration_min>1440)


# Nuevo Summary
summary(all_trips_no_outlier$tripduration_min)


# Comparar Miembros con Casuales en cuanto a la duracion del trayecto

aggregate(all_trips$tripduration_min ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$tripduration_min ~ all_trips$usertype, FUN = median)
aggregate(all_trips$tripduration_min ~ all_trips$usertype, FUN = max)
aggregate(all_trips$tripduration_min ~ all_trips$usertype, FUN = min)


# Consulta el tiempo medio de viaje diario de los miembros y los usuarios ocasionales

tiempoMedio_diaSem_Usuario = aggregate(all_trips$tripduration_min ~ all_trips$usertype +
            all_trips$day_of_week, FUN = mean)
tiempoMedio_diaSem_Usuario


# analizar los datos de usuarios por tipo y día de la semana

resumen1 = all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #Crea el campo weekday usando wday()
  group_by(usertype, weekday) %>%                       # agrupa usertype y weekday
  summarise(number_of_rides = n()							          #calcula el número de viaje
            ,average_duration = mean(tripduration_min)) %>% 		# calcula la duración promedio
  arrange(usertype, weekday)								            # ordena
resumen1


# analizar los datos de usuarios por tipo y meses

resumen2 = all_trips %>% 
  mutate(mes = month(start_time, label = TRUE)) %>%  #Crea el campo mes usando month()
  group_by(usertype, mes) %>%                        #agrupa por usertype y mes
  summarise(number_of_rides = n()							       #calcula el número de viajes
            ,average_duration = mean(tripduration_min)) %>% 		# calcula la duración promedio
  arrange(usertype, mes)								             # ordena
resumen2


# Visualicemos el número de viajes por tipo de usuario

all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration_min)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")



# Creemos una visualización de la duración media

all_trips %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration_min)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")



#==================================================================
# PASO 5: EXPORTAR EL ARCHIVO DE RESUMEN PARA SU POSTERIOR ANÁLISIS
#==================================================================
# Crear un archivo csv que vamos a visualizar en Excel, Tableau, o mi software de presentación.
# Nota: Esta ubicación de archivo es para Mac. Si usted está trabajando en un PC, cambie la ubicación del archivo en consecuencia (lo más probable "C:\Users\YOUR_USERNAME\Desktop\...") para exportar los datos. Puede obtener más información aquí: https://datatofish.com/export-dataframe-to-csv-in-r/

# toda la base de datos
write.csv(all_trips, file = 'E:/Programacion/Curso Data Analitycs Google/Proyecto_Final/base_limpia.csv')

# los resumenes 1 y 2 para graficar en excel
write.csv(resumen1, file = 'E:/Programacion/Curso Data Analitycs Google/Proyecto_Final/resumen1.csv')
write.csv(resumen2, file = 'E:/Programacion/Curso Data Analitycs Google/Proyecto_Final/resumen2.csv')



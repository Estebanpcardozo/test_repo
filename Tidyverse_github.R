#Instalación de Pacman
install.packages("pacman")
library("pacman")
p_load(RATest, tidyverse)
#Utilización de tidyverse
library("tidyverse")
library("tidyverse")
sat<- read.csv("https://raw.githubusercontent.com/ignaciomsarmiento/datasets/main/2012_SAT_Results.csv")
demog<- read_csv("https://raw.githubusercontent.com/ignaciomsarmiento/datasets/main/demog.csv")
#Cual es la diferencia entre _csv o .csv?
#cómo lo veo?
str(sat)
str(demog)
# ver la base de datos y filtrar datos
view(demog)

demog_subset<- demog %>% select(DBN, Name, schoolyear, asian_per, black_per, hispanic_per, white_per, starts_with("grade"))

#Quitar duplicados
rm(demog_subset) #quito la base que cree
demog<- demog %>% select(DBN, Name, schoolyear, asian_per, black_per, hispanic_per, white_per, starts_with("grade"))
#Modifiqué la base original y con esto o habrá duplicados


#ahora voy a filtrar por colegios que tengan estudiantes en el grado noveno
demog<- demog %>% filter(grade9!="NA")

#cambiar el nombre
demog<- demog %>% rename(Anoescolar=schoolyear)

#si quier crear una variable:
demog <- demog %>% mutate (perc_total= asian_per+black_per+hispanic_per+white_per)

summary (demog$perc_total)

#Reestructurar los datos
demog_long<- demog %>% 
  filter(Anoescolar=="20112012") %>%
  select(DBN, Name, Anoescolar, asian_per, black_per, hispanic_per,white_per) %>%
  pivot_longer(cols=c(asian_per,black_per, hispanic_per, white_per),
               names_to="Race",
               values_to="Perc")
head(demog_long)

#Agrupando y resumiendo datos
#Otra tarea que solemos realizar mucho es resumir información 
#por grupos. Supongamos que queremos colapsar (resumir) la base 
#anterior (demog_long) en el porcentaje total de las composiciones
#raciales.


demog_long_summary <- demog_long %>% 
  group_by(DBN, Anoescolar,Name) %>% 
  summarize(perc_total=sum(Perc)
    
#UNION DE BASES DE DATOS
# ================================================================================
# 1.El full_join(): es la opción más segura para evitar la eliminación de datos, devuelve todo.

#2.El inner_join(): sólo mantiene lo que es común entre los conjuntos de datos.

#3.El left_join(): une todas las filas de las observaciones que se encuentran en el tibble declarado a la izquierda o en primer lugar.

#4.El right_join(): une todas las filas de las observaciones que se encuentran en el tibble declarado a la derecha o en segundo lugar.

#5.El semi_join(): es como inner_join(), pero sólo se conservan las filas con claves en ambos conjuntos de datos, excepto que no conserva ningún dato del conjunto de datos que aparece a la derecha o en segundo lugar.

#6.El anti_join(): conserva las observaciones del primer data.frame que no coinciden con el segundo .

# ================================================================================
sat_demog_dir <- sat %>%
  left_join(demog, by = c("DBN"))
str(sat_demog_dir)

#Análisis de correlación sencilla
plot(sat_demog_dir$SAT.Math.Avg..Score,sat_demog_dir$black_per)
plot(sat_demog_dir$SAT.Math.Avg..Score,sat_demog_dir$white_per)
plot(sat_demog_dir$SAT.Math.Avg..Score,sat_demog_dir$asian_per)


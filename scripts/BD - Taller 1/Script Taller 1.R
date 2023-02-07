################################################################
# Problem Set 1: Script
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Cárdenas,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret")

# Importing Dataset (Webscrapping)-------------------------------------------------------

# Creamos tabla a partir de la base de datos en la wrb de GEIH, para esto hacemos un ciclo for
#para leer todos los data chunks de la página web

df_list <- list()

for (i in 1:10) {
  html_i <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")) %>%
    html_table()
  df_i <- as.data.frame(html_i)
  df_list[[i]] <- df_i
}

GEIH <- do.call(rbind, df_list) ## compilamos todas las lecturas en una sola data.
GEIH <-GEIH[,-1] # Eliminamos la primera columna

# Leer los datos y guardarlos como un archivo binario R (rds) usando saveRDS()
# para hacer más eficiente la carga de los datos cuando sea necesario

#saveRDS(GEIH, file = "GEIH.rds")
#GEIH<-readRDS("GEIH.Rds")

# Cleaning data -----------------------------------------------------------

# Renombramos las variables de interés

 


### SCRIPT TALLER 1 ###

#INTEGRANTES GRUPO 1:
# JIMENA CARDENAS
# BETINA CORTÉS
# NELSON LÓPEZ
# YILMER PALACIOS

#CARGA DE DATOS DESDE LA WEB - WEBSCRAPPING

rm(list = ls())

require(pacman)  ## llamar la librería pacman: contiene la función p_load()
p_load(tidyverse, rvest) ## p_load llama/instala-llama las librerías que se enlistan:
# Tidyverse contiene las librerías ggplot, dplyr... /// para hacer web-scraping de páginas estáticas


my_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html"
browseURL(my_url) ## Ir a la página

my_html = read_html(my_url) ## leer el html de la página
class(my_html) ## ver la clase del objeto

###__________

## extraer todas las tablas del html 
my_table = my_html %>% html_table()

## numero de tablas extraidas
length(my_table)


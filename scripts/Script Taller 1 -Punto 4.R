################################################################
# Problem Set 1: Script
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Cárdenas,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
#install.packages("MASA")

require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","boot", "sandwich", "ggplot2","MASA")

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
GEIH<-readRDS("GEIH.Rds")

# Cleaning data -----------------------------------------------------------

# Se eliminan las observaciones de las personas menores de 18 años y las personas
# desocupadas
GEIH <-GEIH[GEIH$age>=18,] 
GEIH <- GEIH[GEIH$ocu == 1, ]

# Se renombran la variable de máx. nivel de eduación para mayor claridad
GEIH <- rename(GEIH, educ = p6210)

# La variable de educación se establece como una categórica
GEIH$educ <- factor(GEIH$educ)
class(GEIH$educ)

# La variable de tipo de ocupación (relab) se establece como una categórica
GEIH$relab <- factor(GEIH$relab)
class(GEIH$relab)


#Cálculo de la experiencia potencial
# En primer lugar, se estiman los años de educación dependiendo del máximo nivel 
# alcanzado
GEIH$añoseduc <- ifelse(GEIH$educ == 3, 5, 
                      ifelse(GEIH$educ == 4, 9, 
                             ifelse(GEIH$educ == 5, 11, 
                                    ifelse(GEIH$educ == 6, 16, 
                                           ifelse(GEIH$educ == 9, 0, 0)))))


# Se aplica la fórmula de experiencia potencial
GEIH$experp <- GEIH$age - 5 - GEIH$añoseduc

# Los valores negativos se aproximan a 0 experiencia
GEIH$experp <- ifelse(GEIH$experp < 0, 0, GEIH$experp)

# Se eliminan las personas que tengan una experiencia de 0 años
GEIH<- GEIH[GEIH$experp>0,]

# Cálculo de las horas totales trabajadas
# Se suman las horas trabadas en el empleo principal y secundario en una sola variable
# llamada horast
GEIH$hoursWorkActualSecondJob <- ifelse(is.na(GEIH$hoursWorkActualSecondJob), 0, GEIH$hoursWorkActualSecondJob)
GEIH$horast <- GEIH$hoursWorkUsual + GEIH$hoursWorkActualSecondJob

# Se eliminan las personas que tengan un ingreso total de 0 
GEIH<- GEIH[GEIH$ingtot>0,]

#Salario por hora (Ingresos mensuales, por 12 meses, dividido en las horas semanales trabajadas por 52 semanas del año)
GEIH$inghora <- (GEIH$ingtot*12)/(GEIH$horast*52)

# Se hace un subset de las variables de interés 
GEIHf <-subset(GEIH, select = c("inghora","age","sex","educ","experp","horast","estrato1","relab")) 
GEIHf <- na.omit(GEIHf)

#Stargazer no lee variables categóricas, por eso las volví a poner numéricas, Y: perfecto! :)
GEIHf$sex <- as.numeric(GEIHf$sex)
GEIHf$educ <- as.numeric(GEIHf$educ)
GEIHf$relab <- as.numeric(GEIHf$relab)

stargazer(GEIHf, summary = TRUE, type = "text")

#Identificamos y eliminamos outliers, estos serán los que estén 3 sd alejados de la media, 
# creamos nuevo df llamado GEIHSO

GEIHSO <- GEIHf

GEIHSO <- GEIHSO %>%
  filter(inghora < mean(inghora) + 3 * sd(inghora) &
           inghora > mean(inghora) - 3 * sd(inghora))

GEIHSO <- GEIHSO %>%
  filter(age < mean(age) + 3 * sd(age) &
           age > mean(age) - 3 * sd(age))

GEIHSO <- GEIHSO %>%
  filter(experp < mean(experp) + 3 * sd(experp) &
           experp > mean(experp) - 3 * sd(experp))

#Pensar bien para presentar estadísticas descriptivas y revisar construcciòn de variables, Y: listo

stargazer(GEIHSO, summary = TRUE, type = "text")


#/////////////////////////////////////////////////////////////////////////////

# Age-wage profile --------------------------------------------------------

#Logaritmo del salario por hora
GEIHSO$lningresoh <- log(GEIHSO$inghora)

#Edad al cuadrado
GEIHSO$age2 <- I(GEIHSO$age^2)

reg1 <- subset (GEIHSO, select = c("lningresoh","age","age2"))
view(reg1)

#Regresión lineal 
reg_lin <- lm(lningresoh ~ age + age2, data = GEIHSO)


# Ver el resultado de la regresión lineal
summary(reg_lin)
stargazer(reg_lin,type="text")


# Relación entre los residuos de la regresión lineal y la variable dependiente 
ggplot(reg1) + 
  geom_point(aes(x = reg_lin$residuals, y = lningresoh)) +
  ggtitle("Residuos de la regresión lineal vs ln ingreso por hora") +
  labs(x = "Residuales", y = "Log de ingreso por hora")

# Esto se hace para ver qué tanto los residuales se ajustan a la recta de la regresión
# Este gráfico se utiliza comúnmente para verificar la normalidad y homogeneidad de 
# los residuos y para asegurarse de que los supuestos del modelo estén siendo cumplidos.

#Analizar la edad pico con los resultados del modelo
coefs <- coef(reg_lin)
p_ingh<- coefs[1]+coefs[2]* reg1$age + coefs[3]* reg1$age2
p_ingh <- c(p_ingh)
p_ingh<- data.frame(p_ingh)

#Gráfico del perfil edad-ingresos estimado que implica la ecuación anterior
ggplot(p_ingh) + 
  geom_point(aes(x = reg1$age, y = p_ingh)) +
  ggtitle("Perfil edad-ingresos") +
  labs(x = "Edad", y = "Pred. log. de ingreso por hora")

#Construcción de intervalos de confianza usando bootstrap
age_mean<- mean(reg1$age)
age2_mean<- mean(reg1$age2)
betas <- reg_lin$coefficients
view(betas)
b0<-betas[1]
b1<-betas[2]
b2<-betas[3]

#Set seed para asegurar replicabilidad
set.seed(1111)
R<-1000
est_reg1<- rep(0,R)

#Loop que calcula la edad media para la cual el ingreso por hora promedio es máximo

for(i in 1: R){ age_mean<- mean(reg1$age)
age2_mean<- mean(reg1$age2)
ingh_sample<- sample_frac(reg1, size = 1,replace = TRUE)
reg1_sample<- lm(lningresoh ~ age + age2, data = ingh_sample)
betas1<- reg1_sample$coefficients
betas1
b0 <- betas1[1]
b1<- betas1[2]
b2 <- betas1[3]
est_reg1[i]<- b1/(-2*b2)
}

#Histograma
hist(est_reg1, main = "Edad en la que se max. ln ingreso por hora", xlab = "Edad", col = "gray")
abline(v = mean(est_reg1), col = "red", lwd = 2)

summary(est_reg1)

#Intervalos de confianza
est_boot <- function(reg1, index, 
                     age_mean = mean(reg1$age), 
                     age2_mean =  mean(reg1$age2) ){
  coef(lm(lningresoh~age+ age2, data = reg1, subset = index))}

bootstatistics <- boot(reg1, statistic = est_boot, R = 1000)
bootstatistics

#Cálculo de la edad pico
edadpico <- b1/(-2*b2)
edadpico





################################################################
# Problem Set 1: Script
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Cárdenas,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","boot", "sandwich", "ggplot2", "boot")

# Importing Dataset (Webscrapping)-------------------------------------------------------

# Importamos la base datos haciendo webscrapping, para esto hacemos un loop
# para leer todos los data chunks de la página web

df_list <- list()

for (i in 1:10) {
  html_i <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")) %>%
    html_table()
  df_i <- as.data.frame(html_i)
  df_list[[i]] <- df_i
}

GEIH <- do.call(rbind, df_list) ## Compilamos todas las lecturas en un solo data frame.
GEIH <-GEIH[,-1] # Eliminamos la primera columna

# Leer los datos y guardarlos como un archivo binario R (rds) usando saveRDS()
# para hacer más eficiente la carga de los datos cuando sea necesario

saveRDS(GEIH, file = "GEIH1.rds")
GEIH<-readRDS("GEIH1.Rds")

# Cleaning data -----------------------------------------------------------

# Se eliminan las observaciones de las personas menores de 18 años y las personas
# desocupadas
GEIH <-GEIH[GEIH$age>=18,] 
GEIH <- GEIH[GEIH$ocu == 1, ]

# Se renombran la variable de máx. nivel de eduación para mayor claridad
GEIH <- rename(GEIH, educ = p6210)

# La variable de educación y tipo de ocupación se establecen como categóricas
GEIH$educ <- factor(GEIH$educ)
class(GEIH$educ)

GEIH$relab <- factor(GEIH$relab)
class(GEIH$relab)


# Cálculo de la experiencia potencial
# En primer lugar, se estiman los años de educación dependiendo del máximo nivel 
# alcanzado
GEIH$añoseduc <- ifelse(GEIH$educ == 3, 5, 
                        ifelse(GEIH$educ == 4, 9, 
                               ifelse(GEIH$educ == 5, 11, 
                                      ifelse(GEIH$educ == 6, 16, 
                                             ifelse(GEIH$educ == 9, 0, 0)))))


# Se aplica la fórmula de experiencia potencial, Los valores negativos se aproximan a 0 experiencia y
# Se eliminan las personas que tengan una experiencia de 0 años

GEIH$experp <- GEIH$age - 5 - GEIH$añoseduc
GEIH$experp <- ifelse(GEIH$experp < 0, 0, GEIH$experp)
GEIH<- GEIH[GEIH$experp>0,]


# Cálculo de las horas totales trabajadas - Se suman las horas trabadas en el empleo principal y secundario en una sola variable
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

#Stargazer no lee variables categóricas, por eso las volví a poner numéricas
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


# Revisión de estadísticas significativas
stargazer(GEIHSO, summary = TRUE, type = "text")


# Creamos la variable Ln(wage)
GEIHSO$lningresoh <- log(GEIHSO$inghora)


#Realizamos histograma de las variables de interés para mayor análisis

par(mfrow = c(2, 3))
hist(GEIHSO$lningresoh, main = "Ln Salario por hora", xlab = "Valores")
hist(GEIHSO$age, main = "Edad", xlab = "Valores")
hist(GEIHSO$educ, main = "Educación", xlab = "Valores")
hist(GEIHSO$experp, main = "Experiencia Potencial", xlab = "Valores")
hist(GEIHSO$relab, main = "Tipo de Ocupación", xlab = "Valores")
hist(GEIHSO$estrato1, main = "Estrato Socio-Económico", xlab = "Valores")

par(mfrow = c(1, 1))


#Realizamos Boxplot de las variables de interés para mayor análisis

par(mfrow = c(2, 3))
boxplot(GEIHSO$age, horizontal = TRUE, main = "Edad", outlier.shape = "*", outlier.color = "red")
boxplot(GEIHSO$educ, horizontal = TRUE, main = "Educación", outlier.shape = "*", outlier.color = "red")
boxplot(GEIHSO$experp, horizontal = TRUE, main = "Experiencia Potencial", outlier.shape = "*", outlier.color = "red")
boxplot(GEIHSO$relab, horizontal = TRUE, main = "Tipo de Ocupación", outlier.shape = "*", outlier.color = "red")
boxplot(GEIHSO$estrato1, horizontal = TRUE, main = "Estrato Socio-Económico", outlier.shape = "*", outlier.color = "red")
par(mfrow = c(1, 1))


# P3: Age - wage profile ------------------------------------------------------
# En esta sección del script se desarrolla el tercer (3) punto del taller

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
set.seed(111)
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
abline(v = mean(est_reg1), col = "blue", lwd = 2)
xlim(44,50)
axis(side = 1, at = seq(44,50, by = 1))

summary(est_reg1)

#Intervalos de confianza
est_boot <- function(reg1, index, 
                     age_mean = mean(reg1$age), 
                     age2_mean =  mean(reg1$age2) ){
  coef(lm(lningresoh~age+ age2, data = reg1, subset = index))}

bootstatistics <- boot(reg1, statistic = est_boot, R = 1000)
bootstatistics

# Para b0
ICinfb0<- 7.8060988293 - 1.96*(6.072904e-02)
ICsupb0 <- 7.8060988293 + 1.96*(6.072904e-02)

ICinfb0
ICsupb0

# Para b1
ICinfb1<- 0.0427107403 - 1.96*(3.216626e-03)
ICsupb1 <- 0.0427107403 + 1.96*(3.216626e-03)

ICinfb1
ICsupb1

# Para b2
ICinfb2<- -0.0004603549 - 1.96*(3.921086e-05)
ICsupb2 <- -0.0004603549 + 1.96*(3.921086e-05)

ICinfb2
ICsupb2

#Cálculo de la edad pico
edadpico <- b1/(-2*b2)
edadpico

#Borramos df para preparar data para el próximo punto
keep_vars <- c("GEIHSO")
all_vars <- ls()
rm(list = setdiff(all_vars, keep_vars))

saveRDS(GEIHSO, file = "GEIH.rds")
rm(list = ls()) 
GEIHSO<-readRDS("GEIH.Rds")


# P4.The gender earnings GAP -------------------------------------------------

# 4.a Wage & Gender Relationship

# La variable de educación y tipo de ocupación se establecen como categóricas
GEIHSO$educ <- factor(GEIHSO$educ)
class(GEIHSO$educ)

GEIHSO$relab <- factor(GEIHSO$relab)
class(GEIHSO$relab)

GEIHSO$estrato1 <- factor(GEIHSO$estrato1)
class(GEIHSO$relab)


# Se seleccionan las variables a usar
reg2 <- subset (GEIHSO, select = c("lningresoh","sex"))

#Regresión lineal 
reg_lin <- lm(lningresoh ~ sex, data = GEIHSO)

# Ver el resultado de la regresión lineal
summary(reg_lin)
stargazer(reg_lin,type="text")

# Regresión con errores robustos
reg <- lm(lningresoh ~ sex, data = GEIHSO)
cov_matrix <- vcovHC(reg, type = "HC1")
se_coef <- sqrt(diag(cov_matrix))
t_value <- coef(reg) / se_coef
p_value <- 2 * (1 - pt(abs(t_value), df.residual(reg)))
confint_lower <- coef(reg) - qt(0.975, df.residual(reg)) * se_coef
confint_upper <- coef(reg) + qt(0.975, df.residual(reg)) * se_coef
cbind(coef = coef(reg), se_coef, t_value, p_value, confint_lower, confint_upper)

# Se descarta la regresión con errores robustos, pues la veriable es dicótoma y no aporta mucho al ejercicio 

ggplot(reg2, aes(x = sex, y = lningresoh)) + 
  geom_point() + 
  ggtitle("Scatter Plot") +
  labs(x = "Sex", y = "Wage")

# 4.b Equal Pay for Equal Work? ----------------------

# Regresión convencional

GEIHSO$age2 <- I(GEIHSO$age^2)
reg2control <- subset (GEIHSO, select = c("lningresoh","sex", "age", "age2","educ","experp","relab","estrato1"))
reg_lincontrol <- lm(lningresoh ~ ., data = reg2control)
summary(reg_lincontrol)
stargazer(reg_lincontrol,type="text")

#prueba de colinealidad VIF para la variable experp: dado que tenemos variables tipo factor, VIF del paquete "car" 
#no la puede cálcular, lo haremos a "mano"

regVIF <- lm(experp ~ sex + age + age2 + educ + relab + estrato1, data = reg2control)
residuals <- residuals(regVIF)
sse <- sum(residuals^2)
dof <- nrow(regVIF$model) - 2
variance_estimate <- sse / dof
VIF <- 1/variance_estimate
VIF # efectivamente nos dió colinealidad.

# Regresión usando FWL

regFWL<-lm(lningresoh ~ sex + age + age2 + educ + experp + relab + estrato1, data = reg2control)
regFWLSex<-lm(lningresoh ~ sex, data = reg2control)
stargazer(regFWL,regFWLSex,type="text",digits=7)

#Paso 1 FWL
reg2control<-reg2control %>% mutate(SexResidF=lm(sex~ age + age2 + educ + experp + relab + estrato1 ,reg2control)$residuals) #Residuals of sex~controls 
reg2control<-reg2control %>% mutate(lnwageResidF=lm(lningresoh ~ age2 + age + educ + experp + relab + estrato1 ,reg2control)$residuals) #Residuals of lnwage~controls 

#Paso 2 FWL
regFWL2<-lm(lnwageResidF~SexResidF,reg2control)
stargazer(regFWL,regFWLSex,regFWL2,type="text",digits=7) # with stargazer we can visualize the coefficients next to each other

sum(resid(regFWL)^2)
sum(resid(regFWLSex)^2)
sum(resid(regFWL2)^2)

sqrt(diag(vcov(regFWL2))*(15659/15639))[2]
sqrt(diag(vcov(regFWL)))[2]

# Regresión usando FWL + Bootstrap (estimates + SE)

set.seed(12345)

regFWLBoot <- subset (GEIHSO, select = c("lningresoh","sex", "age2", "age","educ","experp","relab","estrato1"))

FWLFun <-function(data,index){
  
  regFWLBoot <- subset (GEIHSO, select = c("lningresoh","sex", "age2", "age","educ","experp","relab","estrato1"))
  data<-data %>% mutate(SexResidF=lm(sex~ age + age2 + educ + experp + relab + estrato1 ,data)$residuals) #Residuals of sex~controls 
  data<-data %>% mutate(lnwageResidF=lm(lningresoh ~ age2 + age + educ + experp + relab + estrato1 ,data)$residuals) #Residuals of lnwage~controls 
  coef(lm(lnwageResidF~SexResidF, data = data, subset = index))[2]  # reg lnwage residual on sex residuals
  
}

FWLFun (regFWLBoot,1:nrow(regFWLBoot)) # probamos la función
databoot <- boot(regFWLBoot, FWLFun, R = 1000) # ejecutamos el Bootstrap
databoot

# 4.c Plot age-wage by gender ----------------------

set.seed(12345)
regFWLBoot <- subset (GEIHSO, select = c("lningresoh","sex", "age2", "age"))

# Para el caso de los hombres:
regFWLBootMale <- regFWLBoot[regFWLBoot$sex == 1, ]
regprueba <- lm(lningresoh ~ age + age2, data = regFWLBootMale)
stargazer(regprueba,type="text")


coefregmale <- lm(lningresoh ~ age + age2, data = regFWLBootMale)$coefficients
graficamale <- coefregmale[1]+coefregmale[2]*regFWLBootMale$age+coefregmale[3]*regFWLBootMale$age2
plot(regFWLBootMale$age, graficamale, xlab="Edad", ylab="ln(salario)", xlim = c(18, max(regFWLBootMale$age)))


Malemax <-function(data,index){
  coefs <- (lm(lningresoh ~ age + age2, data = data, subset = index))$coefficients  # reg lnwage residual on sex residuals
  b1<-coefs[2]
  b2<-coefs[3] 
  Agemax <- (-b1)/(2*b2)
  return(Agemax)
  
}

Malemax (regFWLBootMale,1:nrow(regFWLBootMale)) # probamos la función
databootmale <- boot(regFWLBootMale, Malemax, R = 1000) # ejecutamos el Bootstrap
databootmale

ICinfmale <- 49.12867 - 1.96*(1.121325)
ICsupmale <- 49.12867 + 1.96*(1.121325)

ICinfmale
ICsupmale

# Ahora para la mujer

set.seed(12345)
regFWLBoot <- subset (GEIHSO, select = c("lningresoh","sex", "age2", "age"))
regFWLBootFemale <- regFWLBoot[regFWLBoot$sex == 0, ]
regprueba <- lm(lningresoh ~ age + age2, data = regFWLBootFemale)
stargazer(regprueba,type="text")

coefregfemale <- lm(lningresoh ~ age + age2, data = regFWLBootFemale)$coefficients
graficafemale <- coefregfemale[1]+coefregfemale[2]*regFWLBootFemale$age+coefregfemale[3]*regFWLBootFemale$age2
plot(regFWLBootFemale$age, graficafemale, xlab="Edad", ylab="ln(salario)", xlim = c(18, max(regFWLBootMale$age)))



Femalemax <-function(data,index){
  coefs <- (lm(lningresoh ~ age + age2, data = data, subset = index))$coefficients  # reg lnwage residual on sex residuals
  b1<-coefs[2]
  b2<-coefs[3] 
  Agemax <- (-b1)/(2*b2)
  return(Agemax)
  
}

Femalemax (regFWLBootFemale,1:nrow(regFWLBoot)) # probamos la función
databootfemale <- boot(regFWLBootFemale, Femalemax, R = 1000) # ejecutamos el Bootstrap
databootfemale

ICinfmale <- 42.03397 - 1.96*(0.9934249)
ICsupmale <- 42.03397 + 1.96*(0.9934249)

ICinfmale
ICsupmale

# P5: Predicting earnings -------------------------------------------------

#Borramos df para preparar data para el punto 5
rm(list = ls()) 
GEIHSO<-readRDS("GEIH.Rds")

GEIHSO$relab <- factor(GEIHSO$relab)
GEIHSO$educ <- factor(GEIHSO$educ)
GEIHSO$estrato1 <- factor(GEIHSO$estrato1)

# Se divide la muestra 70% 30%, se incluye la semilla permite reproducibilidad
set.seed(1111) 

# Calcular el número de filas en la base de datos
n_rows <- nrow(GEIHSO)

# Calcular el tamaño de los conjuntos de entrenamiento y prueba
train_size <- round(n_rows * 0.7)
test_size <- n_rows - train_size

# Crear un vector con números aleatorios para seleccionar las filas para el conjunto de entrenamiento y prueba
split_indices <- sample(1:n_rows, size = n_rows, replace = FALSE)

# Dividir la base de datos en conjunto de entrenamiento y prueba
train_data <- GEIHSO[split_indices[1:train_size], ]
test_data <- GEIHSO[split_indices[(train_size + 1):n_rows], ]

# Se verifican las dimensiones de cada dataframe
dim(train_data)
dim(test_data)

# Se vuelven a estimar los modelos planteados a lo largo del Problem Set, además de uno incial
# con solo la constante para usar como ejercicio base 

# Modelo base: Comenzamos utilizando un modelo simple sin covariables, sólo una constante.
mp0<- lm(lningresoh ~ 1, data = train_data)
summary(mp0)
coef(mp0)
paste("Coef:", mean(train_data$lningresoh))
test_data$mp0<-predict(mp0,newdata = test_data)

#Se calcula el MSE para el modelo base (es decir, su rendimiento en la predicción)
MSE_mp0<-with(test_data,mean((lningresoh-mp0)^2))

# Modelo previo 1
mp1<- lm(lningresoh ~ age + age2, data = train_data)
summary(mp1)
coef(mp1)
paste("Coef:", mean(train_data$lningresoh))
test_data$mp1<-predict(mp1,newdata = test_data)

#Se calcula el MSE para el modelo previo 1
MSE_mp1<-with(test_data,mean((lningresoh-mp1)^2))

# Modelo previo 2
mp2<- lm(lningresoh ~ sex, data = train_data)
summary(mp2)
coef(mp2)
paste("Coef:", mean(train_data$lningresoh))
test_data$mp2<-predict(mp2,newdata = test_data)

#Se calcula el MSE para el modelo previo 2
MSE_mp2<-with(test_data,mean((lningresoh-mp2)^2))

# Modelo previo 3
mp3<- lm(lningresoh ~ sex + age + age2 + educ + experp + relab + estrato1, data = train_data)
summary(mp3)
coef(mp3)
paste("Coef:", mean(train_data$lningresoh))
test_data$mp3<-predict(mp3,newdata = test_data)

#Se calcula el MSE para el modelo previo 3
MSE_mp3<-with(test_data,mean((lningresoh-mp3)^2))

#Se presentan las estimaciones de los modelos
stargazer(mp0,mp1,mp2,mp3, summary = TRUE, type = "text")


#Se plantean 5 modelos nuevos con especificaciones adicionales que incluya no-linealidades y complejidades respecto a los anteriores

# Modelo nuevo 1 (logaritmo en var experiencia potencial)
test_data$lnexperp <- log(test_data$experp)
train_data$lnexperp <- log(train_data$experp)

mn1<- lm(lningresoh ~ sex + age + age2 + educ + lnexperp + relab + estrato1, data = train_data)
coef(mn1)
paste("Coef:", mean(train_data$lningresoh))
test_data$mn1<-predict(mn1,newdata = test_data)

#Se calcula el MSE para el modelo nuevo 1
MSE_mn1<-with(test_data,mean((lningresoh-mn1)^2))

# Modelo nuevo 2 (Interacción entre estrato y género)
mn2<- lm(lningresoh ~ sex + age + age2 + educ + experp + relab + estrato1 + sex*estrato1, data = train_data)
coef(mn2)
paste("Coef:", mean(train_data$lningresoh))
test_data$mn2<-predict(mn2,newdata = test_data)

#Se calcula el MSE para el modelo nuevo 2
MSE_mn2<-with(test_data,mean((lningresoh-mn2)^2))

# Modelo nuevo 3 (Quitando edad^2 y poniendo experiencia^2)
mn3<- lm(lningresoh ~ sex + age + educ + experp + I(experp^2) + relab + estrato1, data = train_data)
coef(mn3)
paste("Coef:", mean(train_data$lningresoh))
test_data$mn3<-predict(mn3,newdata = test_data)

#Se calcula el MSE para el modelo nuevo 3
MSE_mn3<-with(test_data,mean((lningresoh-mn3)^2))

# Modelo nuevo 4 (Interacción entre edad y género)
mn4<- lm(lningresoh ~ sex + age*sex + age + age2 + educ + experp + relab + estrato1, data = train_data)
coef(mn4)
paste("Coef:", mean(train_data$lningresoh))
test_data$mn4<-predict(mn4,newdata = test_data)

#Se calcula el MSE para el modelo nuevo 4
MSE_mn4<-with(test_data,mean((lningresoh-mn4)^2))

# Modelo nuevo 5 (Interacción entre edad y género, quitando edad^2)
mn5<- lm(lningresoh ~ sex + age*sex + age + educ + experp + relab + estrato1, data = train_data)
coef(mn5)
paste("Coef:", mean(train_data$lningresoh))
test_data$mn5<-predict(mn5,newdata = test_data)

#Se calcula el MSE para el modelo nuevo 5
MSE_mn5<-with(test_data,mean((lningresoh-mn5)^2))

#Se presentan las estimaciones de los modelos nuevos
stargazer(mn1,mn2,mn3,mn4,mn5, summary = TRUE, type = "text")

#Los resultados de los rendimientos de preducción para los modelos previos y nuevos se condensan a continuación:
MSE_table<-c(MSE_mp0, MSE_mp1, MSE_mp2, MSE_mp3, MSE_mn1,MSE_mn2,MSE_mn3,MSE_mn4,MSE_mn5)
x_label<-c('Modelo 0','Modelo 1', 'Modelo 2', 'Modelo 3', 'Modelo nuevo 1','Modelo nuevo 2','Modelo nuevo 3','Modelo nuevo 4', 'Modelo nuevo 5')
MSEtabla<-data.frame(x_label,MSE_table)

#Se grafican los MSE de cada uno de los modelos predicos para poder compararlos
ggplot(data=MSEtabla, aes(x = x_label, y = MSE_table, group=1)) + 
  geom_line() +  
  geom_point() +
  ggtitle("MSE de los modelos especificados") +
  ylab("MSE") +
  xlab ("Número de modelo")

#Para tener idea de cuáles modelos tienen el MSE más bajo
ordenMSE <- MSEtabla[order(MSEtabla$MSE_table), ]
View(ordenMSE)

# De todos los modelos presentados, los que tienen un menor MSE son los modelos nuevos 1 y 3. Es decir, en donde se tiene un mejor performance en la predicción. 
# Sin embargo, los MSE son muy similares a los de los demás modelos, especialmente el modelo previo 3 a y los nuevos 2, 4 y 5. 

# Apalancamiento
install.packages("caret")

alpha <- c()
u <- c()
h <- c()

#El modelo con menor error cuadrático medio se calcula nuevamente
bestmodel<-lm(lningresoh ~ sex + age + age2 + educ + lnexperp + relab + estrato1, data = test_data)

#Calcular el leverage para el modelo con el menor MSE
alphass <- c()
for (j in 1:nrow(test_data)) {
  u_j <- bestmodel$residual[j]
  h_j <- lm.influence(bestmodel)$hat[j]
  alpha <- u_j/(1-h_j)
  alphass <- c(alphass, alpha)
} 

#Teniendo en cuenta que es posible que un leverage mayor a 1 o menor que -1 se podría considerar alto, se calcula lo siguiente:
alphass<-data.frame(alphass)
leverage<-alphass[alphass$alphass>=1|alphass<=-1,]
leverage<-data.frame(leverage)
lvpercentage<-((nrow(leverage)/nrow(alphass)*100))
xlabel_alpha<-1:nrow(test_data)
xlabel_alpha<-data.frame(xlabel_alpha)
alphass<-cbind(alphass, xlabel_alpha)
view(lvpercentage)

# Se grafican los resultados obtenidos
ggplot(data=alphass, aes(x = xlabel_alpha, y = alphass, group=1)) + 
  geom_point() + 
  ggtitle("Leverage para el modelo con mejor métrica de MSE")

#Se consultan los valores máximos y mínimos
max(alphass$alphass)
min(alphass$alphass)


# LOOCV para el modelo con mejor performance predictivo, es decir, el mn1
GEIHSO$lnexperp <- log(GEIHSO$experp)

modelLOOCV1 <- train(lningresoh ~ sex + age + age2 + educ + lnexperp + relab + estrato1, 
                     data = GEIHSO,
                     method = "lm",
                     trControl = trainControl(method = "LOOCV"))

# Resultados 
modelLOOCV1

RMSE_modelLOOCV1<-modelLOOCV1$results
RMSE_modelLOOCV1<-RMSE_modelLOOCV1$RMSE
RMSE_modelLOOCV1<-mean(RMSE_modelLOOCV1)

view(RMSE_modelLOOCV1)
# 0.6448116

# LOOCV para el segundo modelo con mejor performance predictivo, es decir, el mn3
modelLOOCV2 <- train(lningresoh ~ sex + age + educ + experp + I(experp^2) + relab + estrato1, 
                     data = GEIHSO,
                     method = "lm",
                     trControl = trainControl(method = "LOOCV"))

# Resultados 
modelLOOCV2

RMSE_modelLOOCV2<-modelLOOCV2$results
RMSE_modelLOOCV2<-RMSE_modelLOOCV2$RMSE
RMSE_modelLOOCV2<-mean(RMSE_modelLOOCV2)

view(RMSE_modelLOOCV2)
# 0.6469727




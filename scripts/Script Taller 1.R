################################################################
# Problem Set 1: Script
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Cárdenas,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","boot", "sandwich", "ggplot2","MASA", "boot")

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
ICinfb0<- 49.12867 - 1.96*(1.121325)
ICsupb0 <- 49.12867 + 1.96*(1.121325)

ICinfb0
ICsupb0

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
reg2control <- subset (GEIHSO, select = c("lningresoh","sex", "age2", "age","educ","experp","relab","estrato1"))
reg_lincontrol <- lm(lningresoh ~ ., data = reg2control)
summary(reg_lincontrol)
stargazer(reg_lincontrol,type="text")

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

sqrt(diag(vcov(regFWL2))*(15659/15653))[2]
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
regFWLBootMale <- regFWLBoot[regFWLBoot$sex == 1, ]
regprueba <- lm(lningresoh ~ age + age2, data = regFWLBootMale)
stargazer(regprueba,type="text")


coefregmale <- lm(lningresoh ~ age + age2, data = regFWLBootMale)$coefficients
graficamale <- coefregmale[1]+coefregmale[2]*regFWLBootMale$age+coefregmale[3]*regFWLBootMale$age2
plot(regFWLBootMale$age, graficamale)



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
plot(regFWLBootFemale$age, graficafemale)



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



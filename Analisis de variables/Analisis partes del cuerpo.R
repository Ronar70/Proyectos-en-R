### PARTES DEL CUERPO
### Cargamos archivo
file.choose()
 pdc <- read.table("C:\\Users\\Familia Leon\\R\\Master D\\Ejemplos\\partes del cuerpo.txt", header = TRUE)

glimpse(pdc)

#Modificamos el tipo de variable
pdc$edad <- as.integer(pdc$edad)
pdc$altura <- as.integer(pdc$altura)
pdc$muneca <- as.integer(pdc$muneca)
pdc$biceps <- as.integer(pdc$biceps)
pdc$peso <- as.integer(pdc$peso)

glimpse(pdc)


### ANALISIS DE NORMALIDAD
# obtenemos los coeficientes de correlacion de las variables numericas
#Esta es la primera opcion
pdc_cor <- cor(pdc_numer, method = "pearson")
round(cor(pdc_cor, method = "pearson"), digits = 2)
#segunda opcion
pdc_cor <- round(cor(pdc_cor), digits = 2)
pdc_cor

#Realizamos los graficos de los coeficietes de correlacion
ggcorr(pdc_cor, method = c("everything", "pearson"))

ggpairs(pdc, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")
        
ggcorrplot(pdc_cor, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Correlacion variables  PDC", 
           ggtheme=theme_bw)

#Validamos el coeficiente de correlacion con cada variable
#plot de las las variables independientes
plot(pdc$peso~pdc$edad)
plot(pdc$peso~pdc$altura)
plot(pdc$peso~pdc$muneca)
plot(pdc$peso~pdc$biceps)

cor.test(pdc$peso,pdc$edad)
cor.test(pdc$peso,pdc$altura)
cor.test(pdc$peso,pdc$muneca)
cor.test(pdc$peso,pdc$biceps)

##HIPOTESIS
#Las hipotesis planteadas son sobre los coeficientes de correlacion
#Ho Coeficiente = 0
#Ha Coeficiente diferente de 0
# Metodo del valor Pvalue
# Ho= a favor de Ha si el nivel de significancia alfa>=Pvalue # alfha =0.05
  0.05>=0.001111
  0.05>=6.619e-09
  0.05>=4.941e-10
  0.05>=7.095e-14
  
#RESUMEN
# De acuerdo se rechaza la Ho a favor de la Ha
# La correlacion mas fuerte es altura 0.79 edad 0.52 biceps 0.90 muneca 0.82

#PRUEBA DE NORMALIDAD 
#Si cumple la lineamidad para los dos predictores
#Revisamos la distribucion de los datos con hist y qqnorm
hist(pdc$edad, col="blue")
hist(pdc$altura, col="red")
hist(pdc$muneca, co="green")
hist(pdc$biceps, col="pink")

qqnorm(pdc$edad)
qqline(pdc$edad, pch=19, col="red")
qqnorm(pdc$altura)
qqline(pdc$altura, pch=19, col="blue")
qqnorm(pdc$muneca)
qqline(pdc$muneca, pch=19, col="blue")
qqnorm(pdc$biceps)
qqline(pdc$biceps, pch=19, col="blue")

#Comprobamos que existe nnormalidad en los datos
# Adicional otra tecnica es contrastes de normalidad 
# shapiro (menos de 50 obser) y Kolmogorov (mas de 50 obser)
# Para que cumpla con normalidad alfha debe ser mayor que 0.05
altura_test <- shapiro.test(pdc$altura)
print(altura_test)
# Esta variable cumple con la hipotesis Ha

edad_test <- shapiro.test(pdc$edad)
print(edad_test)
# Esta variable NO cumple la hipotesis nula y podemos mejorar su normalidad con Log pvalue < 0.05 0.0005681 Menor que 0.05

munec_test <- shapiro.test(pdc$muneca)
print(munec_test)
#____Hiptesis__


# Esta variable  NO cumple la hipotesis nula y podemos mejorar su normalidad con Log pvalue < 0.05  Pvalue0.002974 menor que 0.05
# 
# 
biceps_test <- shapiro.test(pdc$biceps)
print(biceps_test)
# Esta variable cumple la hipotesis nula y podemos mejorar su normalidad con Log pvalue < 0.05 pvalue 0.008725 menor que 0.05

#TRANSFORMACION DE VARIABLES
#Transformar las variables que no cumplen para mejorar su distribución.
par(mfrow = c(1, 1))
hist(log10(pdc$edad), breaks = 10, main = "", xlab = "Log10(edad)",
     border = "blue")

qqnorm(log10(pdc$edad), main = "", col = "blue")
qqline(log10(pdc$edad))

par(mfrow = c(1, 1))
shapiro.test(log10(pdc$edad))
# Con la formula el pValue mejoro a 0.073 mayor que 0.05

#Procedemos con la otra variable
par(mfrow = c(1, 1))
hist(log10(pdc$muneca), breaks = 10, main = "", 
     xlab = "Log10(muneca)", border = "blue")

qqnorm(log10(pdc$muneca), main = "", col = "blue")
qqline(log10(pdc$muneca))

par(mfrow = c(1, 1))
shapiro.test(log10(pdc$muneca))
# la variable muñeca no paso la prueba, se deja a un lado

#Hacemos la prueba con la variable biceps
par(mfrow = c(1, 1))
hist(log10(pdc$biceps), breaks = 10, 
     main = "", xlab = "Log10(biceps)",
     border = "blue")

qqnorm(log10(pdc$biceps), main = "", col = "blue")
qqline(log10(pdc$biceps))

par(mfrow = c(1, 1))
shapiro.test(log10(pdc$biceps))

#Vamos a realizar una regresion lineal simple con peso y altura
#Nivel de significancia de la variables elegidad
plot(pdc$peso~pdc$altura)
cor.test(pdc$peso,pdc$altura)

#vamos a tranformar los datos para mejorar la linealidad de la variable peso y altura
#Creamos dos variables con la funcion logaritmos de las variables
pdc$lnpeso <-with(pdc,log(peso))

pdc$lnaltura <- with(pdc,log(altura))

pdc$recipaltura <-with(pdc,1/altura)

#Revisamos el df para ver las nuevas variables

#Volvemos a realizar el analisis de correlacion con las nuevas variables
plot(pdc$lnpeso~pdc$lnaltura)
cor.test(pdc$lnpeso,pdc$lnaltura)
#la relacion es moderada 0.79
#
#Reciproco
plot(pdc$peso~pdc$recipaltura)
cor.test(pdc$peso~pdc$recipaltura)

#REGRESION SIMPLE
#Estimacion de la ecuacion de regresion simple muestral
#Relacion de de Y contra X
#Efecto de la variable explicativa sobre la dependiente
#Pronostico de Y sobre valores X
modelo_linealpdc <- lm(peso ~ altura, pdc)
summary(modelo_linealpdc)

#Esta prueba me valida todos los supuestos de la RL
gvlma(modelo_linealpdc)

#Prueb de anova la significancia de la regresion F de Fisher, la anterior era de T
##esta prueba me ratifica Pvalue>0.05
anova(modelo_linealpdc)

#Resumen
#Estimate (Coefficients): estos son los valores estimados para conseguir esa predicción o regresión.
#El ejemplo anterior nos daría como resultado la siguiente función para predecir la EDAD #El modelo lineal generado sigue la ecuación Y=b0+b1X peso = -123.3481 + 1.1209*(altura)   Por cada unidad que se incrementa la altura, el peso aumenta en promedio 1.1209 unidades
#R2 0.633 indica que el modelo calculado explica %63.3% de la variabilidad presente en la variable respuesta (peso) mediante la variable independiente (altura).
#El p-value obtenido en el test F (6.619e-09) determina que sí el modelo es significante

#Podemos revisar los elementos del modelo
help(lm)
names(modelo_linealpdc)
modelo_linealpdc$coefficients
modelo_linealpdc$residuals
modelo_linealpdc$fitted.values
#INTEVALOS DE CONFIANZA DEL MODELO 
confint(modelo_linealpdc)


#GRAFICAs DEL MODELO con Plot son 4 graficas y ggplot
plot(modelo_linealpdc)
#
#Grafica de la linea recta de regresion

ggplot(data = pdc, mapping = aes(x = altura, y = peso)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'peso ~ altura', x  =  'altura') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Separando los datos para el entrenamiento.
split <- sample.split(pdc, SplitRatio =0.8)
split
train <- subset(pdc, split="TRUE")
test <- subset(pdc, split="FALSE")
train
test

prop.table(table(train$peso))
prop.table(table(test$peso))

set.seed(123)
# Se crean los índices de las observaciones de entrenamiento
install.packages("caret")
library(caret)

train <- createDataPartition(y = pdc$peso, p = 0.8, list = TRUE, times = 1)

datos_train <- datos[train, ]
datos_test  <- datos[-train, ]




#PREDICCIONES------------------------
##Lo primero que se debe hacer es agregar a los datos originales el vector con las estimaciones  

predpdc1 <- predict(modelo_linealpdc, test)
predpdc1

#Grafica compara modelo predictivo vs los valores actuales

plot(test$peso,type = "l", lty = 1.9 , col = "red")
lines(predpdc1,type = "l", col = "blue")

#Buscando la presicion el error
rmse <- sqrt(mean(predpdc1-pdc$peso)^2)
rmse


#------------------REVISAR ESTAS FUNCIONES--------------------
modelo_linealpdc=lm(peso ~ altura, data=pdc)
summ=summary(modelo_linealpdc)
rse=summ$sigma
f=qt(0.975,nrow(pdc)-2)
xx=seq(0,300,1)

predi1=predict(modelo_linealpdc,newdata = data.frame(altura=xx),
               se.fit = TRUE,interval = "confidence")
predi2=predict(modelo_linealpdc,newdata = data.frame(altura=xx),
               se.fit = TRUE,interval = "prediction")
pred.df=data.frame(x=xx,predi2$fit,se.conf=predi1$se.fit)

#==================================================================





# La función lm() calcula y almacena los valores predichos por el modelo y los # residuos.
pdc$predicciones <- predict(modelo_linealpdc)

# En  el diagrama dispersión los puntos originales  X1 Y1 estan en negro y los puntos rojos son las estimaciones ya es el modelo X1Y1 cruzan por la linea gris los residuales sson los trazon de linea roja

ggplot(pdc, aes(x=altura, y=peso)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=altura, yend=predicciones), 
               col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()

#_------------------
pdc |> 
  ggplot(aes(x=altura, y=peso))+
  geom_point()+
  geom_abline(intercept = 0.5, slope = 0.4, col = "blue")

lm(peso ~ altura, data = pdc)

# Se genera una secuencia de valores x_i que abarquen todo el rango de las
# observaciones de la variable X
puntos <- seq(from = min(pdc$altura),
              to = max(pdc$altura),
              length.out = 100)
# Se predice el valor de la variable Y junto con su intervalo de confianza para
# cada uno de los puntos generados. En la función predict() hay que nombrar a 
# los nuevos puntos con el mismo nombre que la variable X del modelo.
# Devuelve una matriz.
limites_intervalo <- predict(object = modelo_linealpdc,
                             newdata = data.frame(altura = puntos),
                             interval = "confidence", level = 0.95)
head(limites_intervalo, 3)

# Finalmente se añaden al gráfico las líneas formadas por los límites
# superior e inferior.
plot(pdc$altura, pdc$peso, col = "firebrick", pch = 19, ylab = "peso",
     xlab = "altura", main = 'peso ~ altura')
abline(modelo_lineal, col = 1)
lines(x = puntos, y = limites_intervalo[,2],type = "l", col = 2, lty = 3)
lines(x = puntos, y = limites_intervalo[,3],type = "l", col = 3, lty = 3)

# La función lm() calcula y almacena los valores predichos por el modelo y los
# residuos.
pdc$predicciones <- modelo_linealpdc$fitted.values
pdc$residuals   <- modelo_linealpdc$residuals
head(pdc)

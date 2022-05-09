#Cargamos paquetes y librerias
install.packages("lmtest")
library(lmtest)
library(MASS)
library(car)
data("Boston")
#Los siguientes casos han de ser evaluados a la hora de considerar válido un modelo lineal:
  
#1. No linealidad entre predictores y variable respuesta
#2. Correlación de los errores
#3. Variabilidad no constante en los residuos (heterocedasticidad)
#4. Outliers (valores atípicos)
#5. Puntos con alta influencia (high leverage)
#6. Colinealidad
#7. Tamaño de la muestra

# La variable chas es una variable categórica por lo que se transforma a factor
Boston$chas <- as.factor(Boston$chas)
summary(Boston)
glimpse(Boston)
# Dado que hay muchas variables, se grafican por grupos de 4, excluyendo las
# categóricas
boston_cor <- Boston |> 
  select_if(is.numeric)
#Obtenemos la correlacion numerica
boston_cor <- cor(boston_cor, method = "pearson") 
boston_cor
#obtenemos graficos
ggcorr(boston_cor, method = c("everything", "pearson"))
chart.Correlation(boston_cor, histogram = TRUE, method = "pearson")

#_______________________Regresión lineal simple_________________________
#______Se pretende predecir el valor de la vivienda en función del porcentaje de pobreza de la población
modelo_simple <- lm(data = Boston,formula = medv ~ lstat)
summary(modelo_simple)
#-----Anotaciones
#1 Multiple R-squared:  0.5441 explica el 54% de la variabilidad de las observaciones.
#2.Intercept(??0): cuando lstat es 0 el pre vivienda es  34.55384
#3.Predictor lstat(??1): por cada unidad que se incrementa el predictor lstat el precio de la vivienda disminuye en promedio 0.9500 unidades.
#
#________La estimación de todo coeficiente de regresión______
confint(modelo_simple, level = 0.95)
#                  2.5 %     97.5 %
#  (Intercept)   33.448457 35.6592247
#   lstat        -1.026148 -0.8739505
#Dado que el p-value del predictor lstat ha resultado significativo para un ??=0.05, su intervalo de confianza del 95% no contiene el valor 0.

#_____Intervalo de confianza______
#los intervalos de confianza se aplican al valor promedio que se espera de y para un determinado valor de x
#Con esta formula obtenemos el pronostico de acuerdo a un valor de la variable predictora 4.89
predict(object = modelo_simple, newdata = data.frame(lstat = c(4.89)),
        interval = "confidence", level = 0.95)
#Con esta funcion obtenemos la tabla total de todos los predictores
#Como es un intervalo el lwr es limite inferior upr limite superior
intervalos=predict(modelo_simple,interval = "confidence", level = 0.95)
print(intervalos)
#______________Intervalo de predicción_____________
predict(object = modelo_simple, newdata = data.frame(lstat = c(3.0799015)),
interval = "prediction", level = 0.95)

intervalos1=predict(modelo_simple,interval = "prediction", level = 0.95)
print(intervalos1)
#Esta funcion me arroja los valores de X para Y
#Ambos intervalos están centrados en torno al mismo valor prediccion y confidence
#
#________GRAFICA REGRESION LINEAL____________
attach(Boston)
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")
#La representación gráfica de las observaciones muestra que la relación entre ambas variables estudiadas no es del todo lineal, lo que apunta a que otro tipo de modelo podría explicar mejor la relación. Aun así la aproximación no es mala
#
#___Una de las mejores formas de confirmar que las condiciones necesarias para un modelo de regresión lineal simple por mínimos cuadrados se cumplen es mediante el estudio de los residuos del modelo
## Graficamos los minimos cuadrados
par(mfrow = c(1,2))
plot(modelo_simple)
#Anotaciones
#1.Los residuos confirman que los datos no se distribuyen de forma lineal, ni su varianza constante (plot1).
#2.se observa que la distribución de los residuos no es normal (plot2)
#----Validamos correlacion entre las variables
cor.test(Boston$medv,Boston$lstat)

lstat_test <- shapiro.test(Boston$lstat)
print(lstat_test)

#Sig(p valor) > alfa: No rechazar H0 (normal).
8.287e-14>0.05
#Sig(p valor) < alfa: Rechazar H0 (no normal)
8.287e-14<0.05

 
#Otra comprobacion puede ser los outliers altos leverage
# Gráfico residuos estudentizados frente a valores ajustados por el modelo
ggplot(data = Boston, aes(x = predict(modelo_simple), 
                        y = abs(rstudent(modelo_simple))))+
    geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
    geom_point(aes(color = ifelse(abs(rstudent(modelo_simple)) > 3, "red", "black")))+
    scale_color_identity()+
    labs(title = "Distribución de los residuos estudentizados", 
       x = "Predicción modelo", 
       y = "Residuos estudentizados")+
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#--------------VALIDACION DEL MODELO--------------------
#--------------------------------------------------------------
#1. plot(modelo) -> Análisis de los residuos (distribución, variabilidad.)
plot(modelo_simple)
###1.Los residuos confirman que los datos no se distribuyen de forma lineal, ni su varianza constante (plot1).
###2.se observa que la distribución de los residuos no es normal (plot2)
#-------------------------------------------------------------------------
#2. shapiro.test(modelo$residuals) -> Test de hipótesis de Shapiro Wilk para el análisis de normalidad
lstat_test <- shapiro.test(Boston$lstat)
print(lstat_test)
# 1. La variable tiene una correlacion moderada 0.73
# 2. La hipotesis determina normalidad
#----------------------------------------------------------------------------
#3. Calcular valores observaciones extremas e influyentes outliers
# Valores de distancia identifica observaciones respecto a X
# Residuales estudentizados  identifica observaciones respecto a Y
# 
#3.1  Prueba Bopnferroni detectar outliers
outlierTest(modelo_simple)
#Los 4 primeros outliers
outlierTest(modelo_simple, cutoff=Inf, n.max=4)
#Grafica de identificar los outliers
influenceIndexPlot(modelo_simple, vars="Bonf", las=1)
#
#Residuales estudentizados con respecto a Y si un residual es muy diferente a los otros ya se considera sospechoso
# La condicion es rstudenti es mayor o igual a 2  
plot(predict(modelo_simple))

plot(x = modelo_simple$fitted.values, y = abs(rstudent(modelo_simple)))
abline(h = 3, col = "red")

#Gráfico residuos estudentizados frente a valores ajustados por el modelo
ggplot(data = Boston, aes(x = predict(modelo_simple), 
                          y = abs(rstudent(modelo_simple))))+
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
  geom_point(aes(color = ifelse(abs(rstudent(modelo_simple)) > 3, "red", "black")))+
  scale_color_identity()+
  labs(title = "Distribución de los residuos estudentizados", 
       x = "Predicción modelo", 
       y = "Residuos estudentizados")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

## cualquier observación en un conjunto de datos que tenga un residuo estudentizado mayor que un valor absoluto de 3 es un valor atípico
## 
## 3.2  Detección de observaciones influyentes
influence.measures(modelo_simple)
influenceIndexPlot(modelo_simple, vars="Bonf", las=1)
which(rstudent(modelo_simple) > 3)
#
#Distancia de Cook # La distancia Cook identifica valores influyentes
# Un punto influyente tiene impacto sobre el modelo no se deben de retirar se deben identificar y tomar una desicion.
# Como en las graficas anteriores identificamos ya los puntos atipicos se deben analizar pues ya tenemos identificadas las observaciones
cooks.distance(modelo_simple)

cutoff <- 5 / (150-2-2)  # Cota
plot(modelo_simple, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")
#identificamos los puntos influyentes con el # de observacion
par(mfrow=c(1, 2))
plot(modelo_simple, col='deepskyblue4', pch=19)
#se ve mas claramente los valores influyentes
# Si hay una razón de peso para considerarlas como observaciones atípicas, ellas deben salir del modelo. Si por el contrario, no hay nada raro con las observaciones, ellas deben seguir en el modelo

#-------------------------------------------------------------------------------
#4. bptest(modelo) -> Test de contraste de homocedasticidad  de los errores de regresion lineal Breusch-Pagan
#Ho= los errores tienen varianza constante
#Ha= los errores no tienen varianza constante
bptest(modelo_simple)
#data:  modelo_simple
#BP = 15.497, df = 1, p-value = 8.262e-05
8.262e-05<0.05
#se observa que el valor-P es menor que el nivel de significancia usual de 5%, por lo tanto, hay evidencias para decir que NO se cumple la homocedasticidad de los  errores
#---------------------------------------------------------------------------------
#5. vif(modelo) -> Calcula VIFs (factor de inflación de la varianza)
#Cómo identificar la multicolinealidad?
#1 Matriz de correlacion
#2 Pruebas estadisticas
#
corrplot(boston_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

vif(modelo_simple, ) # no funciona por que tiene una sola variable

gvlma(modelo_simple)

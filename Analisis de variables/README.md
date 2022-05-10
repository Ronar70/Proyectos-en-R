# Proyectos-en-R

Analisis de variables

Para iniciar con el modelo de regresion lineal simple y multiple las variables que analizamos deben cumplir condiciones para ser tenidas en cuenta para el modelo.

Condiciones para la regresión lineal múltiple

1. linealidad entre predictores y variable respuesta
  La relación entre ambas variables debe ser lineal. Para comprobarlo se puede     recurrir a: Graficar ambas variables a la vez (scatterplot o diagrama de         dispersión), superponiendo la recta del modelo generado por regresión lineal.
  
2. Distribución Normal de los residuos
  Los residuos se tiene que distribuir de forma normal, con media igual a 0. Esto   se puede comprobar con un histograma, con la distribución de cuantiles          (qqnorm() + qqline()) o con un test de hipótesis de normalidad.

3. Distribución Normal de los residuos (heterocedasticidad)
  La homocedasticidad implica que la varianza se mantenga constante. Puede         analizarse de forma gráfica representando las observaciones en un diagrama de    dispersión y viendo si mantiene una homogeneidad en su dispersión a lo largo     del eje X
  
4. Outliers (valores atípicos)
  Los valores atípicos o extremos  pueden generar una falsa correlación que       realmente no existe, u ocultar una existente. Usamos Boxplot.


5. Independencia, Autocorrelación:
  Las observaciones deben ser independientes unas de otras. Esto es importante     tenerlo en cuenta cuando se trata de mediciones temporales. Puede detectarse     estudiando si los residuos siguen un patrón o tendencia.

6. Colinealidad
  La colinealidad se refiere a la situación en la que dos o más predictores están   estrechamente relacionados uno con otro. Se dice que dos predictores             correlacionados son colineales. La presencia de colinealidad puede suponer un    problema, pues puede dificultar la separación del efecto individual de           variables colineales sobre la variable respuesta.


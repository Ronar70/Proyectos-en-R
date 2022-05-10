# Proyectos-en-R

Analisis de variables

Para iniciar con el modelo de regresion lineal simple y multiple las variables que analizamos deben cumplir condiciones para ser tenidas en cuenta para el modelo.

Condiciones para la regresi�n lineal m�ltiple

1. linealidad entre predictores y variable respuesta
  La relaci�n entre ambas variables debe ser lineal. Para comprobarlo se puede     recurrir a: Graficar ambas variables a la vez (scatterplot o diagrama de         dispersi�n), superponiendo la recta del modelo generado por regresi�n lineal.
  
2. Distribuci�n Normal de los residuos
  Los residuos se tiene que distribuir de forma normal, con media igual a 0. Esto   se puede comprobar con un histograma, con la distribuci�n de cuantiles          (qqnorm() + qqline()) o con un test de hip�tesis de normalidad.

3. Distribuci�n Normal de los residuos (heterocedasticidad)
  La homocedasticidad implica que la varianza se mantenga constante. Puede         analizarse de forma gr�fica representando las observaciones en un diagrama de    dispersi�n y viendo si mantiene una homogeneidad en su dispersi�n a lo largo     del eje X
  
4. Outliers (valores at�picos)
  Los valores at�picos o extremos  pueden generar una falsa correlaci�n que       realmente no existe, u ocultar una existente. Usamos Boxplot.


5. Independencia, Autocorrelaci�n:
  Las observaciones deben ser independientes unas de otras. Esto es importante     tenerlo en cuenta cuando se trata de mediciones temporales. Puede detectarse     estudiando si los residuos siguen un patr�n o tendencia.

6. Colinealidad
  La colinealidad se refiere a la situaci�n en la que dos o m�s predictores est�n   estrechamente relacionados uno con otro. Se dice que dos predictores             correlacionados son colineales. La presencia de colinealidad puede suponer un    problema, pues puede dificultar la separaci�n del efecto individual de           variables colineales sobre la variable respuesta.


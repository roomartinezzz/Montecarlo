# Montecarlo :https://rociomartinez.shinyapps.io/montecarlo_g1/
# Proyecto Montecarlo
Este proyecto en R utiliza el conjunto de datos penguins de la librería palmerpenguins para realizar un análisis exploratorio interactivo y simulaciones de Montecarlo relacionadas con el comportamiento de los pingüinos. A través de una interfaz gráfica desarrollada con Shiny, los usuarios pueden explorar el dataset, generar visualizaciones interactivas y realizar cálculos de probabilidades de apareamiento entre pingüinos de la misma o diferentes especies. A continuación, se describen las funcionalidades clave:

## 1. Preparación de Datos
Se carga y limpia el dataset penguins, eliminando los valores faltantes y ajustando la variable sex para que tenga valores más interpretables ("male" y "female").
El conjunto de datos se presenta en una tabla interactiva utilizando el paquete DT, lo que permite a los usuarios filtrar, buscar y explorar los datos de manera eficiente.
También se genera un resumen estadístico de los datos, que ofrece información básica sobre las variables presentes en el dataset.
## 2. Gráficos Interactivos
Se implementan varias visualizaciones interactivas para representar la distribución de pingüinos por isla, la cantidad de pingüinos por especie y sexo, y la frecuencia de características en el dataset.
La función graficar_especie permite seleccionar una especie y visualizar un gráfico de barras interactivo que muestra la cantidad de pingüinos en cada isla.
Otras visualizaciones incluyen gráficos de barras que muestran la cantidad de pingüinos por isla, por especie y sexo, y gráficos de frecuencias de variables seleccionadas.
## 3. Simulaciones de Probabilidad de Apareamiento
Se calculan las probabilidades de que dos pingüinos de la misma especie tengan éxito en el apareamiento, considerando su sexo. Este cálculo se realiza mediante simulaciones de Montecarlo, en las que se generan parejas aleatorias y se evalúa si son de sexos diferentes.
Además, se calcula la probabilidad de que el apareamiento ocurra entre pingüinos de diferentes especies, nuevamente mediante simulaciones.
## 4. Generación de Reportes Dinámicos
Utilizando Rmarkdown, se genera un reporte HTML dinámico que resume los resultados del análisis. Los usuarios pueden descargar este archivo de reporte o acceder al código fuente .Rmd para personalizarlo según sus necesidades.
## 5. Interactividad y Descarga de Archivos
La aplicación permite a los usuarios interactuar con los gráficos mediante controles de selección y botones para cambiar las especies o variables que se visualizan.
Además, se facilita la descarga de archivos generados, como el archivo Rmarkdown del reporte.
## 6. Uso de Shiny para la Interactividad
El proyecto está construido sobre Shiny, un marco de trabajo en R que permite crear aplicaciones web interactivas. Los usuarios pueden interactuar con los datos y ver los resultados en tiempo real sin necesidad de recargar la página.
# Objetivo del Proyecto
Este proyecto tiene como objetivo proporcionar una forma interactiva de explorar los datos de pingüinos y realizar simulaciones estadísticas simples, lo que permite a los usuarios visualizar patrones en los datos y obtener resultados significativos sobre el comportamiento de los pingüinos, específicamente en relación con su apareamiento. Además, el uso de visualizaciones interactivas facilita la comprensión de las relaciones entre las variables del dataset.

El código es modular y puede ser fácilmente extendido para incluir más análisis o visualizaciones según las necesidades del usuario.

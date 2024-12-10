library(gridExtra)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(corrplot)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(DT)
library(tidyr)
library(shinydashboard)
library(stringr) 
library(reshape2)
library(shinyBS)
library(openxlsx)
library(tidyverse)
library(palmerpenguins)
library(shinyWidgets)

#---------------------------------------------------------------------------------------------------------
color <- c(
  "#A4D8E1",  # Cyan claro
  "#4DA6C4",  # Cyan medio
  "#2E93B8",  # Cyan oscuro
  "#1C7A92",  # Cyan muy oscuro
  "#66CDAA",   #Turquesa Suave
  "#DAF7A6",  # Verde claro,
  "#FF6F61",  # Coral
  "#F7CAC9",  # Rosa claro
  "#77DD77",  # Verde Menta
  "#92A8D1",  # Azul suave
  "#88B04B" ,# Verde manzana
  "#B9D3C1",  # Verde agua
  "#98A8D7",  # Azul suave
  "#82CCDD",  # Aqua
  "#C5E1A5",  # Verde claro (Mate)
  "#FFB143",  # Naranja suave
  "#BFACE2"   # Lavanda
  
)

#------------------------------------------------------------------------------------------------------------
#Dataset

datos <- penguins
data_summary <- summary(datos)
data_str <- capture.output(str(datos))
data_table <- datatable(datos)
datos <- na.omit(datos)
datos <- datos %>%
  mutate(sex = ifelse(sex == 0, "male", "female"))

#--------------------------------------------------------------------------------------------------------

graficar_especie <- function(datos, especie) {
  
  # Filtrar los datos para la especie seleccionada
  data_filtrada <- datos %>%
    filter(species == especie) %>%  
    group_by(island) %>%
    summarise(TotalSpecie = n(), .groups = "drop")
  
  # Mensaje de depuración: imprimir los datos filtrados
  print(data_filtrada)  
  

  if (nrow(data_filtrada) == 0) {
    stop("La especie seleccionada no tiene datos.")
  }
  
  # Paleta de colores pastel
  pastel <- c("#C5E1A5", "#1C7A92", "#2E93B8")
  
  # Crear el gráfico de barras
  p <- ggplot(data_filtrada, aes(x = island, y = TotalSpecie, fill = island)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Islas",
      y = "Cantidad de pingüinos"
    ) +
    theme_minimal() +
    scale_fill_manual(values = pastel)  # Escala de colores para las islas

  
  # Convertir a un gráfico interactivo con ggplotly
  p <- ggplotly(p)
  
  return(p)
}




#------------------------------------------------------------------------------------------------------------------

# Definir la interfaz de usuario
ui <- navbarPage(
  title = "SIMULACIÓN MONTECARLO",
  theme = shinytheme("yeti"),
  
  # Incluir el CSS en el <head>
  tags$head(
    tags$style(HTML("
       /* FONDOS */
    .dataset-background {
      background-color: #f2f2f2 !important; /* Blanco tirando a gris */
      min-height: 100vh; /* Ajusta la altura mínima según el contenido */
      width: 100vw; /* Ocupa toda la anchura de la pantalla */
      position: absolute; /* Asegura que el fondo cubra toda la pantalla */
      top: 0;
      left: 0;
      z-index: -1; /* Para que no interfiera con otros elementos */
    }
      .box {
          border-radius: 10px;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1);
        }
        .action-button {
          background-color: #3c8dbc;
          color: white;
          border-radius: 5px;
        }
        h3 {
          font-family: 'Arial', sans-serif;
        }
    
    /* Fondo transparente para el contenido */
    .transparent-bg {
      background-color: rgba(242, 242, 242) !important; /* Fondo blanco grisáceo con transparencia */
      width: 100vw; /* Ocupa toda la anchura de la pantalla */
      position: absolute; /* Asegura que el fondo cubra toda la pantalla */
      top: 0;
      left: 0;
      z-index: -1; /* Para que no interfiera con otros elementos */
    }
    /* Resto del CSS */
    /* FLIP CARDS */
    .flip-card {
      position: relative;
      z-index: 1;
      width: 100%;
      height: 700px;
      perspective: 1000px;
      margin: 10px;
    }
    .flip-card-inner {
      position: relative;
      width: 100%;
      height: 100%;
      transition: transform 0.8s;
      transform-style: preserve-3d;
    }
    .flip-card:hover .flip-card-inner {
      transform: rotateY(180deg);
    }
    .flip-card-front, .flip-card-back {
      position: absolute;
      width: 100%;
      height: 100%;
      backface-visibility: hidden;
      z-index: 2;
    }
    .flip-card-front {
      background-color: #bbb;
      color: black;
      display: flex;
      justify-content: center;
      align-items: center;
    }
    .flip-card-back {
      background-color: #2980b9;
      color: white;
      transform: rotateY(180deg);
      display: flex;
      justify-content: center;
      align-items: center;
    }
    /* BOTONES INTERACTIVOS */
    .slider-label {
      font-size: 24px;
      font-weight: bold;
      color: #333;
    }

    .member-link:hover {
      background-color: #f0f0f0;
      transform: scale(1.5);
    }
    .icon {
      display: none;
      position: absolute;
      left: 100%;
      top: 0;
      margin-left: 10px;
    }
    .member:hover .icon {
      display: block;
    }
    "))
  )
,
  
  
  
 
  navbarMenu("Teoría", icon = icon('book'),
             tabPanel("Información", 
                      icon = icon('lightbulb'),
                      fluidRow(
                        # Caja principal para explicar ¿Qué es la simulación Montecarlo?
                        box(
                          title = '¿Qué es la simulación Montecarlo?', width = 12, status = 'warning',
                          tags$p(
                            "La simulación de Montecarlo es un tipo de algoritmo computacional que utiliza un muestreo aleatorio repetido para obtener la probabilidad de que ocurra una serie de resultados.",
                            style = "margin-bottom: 15px; text-align: justify; font-size: 16px; line-height: 1.6;"
                          ),
                          tags$p(
                            "La clave de este método está en entender el término ‘simulación’. Realizar una simulación consiste en repetir, o duplicar, las características y comportamientos de un sistema real. Así pues, el objetivo principal de la simulación de Montecarlo es intentar imitar el comportamiento de variables reales para, en la medida de lo posible, analizar o predecir cómo van a evolucionar.",
                            style = "margin-bottom: 15px; text-align: justify; font-size: 16px; line-height: 1.6;"
                          ),
                          tags$br(),
                          img(src = 'metodo.jpg', style = "width:55%; height: auto; display: block; margin: 0 auto 20px auto; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);")
                        ),
                        
                        # Panel de pestañas
                        tabsetPanel(
                          # Pestaña para explicar por qué es importante la simulación de Monte Carlo
                            tabPanel(
                              '¿Por qué es importante?',
                              tags$div(
                                style = "padding: 20px;",  # Añadir padding general para dar espacio en los lados
                                tags$h4("Beneficios de la Simulación de Monte Carlo", style = "color: #2C3E50; text-align: center; font-weight: bold; margin-bottom: 20px;"),
                                
                                # Mejora de la precisión de las predicciones
                                tags$div(
                                  class = "accordion", id = "accordionExample",
                                  tags$div(
                                    class = "card",
                                    tags$div(
                                      class = "card-header", id = "headingOne",
                                      tags$h5(
                                        tags$button(
                                          class = "btn btn-link", type = "button", "data-toggle" = "collapse",
                                          "data-target" = "#collapseOne", "aria-expanded" = "true", "aria-controls" = "collapseOne",
                                          tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"),  # Ícono de línea de predicción
                                          "Mejora la precisión de las predicciones"
                                        )
                                      )
                                    ),
                                    tags$div(
                                      id = "collapseOne", class = "collapse", "aria-labelledby" = "headingOne", "data-parent" = "#accordionExample",
                                      tags$div(
                                        class = "card-body",
                                        tags$p(
                                          "En entornos complejos, las predicciones basadas en suposiciones simples pueden ser inexactas. La simulación de Monte Carlo tiene en cuenta la variabilidad y la incertidumbre, lo que permite obtener predicciones más precisas y realistas.",
                                          style = "text-align: justify;"
                                        )
                                      )
                                    )
                                  ),
                                  
                                  # Evaluación y mitigación de riesgos
                                  tags$div(
                                    class = "card",
                                    tags$div(
                                      class = "card-header", id = "headingTwo",
                                      tags$h5(
                                        tags$button(
                                          class = "btn btn-link collapsed", type = "button", "data-toggle" = "collapse",
                                          "data-target" = "#collapseTwo", "aria-expanded" = "false", "aria-controls" = "collapseTwo",
                                          tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 10px;"),  # Ícono de riesgo
                                          "Evalúa y mitiga riesgos"
                                        )
                                      )
                                    ),
                                    tags$div(
                                      id = "collapseTwo", class = "collapse", "aria-labelledby" = "headingTwo", "data-parent" = "#accordionExample",
                                      tags$div(
                                        class = "card-body",
                                        tags$p(
                                          "Al simular una amplia gama de posibles resultados, la simulación de Monte Carlo identifica y cuantifica los riesgos asociados con una decisión o proyecto. Esto permite a los tomadores de decisiones anticipar problemas potenciales y desarrollar estrategias de mitigación de riesgos adecuadas.",
                                          style = "text-align: justify;"
                                        )
                                      )
                                    )
                                  ),
                                  
                                  # Optimización de la toma de decisiones
                                  tags$div(
                                    class = "card",
                                    tags$div(
                                      class = "card-header", id = "headingThree",
                                      tags$h5(
                                        tags$button(
                                          class = "btn btn-link collapsed", type = "button", "data-toggle" = "collapse",
                                          "data-target" = "#collapseThree", "aria-expanded" = "false", "aria-controls" = "collapseThree",
                                          tags$i(class = "fas fa-cogs", style = "margin-right: 10px;"),  # Ícono de decisión optimizada
                                          "Optimiza la toma de decisiones"
                                        )
                                      )
                                    ),
                                    tags$div(
                                      id = "collapseThree", class = "collapse", "aria-labelledby" = "headingThree", "data-parent" = "#accordionExample",
                                      tags$div(
                                        class = "card-body",
                                        tags$p(
                                          "Al proporcionar una comprensión más completa de las posibles ramificaciones de una decisión, la simulación de Monte Carlo ayuda a los líderes empresariales a tomar decisiones más informadas y estratégicas. Permite evaluar diferentes opciones y seleccionar la que maximice el valor o minimice el riesgo.",
                                          style = "text-align: justify;"
                                        )
                                      )
                                    )
                                  ),
                                  
                                  # Planificación financiera y de proyectos
                                  tags$div(
                                    class = "card",
                                    tags$div(
                                      class = "card-header", id = "headingFour",
                                      tags$h5(
                                        tags$button(
                                          class = "btn btn-link collapsed", type = "button", "data-toggle" = "collapse",
                                          "data-target" = "#collapseFour", "aria-expanded" = "false", "aria-controls" = "collapseFour",
                                          tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"),  # Ícono de planificación
                                          "Facilita la planificación financiera y de proyectos"
                                        )
                                      )
                                    ),
                                    tags$div(
                                      id = "collapseFour", class = "collapse", "aria-labelledby" = "headingFour", "data-parent" = "#accordionExample",
                                      tags$div(
                                        class = "card-body",
                                        tags$p(
                                          "La simulación de Monte Carlo es ampliamente utilizada en la planificación financiera y de proyectos. Permite modelar diferentes escenarios económicos, evaluar la viabilidad financiera de inversiones y estimar la duración y el costo de proyectos complejos.",
                                          style = "text-align: justify;"
                                        )
                                       
                                     )
                                   ))))),
                          tabPanel("Ventajas y desventajas",
                                   tags$div(
                                     style = "padding-left: 10px; padding-right: 10px;",
                                     tags$style(HTML("
        .card {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          margin-bottom: 20px;
          transition: transform 0.3s ease;
        }
        .card:hover {
          transform: scale(1.02);
        }
        .card-header {
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .card-content {
          margin-left: 15px;
          color: #666;
        }
        .card-icon {
          font-size: 24px;
          margin-right: 10px;
          color: #007bff;
        }
      "))
                                   ),
                                   
                                   fluidRow(
                                     column(6,
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("check-circle", class = "card-icon"), 
                                                       "Permite modelar sistemas complejos"),
                                              tags$div(class = "card-content", 
                                                       "Modela eficazmente sistemas con múltiples variables no lineales.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("check-circle", class = "card-icon"), 
                                                       "Maneja incertidumbre eficazmente"),
                                              tags$div(class = "card-content", 
                                                       "Genera múltiples escenarios aleatorios para manejar incertidumbre.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("check-circle", class = "card-icon"), 
                                                       "Proporciona estimaciones de probabilidades"),
                                              tags$div(class = "card-content", 
                                                       "Facilita la evaluación de riesgos mediante estimaciones de probabilidad.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("check-circle", class = "card-icon"), 
                                                       "Es altamente flexible y adaptable"),
                                              tags$div(class = "card-content", 
                                                       "Puede aplicarse en una amplia variedad de problemas en diferentes campos.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("check-circle", class = "card-icon"), 
                                                       "Permite realizar análisis de sensibilidad"),
                                              tags$div(class = "card-content", 
                                                       "Ayuda a mejorar la confiabilidad de las conclusiones mediante la validación y análisis de sensibilidad.")
                                            )
                                     ),
                                     
                                     column(6,
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("times-circle", class = "card-icon"), 
                                                       "Requiere conocimientos sólidos de estadísticas"),
                                              tags$div(class = "card-content", 
                                                       "Su implementación adecuada requiere un dominio avanzado de estadísticas.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("times-circle", class = "card-icon"), 
                                                       "Computacionalmente intensivo"),
                                              tags$div(class = "card-content", 
                                                       "Simulaciones extensas pueden requerir mucho tiempo de cálculo.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("times-circle", class = "card-icon"), 
                                                       "Precisión depende de las distribuciones de probabilidad"),
                                              tags$div(class = "card-content", 
                                                       "Los resultados dependen en gran medida de la precisión de las distribuciones de probabilidad y modelos utilizados.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("times-circle", class = "card-icon"), 
                                                       "Interpretación compleja"),
                                              tags$div(class = "card-content", 
                                                       "La interpretación de los resultados puede requerir experiencia para una aplicación efectiva.")
                                            ),
                                            tags$div(
                                              class = "card",
                                              tags$div(class = "card-header", 
                                                       icon("times-circle", class = "card-icon"), 
                                                       "Riesgo de decisiones erróneas"),
                                              tags$div(class = "card-content", 
                                                       "La sobreestimación o subestimación de la incertidumbre puede llevar a decisiones erróneas.")
                                            )
                                       )
                                     ))        ,
                          
                          tabPanel("Cómo usar los métodos?",
                                   tags$div(
                                     style = "padding: 20px;",  
                                     
                                     tags$h4("Pasos básicos para aplicar Montecarlo", style = "color: #2C3E50; text-align: center; font-weight: bold; margin-bottom: 20px;"),
                                     
                                     # Creación de pestañas
                                     tags$div(
                                       class = "nav nav-tabs", id = "montecarloTab", role = "tablist",
                                       
                                       # Pestaña 1: Configuración del modelo
                                       tags$a(
                                         class = "nav-item nav-link active", id = "tab1", "data-toggle" = "tab", href = "#step1", role = "tab", "aria-controls" = "step1", "aria-selected" = "true",
                                         tags$i(class = "fas fa-cogs", style = "margin-right: 10px;"), "Configurar el Modelo Predictivo"
                                       ),
                                       
                                       # Pestaña 2: Especificación de distribuciones
                                       tags$a(
                                         class = "nav-item nav-link", id = "tab2", "data-toggle" = "tab", href = "#step2", role = "tab", "aria-controls" = "step2", "aria-selected" = "false",
                                         tags$i(class = "fas fa-chart-bar", style = "margin-right: 10px;"), "Especificar Distribuciones de Probabilidad"
                                       ),
                                       
                                       # Pestaña 3: Ejecutar Simulaciones
                                       tags$a(
                                         class = "nav-item nav-link", id = "tab3", "data-toggle" = "tab", href = "#step3", role = "tab", "aria-controls" = "step3", "aria-selected" = "false",
                                         tags$i(class = "fas fa-play-circle", style = "margin-right: 10px;"), "Ejecutar Simulaciones"
                                       ),
                                       
                                       # Pestaña 4: Modificación y Análisis
                                       tags$a(
                                         class = "nav-item nav-link", id = "tab4", "data-toggle" = "tab", href = "#step4", role = "tab", "aria-controls" = "step4", "aria-selected" = "false",
                                         tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"), "Modificación y Análisis"
                                       )
                                     ),
                                     
                                     # Contenido de las pestañas
                                     tags$div(
                                       class = "tab-content", id = "montecarloTabContent",
                                       
                                       # Contenido de la primera pestaña
                                       tags$div(
                                         class = "tab-pane fade show active", id = "step1", role = "tabpanel", "aria-labelledby" = "tab1",
                                         tags$h5("Configurar el Modelo Predictivo", style = "color: #2980B9; font-weight: bold; margin-top: 15px; margin-bottom: 10px;"),
                                         tags$p(
                                           "Configure el modelo predictivo, identificando tanto la variable dependiente que debe predecirse como las variables independientes (también conocidas como variables de entrada, de riesgo o predictoras) que impulsarán la predicción.",
                                           style = "text-align: justify;"
                                         )
                                       ),
                                       
                                       # Contenido de la segunda pestaña
                                       tags$div(
                                         class = "tab-pane fade", id = "step2", role = "tabpanel", "aria-labelledby" = "tab2",
                                         tags$h5("Especificar Distribuciones de Probabilidad", style = "color: #2980B9; font-weight: bold; margin-top: 5px; margin-bottom: 10px;"), # margen inferior agregado
                                         tags$p(
                                           "Especifique distribuciones de probabilidad de las variables independientes. Utilizar datos históricos y/o el juicio subjetivo del analista para definir una gama de valores probables y asignar pesos de probabilidad a cada uno.",
                                           style = "text-align: justify;"
                                         )
                                       ),
                                       
                                       # Contenido de la tercera pestaña
                                       tags$div(
                                         class = "tab-pane fade", id = "step3", role = "tabpanel", "aria-labelledby" = "tab3",
                                         tags$h5("Ejecutar Simulaciones", style = "color: #2980B9; font-weight: bold; margin-top: 5px; margin-bottom: 10px;"), # margen inferior agregado
                                         tags$p(
                                           "Ejecute simulaciones de manera repetida, generando valores aleatorios de las variables independientes. Haga esto hasta que se reúnan suficientes resultados para formar una muestra representativa del número casi infinito de combinaciones posibles.",
                                           style = "text-align: justify;"
                                         )
                                       ),
                                       
                                       # Contenido de la cuarta pestaña
                                       tags$div(
                                         class = "tab-pane fade", id = "step4", role = "tabpanel", "aria-labelledby" = "tab4",
                                         tags$h5("Modificación y Análisis de Resultados", style = "color: #2980B9; font-weight: bold; margin-top: 5px; margin-bottom: 10px;"), # margen inferior agregado
                                         tags$p(
                                           "Puede realizar tantas simulaciones Montecarlo como desee modificando los parámetros subyacentes que utiliza para simular los datos. Sin embargo, también querrá calcular el rango de variación dentro de una muestra mediante el cálculo de la varianza y la desviación típica, que son medidas de dispersión utilizadas habitualmente.",
                                           style = "text-align: justify;"
                                         ),
                                         tags$br(),
                                         tags$p(
                                           "La varianza de una variable dada es el valor esperado de la diferencia al cuadrado entre la variable y su valor esperado. La desviación típica es la raíz cuadrada de la varianza. Normalmente, las variaciones más pequeñas se consideran mejores.",
                                           style = "text-align: justify;"
                                         )
                                       )
                                     )
                                   )
                          ),
                          
                          tabPanel("Aplicaciones",
                                   tags$div(
                                     style = "padding-left: 20px; padding-right: 20px;",
                                     tags$h4("Aplicaciones del Método de Monte Carlo", style = "color: #2C3E50; text-align: center; font-weight: bold; margin-bottom: 20px;"),
                                     tags$br(),
                                     tags$p("El método de Monte Carlo tiene una amplia gama de aplicaciones en diversos campos:", 
                                            style = "margin-bottom: 20px; text-align: justify;"),
                                     tags$br(),
                                     tags$ul(
                                       # Crear una lista de aplicaciones con sus descripciones
                                       lapply(list(
                                         list(title = "Economía y Finanzas", description = "Pronóstico y valoración de opciones, gestión de riesgos de portafolios."),
                                         list(title = "Ciencias de la Computación", description = "Optimización de algoritmos complejos y simulaciones en inteligencia artificial."),
                                         list(title = "Investigación Operativa", description = "Simulación de procesos y análisis de colas en sistemas de atención al cliente."),
                                         list(title = "Ciencias Naturales", description = "Modelado de sistemas físicos y biológicos, incluyendo la propagación de enfermedades."),
                                         list(title = "Ingeniería", description = "Análisis de confiabilidad y diseño de experimentos para sistemas complejos."),
                                         list(title = "Telecomunicaciones", description = "Modelado y optimización del tráfico en redes de comunicación."),
                                         list(title = "Medicina", description = "Simulación de ensayos clínicos y análisis de datos de salud."),
                                         list(title = "Proyectos de Ingeniería y Construcción", description = "Estimación de costos y tiempos para proyectos complejos."),
                                         list(title = "Juegos y Simulaciones", description = "Análisis de juegos de azar y simulaciones realistas en entornos de entretenimiento.")
                                       ), function(item) {
                                         tags$li(
                                           style = "margin-bottom: 15px; background-color: #f9f9f9; border-left: 5px solid #0056b3; padding: 10px; border-radius: 5px; transition: background-color 0.3s ease;",
                                           tags$b(item$title, style = "color: #0056b3;"), 
                                           tags$br(),
                                           tags$p(item$description, style = "margin-top: 5px; text-align: justify;")
                                         )
                                       })
                                     ),
                                     tags$br(),
                                     tags$p("Estas aplicaciones demuestran la versatilidad del método de Monte Carlo en la resolución de problemas complejos y la toma de decisiones informadas.", 
                                            style = "margin-bottom: 10px; text-align: justify;"),
                                     tags$style(HTML("
             li:hover {
               background-color: #e0e0e0;
               cursor: pointer;
             }
           "))
                                   )
                          )))))
  
  ,
  
  
  # Pestaña Datos con subpestañas
  navbarMenu("Datos", icon = icon('database'),
             tabPanel("Dataset", 
                      icon = icon('table'),
                      fluidRow(
                        box(
                          title = 'Descripción del Dataset de Pingüinos',
                          width = 12,
                          status = 'primary',
                          solidHeader = TRUE,
                          
                          fluidRow(
                            # Columna izquierda para la lista de características
                            column(width = 6,
                                   tags$div(
                                     tags$h4("Características de los Datos", style = "color: #0073e6; margin-bottom: 15px;"),
                                     tags$ul(
                                       style = "list-style-type: none; padding-left: 0;",
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Species:"), " La especie del pingüino (Adelie, Chinstrap, Gentoo)."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Island:"), " La isla donde se recolectaron los datos (Biscoe, Dream, Torgersen)."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Bill Length (mm):"), " Longitud del pico del pingüino en milímetros."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Bill Depth (mm):"), " Profundidad del pico en milímetros."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Flipper Length (mm):"), " Longitud de la aleta en milímetros."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Body Mass (g):"), " Masa corporal del pingüino en gramos."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Sex:"), " Sexo del pingüino (Male, Female)."
                                       ),
                                       tags$li(style = "margin-bottom: 10px;",
                                               tags$b("Year:"), " Año en que se recolectaron los datos (2007, 2008, 2009)."
                                       )
                                     )
                                   )
                            ),
                            
                            # Columna derecha para las imágenes de especies de pingüinos
                            column(width = 6,
                                   tags$div(
                                     style = "text-align: center;",
                                     tags$h4("Especies de Pingüinos", style = "color: #0073e6; margin-bottom: 15px;"),
                                     uiOutput("penguinImage"), # Espacio dinámico para imagen
                                     tags$br(),
                                     tags$p("Selecciona una especie para explorar más sobre estos fascinantes pingüinos!", 
                                            style = "font-style: italic; color: #777;")
                                   ),
                                   
                                   # Botones de selección de especies
                                   tags$div(
                                     style = "text-align: center; margin-top: 15px;",
                                     actionButton("btn_adelie", "Adelie", class = "btn-primary"),
                                     actionButton("btn_chinstrap", "Chinstrap", class = "btn-primary"),
                                     actionButton("btn_gentoo", "Gentoo", class = "btn-primary")
                                   )
                            )
                          )
                        )
                      )
             
  
             
  ,
                          
                          
  fluidRow(
    box(title = 'Tabla de Datos', 
        width = 6, 
        status = 'warning', 
        DTOutput('data_table'), 
        class = "box_custom"
    ),
    box( title=" ",
        width = 6, 
        status = 'info', 
        solidHeader = TRUE,
        style = "text-align: center;",
        tags$img(src = "ping.gif", 
                 style = "max-width: 100%; height: auto; border-radius: 10px;")))
  
  ,
                          tags$br(),
                          fluidRow(
                            box(title = 'Forma de los Datos', width = 6, status = 'warning', verbatimTextOutput('data_str'), class="box_custom"),
                            box(title = 'Estadísticas Descriptivas', width = 6, status = 'warning', verbatimTextOutput('data_summary'), class="box_custom")
                          ) )
                      ),
  
 
  # Panel de Análisis Exploratorio de Datos
tabPanel("EDA", 
         icon = icon('search'),
         fluidRow(
           
           # Etiqueta y Caja para Gráfico de Pingüinos por Isla para la Especie Seleccionada
           column(
             width = 6,
             style = "margin-bottom: text-align: center;",  # Alineación centrada
             tags$h3("Cantidad de Pingüinos por Isla según Especie"),
             box(
               width = 12, 
               status = 'warning', 
               height = "500px", 
               solidHeader = TRUE,
               selectInput("selected_species", 
                           label = "Seleccionar Especie:",
                           choices = c("Adelie", "Chinstrap", "Gentoo"),
                           selected = "Adelie"),
               plotlyOutput("graficar_especie")
             )
           ),
           
           # Etiqueta y Gráfico Cantidad Total de Pingüinos por Isla
           column(
             width = 6,
             style = "margin-bottom: text-align: center;",  # Alineación centrada
             tags$h3("Cantidad Total de Pingüinos por Isla"),
             box(
               width = 12,
               status = 'info',
               height = "500px",
               solidHeader = TRUE,
               plotlyOutput("plot_islas")
             )
           )
         ),
         
         fluidRow(
           # Etiqueta y Gráfico de Pingüinos por Especie y Sexo
           column(
             width = 6,
             style = "margin-bottom: 30px;",
             tags$h3("Cantidad de Pingüinos por Especie y Sexo"),
             box(
               width = 12,
               status = 'info',
               height = "500px",
               solidHeader = TRUE,
               plotlyOutput("plot_cantEspecSex")
             )
           ),
           
           # Etiqueta y Gráfico de Pingüinos por Sexo
           column(
             width = 6,
             style = "margin-bottom: 30px;",
             tags$h3("Cantidad de Pingüinos por Sexo"),
             box(
                width = 12,
               status = 'info',
               height = "500px",
               solidHeader = TRUE,
               plotlyOutput("plot_sex")
             )
           ),
           
           fluidRow(
             # Selector de variable
             column(
               width = 12,
               style = "margin-bottom: 30px;",
               box(
                 title = "Seleccionar Variable para Frecuencia",
                 width = 6,
                 status = 'warning',
                 solidHeader = TRUE,
                 selectInput("selected_variable", 
                             label = "Seleccionar Variable:",
                             choices = c("Longitud del Pico" = "bill_length_mm",
                                         "Profundidad del Pico" = "bill_depth_mm",
                                         "Longitud de la Aleta" = "flipper_length_mm",
                                         "Masa Corporal" = "body_mass_g"),
                             selected = "bill_length_mm")
               )
             ),
             
             # Gráfico de Frecuencias
             column(
               width = 12,
               style = "margin-bottom: 30px;",
               box(
                 title = "Frecuencia de la variable seleccionada",
                 width = 12,
                 status = 'info',
                 height = "500px",
                 solidHeader = TRUE,
                 plotlyOutput("plot_variable_frecuencia")
         )
)))),
            
                   
  


tabPanel("Simulaciones",
         tabsetPanel( 
           tabPanel( "Simulación 1", 
                     fluidRow(
                       box( width = 12, 
                            title = tags$h3("Probabilidad de Apareamiento entre Pingüinos de la Misma Especie", style = "font-weight: bold; color: #3c8dbc;"), 
                            status = "primary", 
                            solidHeader = TRUE, 
                            # Aquí puedes agregar más contenidos específicos de "Consigna 1"
                            
                            selectInput("especieSeleccionada", 
                                        "Seleccione la especie:",
                                        choices = unique(penguins$species), 
                                        selected = unique(penguins$species)[1]), # Valor inicial
                            
                            # Botón para ejecutar la simulación
                            actionButton("calcular1", "Calcular Probabilidad")
                       )
                     ),
                     
                     fluidRow(
                       box( width = 12,
                            title = NULL,
                            status = "primary", 
                            solidHeader = TRUE, 
                            valueBoxOutput("resultado1_box", width = 6)
                       )
                     )
           ),
           
           tabPanel( "Simulación 2", 
                     fluidRow(
                       box( width = 12, 
                            title = tags$h3("Probabilidad de Apareamiento entre Pingüinos de distintas Especies", style = "font-weight: bold; color: #3c8dbc;"), 
                            status = "primary", 
                            solidHeader = TRUE, 
                            # Aquí puedes agregar más contenidos específicos de "Consigna 1"
                            
                            selectInput("especieSeleccionada1", 
                                        "Seleccione la especie:",
                                        choices = unique(penguins$species), 
                                        selected = unique(penguins$species)[1]), # Valor inicial
                            
                            selectInput("especieSeleccionada2", 
                                        "Seleccione otra especie:",
                                        choices = unique(penguins$species), 
                                        selected = unique(penguins$species)[2]), # Valor inicial
                            
                            # Botón para ejecutar la simulación
                            actionButton("calcular2", "Calcular Probabilidad")
                       )
                     ),
                     
                     fluidRow(
                       box( width = 12,
                            title = NULL,
                            status = "primary", 
                            solidHeader = TRUE, 
                            valueBoxOutput("resultado2_box", width = 6)
                             ) ))
           
           
           
           ))
                              
                             
                               
                 )
    
           
                                                   

   



## Define server logic
server <- function(input, output ,session) {
  
  observe({
    rmd_file <- "Montecarlo.Rmd"
    html_file <- "www/report.html"
    
    # Verifica si el archivo RMD existe
    if (!file.exists(rmd_file)) {
      print("El archivo RMD no se encuentra.")
    }
    
    # Renderiza el archivo HTML si no existe
    if (!file.exists(html_file)) {
      tryCatch({
        rmarkdown::render(rmd_file, output_format = "html_document", output_file = html_file)
        print(paste("HTML generado exitosamente:", html_file))
      }, error = function(e) {
        print(paste("Error al renderizar el archivo RMD:", e$message))
      })
    }
  })
  output$download_rmd <- downloadHandler(
    filename = function() {
      paste("Montecarlo", ".Rmd", sep = "")
    },
    content = function(file) {
      # Verifica si el archivo existe antes de copiar
      if (file.exists("Montecarlo.Rmd")) {
        file.copy("Montecarlo.Rmd", file)
        print("Archivo RMD descargado correctamente.")
      } else {
        print("El archivo RMD no se encuentra.")
      }
    }
  )
  
  output$data_str <- renderPrint({str(datos)})
  output$data_summary <- renderPrint({summary(datos)})
  output$data_table <- renderDT({
    datatable(datos, options = list(
      scrollX = TRUE, pageLength = 5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')  })
  
  
  # Variable reactiva para el estado de la imagen
  selected_species <- reactiveVal("Adelie.jpg")
  
  # Observadores para cambiar la imagen según el botón seleccionado
  observeEvent(input$btn_adelie, {
    selected_species("Adelie.jpg")
  })
  
  observeEvent(input$btn_chinstrap, {
    selected_species("Chinstrap.jpg")
  })
  
  observeEvent(input$btn_gentoo, {
    selected_species("Gentoo.jpg")
  })
  
  # Output para mostrar la imagen seleccionada
  output$penguinImage <- renderUI({
    tags$img(src = selected_species(), 
             style = "max-width: 100%; height: auto; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
  })

  
  
  
  
  
  #EDA
 #-------------------------------------------------------------------------------------------------------- 
  

  output$graficar_especie <- renderPlotly({
    selected_especie <- input$selected_species  
    
    # Llamar a la función de graficar_producto y pasar el producto seleccionado
    especie_plot <- graficar_especie(datos, selected_especie)
    
    return(especie_plot)  # Retornar el gráfico interactivo
  })
  
  # Gráfico de Cantidad Total de Pingüinos por Isla
  output$plot_islas <- renderPlotly({
    islas <- ggplot(data = datos, aes(x = island)) +
      geom_bar(fill = "#2E93B8") + 
      labs(x = "Isla", y = "Cantidad de Pingüinos") +
      theme_minimal()
    ggplotly(islas)
  })
  
  # Gráfico de Cantidad de Pingüinos por Especie y Sexo
  output$plot_cantEspecSex <- renderPlotly({
    graf <- ggplot(data = datos, aes(x = species, fill = sex)) +
      geom_bar(position = "dodge") +
      labs(x = "Especie", y = "Cantidad de Pingüinos", fill = "Sexo") +
      theme_minimal() +
      scale_fill_manual(values = c("#2E93B8", "#FFB6C1"))  
    ggplotly(graf)
  })
  
  # Gráfico de Cantidad de Pingüinos por Sexo
  output$plot_sex <- renderPlotly({
    sex <- ggplot(data = datos, aes(x = sex)) +
      geom_bar(aes(fill = sex)) +  # Agregar 'fill = sex' para usar colores automáticos
      labs( x = "Sexo", y = "Cantidad de Pingüinos") +
      theme_minimal() +
      scale_fill_manual(values = c("#2E93B8", "#FFB6C1"))  
    ggplotly(sex)
  })
  
  output$plot_variable_frecuencia <- renderPlotly({
    # Obtener el nombre de la variable seleccionada
    variable <- input$selected_variable
    
    # Crear el gráfico de frecuencias
    grafico_frecuencia <- ggplot(data = datos, aes_string(x = variable)) +
      geom_histogram(bins = 30, fill = "#2E93B8", color = "white") + 
      labs(
           x = variable,
           y = "Frecuencia") +
      theme_minimal()
    
    # Convertir a un gráfico interactivo
    ggplotly(grafico_frecuencia)
  })
  
  
  
  
  
#------------------------------------------------------------------------------------------------
  # Consigna 1: Probabilidad de apareamiento de una misma especie
  observeEvent(input$calcular1, {
    # Filtrar los datos
    pinguinos <- penguins %>%
      filter(species == input$especieSeleccionada, !is.na(sex))
    
    sexos_disponibles <- unique(pinguinos$sex)
    
    if (length(sexos_disponibles) < 2) {
      probabilidad_exito1 <- 0
      mensaje <- "No hay suficientes pingüinos de ambos sexos para realizar el apareamiento."
    } else {
      num_simulaciones <- 10000
      resultado_apareamiento <- numeric(num_simulaciones)
      for (i in 1:num_simulaciones) {
        pareja <- sample(1:nrow(pinguinos), 2)
        sexo_pareja1 <- pinguinos$sex[pareja[1]]
        sexo_pareja2 <- pinguinos$sex[pareja[2]]
        resultado_apareamiento[i] <- ifelse(sexo_pareja1 != sexo_pareja2, 1, 0)
      }
      probabilidad_exito1 <- mean(resultado_apareamiento)
    }
    
    output$resultado1_box <- renderValueBox({
      valueBox(
        paste0(round(probabilidad_exito1 * 100, 2), "%"),
        subtitle = "Probabilidad de éxito",
        color = ifelse(probabilidad_exito1 > 0, "blue", "red")
      )
    })
  })
  
  # Consigna 2: Probabilidad de apareamiento de distintas especies
  observeEvent(input$calcular2, {
    pinguino1 <- penguins %>%
      filter(species == input$especieSeleccionada1, !is.na(sex))
    pinguino2 <- penguins %>%
      filter(species == input$especieSeleccionada2, !is.na(sex))
    
    sexo_disponible1 <- unique(pinguino1$sex)
    sexo_disponible2 <- unique(pinguino2$sex)
    
    if (length(intersect(sexo_disponible1, sexo_disponible2)) < 2) {
      probabilidad_exito2 <- 0
      mensaje <- "No hay suficientes pingüinos de ambos sexos para realizar el apareamiento."
    } else {
      num_simulaciones <- 10000
      resultado_apareamiento2 <- numeric(num_simulaciones)
      for (i in 1:num_simulaciones) {
        pareja1 <- sample(1:nrow(pinguino1), 1)
        pareja2 <- sample(1:nrow(pinguino2), 1)
        sexo_pareja1 <- pinguino1$sex[pareja1]
        sexo_pareja2 <- pinguino2$sex[pareja2]
        resultado_apareamiento2[i] <- ifelse(sexo_pareja1 != sexo_pareja2, 1, 0)
      }
      probabilidad_exito2 <- mean(resultado_apareamiento2)
    }
    
    output$resultado2_box <- renderValueBox({
      valueBox(
        paste0(round(probabilidad_exito2 * 100, 2), "%"),
        subtitle = "Probabilidad de éxito",
        color = ifelse(probabilidad_exito2 > 0, "green", "red")
      )
    })
  })
  
  
      
  
  
  
 
  
}   















# Run the application
shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)

#Lectura de datos
datos <- read.csv("datosProyecto2.csv", header = TRUE, stringsAsFactors = FALSE, sep = ';')
#Lectura de datos sin valores NA
datosCompletos <- na.omit(datos)

#Parte de interfaz gráfica
ui <- fluidPage(
  
  #titulo principal de la página
  title = "Black Friday Dataset Analysis",
  headerPanel("Black Friday Dataset Analysis"),
  
  "2. Summary",
  #crea el comboBox para calcular la estadística
  selectInput(inputId =  "sum", label = "Seleccionar variable para generar estadísticas:", 
              colnames(datos[3:8])),
  #conecta con el servidor
  verbatimTextOutput('summary'),
  
  
  "5. Histograma",
 
   #crea el comboBox para seleccionar la variable
  selectInput(inputId =  "var", label = "Seleccionar variable para generar histograma:", 
              colnames(datosCompletos[3:8])),
  #conecta con el servidor
  plotOutput('hist')
)

server <- function(input, output){
  
  #muestra los datos estadísticos de las variables
  output$summary <- renderPrint({
    summary(datosCompletos[input$sum])
  })
  
  #Genera el gráfico a partir del output del id var
  output$hist <- renderPlot({
    ggplot(data =datosCompletos, aes_string(x = input$var)) +geom_bar()+ ggtitle("Histograma generado a partir de variable")+ 
      xlab(input$var)+theme(panel.background = element_rect(fill = "pink"))
  })
  
}

#Junta el servidor con la gui
shinyApp(ui = ui, server = server)

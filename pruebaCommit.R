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
  
  "5. Histograma",
 
   #crea el comboBox para seleccionar la variable
  selectInput(inputId =  "var", label = "Seleccionar variable para generar histograma:", 
              colnames(datosCompletos[3:8])),
  plotOutput('hist')
)

server <- function(input, output){
  
  #Genera el gráfico a partir del output del id var
  output$hist <- renderPlot({
    ggplot(data =datosCompletos, aes_string(x = input$var)) +geom_bar(width = 0.5)+ ggtitle("Histograma generado a partir de variable")+ 
      xlab(input$var)
  })
  
}

#Junta el servidor con la gui
shinyApp(ui = ui, server = server)
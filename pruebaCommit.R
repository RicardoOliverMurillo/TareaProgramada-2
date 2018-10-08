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
  
  h3("1. Summary"),
  #crea el comboBox para calcular la estadística
  selectInput(inputId =  "sum", label = "Seleccione una variable para generar las estadísticas:", 
              colnames(datos[3:8])),
  #conecta con el servidor
  verbatimTextOutput('summary'),
  
  h3("3. Mostrar N registros completos"),
  #textInput(inputId = "Nregistros", label = "Ingrese la cantidad de registros que desea ver"),
  textInput("Nregistros", "Ingrese la cantidad de registros que desea ver:"),
  actionButton(inputId = "N", "Mostrar registros"),
  dataTableOutput("tablaN"),
  
  h3("4. Histograma"),
 
   #crea el comboBox para seleccionar la variable
  selectInput(inputId =  "var", label = "Seleccione una variable para generar histograma:", 
              colnames(datosCompletos[3:8])),
  #conecta con el servidor
  plotOutput('hist')
)

server <- function(input, output){
  
  #muestra los datos estadísticos de las variables
  output$summary <- renderPrint({
    summary(datosCompletos[input$sum])
  })
  
  #funcion que verifica que la entrada sea mayor a 1 y muestra los registros del número ingresado
  mostrarRegistro <- eventReactive(input$N,{
    x <- as.integer(input$Nregistros)
    print(str(x))
    if(x>=1){
      datosCompletos[1:x,] #selecciona las filas
    }else{ #mensaje de error si el número es menor a 1
      showModal(modalDialog(
        title = "ERROR",
        "Valor debe ser mayor a 1",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  #muestra la tabla con los N datos
  output$tablaN <- renderDataTable({
    mostrarRegistro()
  })
  
  #Genera el gráfico a partir del output del id var
  output$hist <- renderPlot({
    ggplot(data =datosCompletos, aes_string(x = input$var)) +geom_bar()+ ggtitle("Histograma generado a partir de variable")+ 
      xlab(input$var)+theme(panel.background = element_rect(fill = "pink"))
  })
  
}

#Junta el servidor con la gui
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(stringi)


#Lectura de datos
datos <- read.csv("datosProyecto2.csv", header = TRUE, stringsAsFactors = FALSE, sep = ';')
#Lectura de datos sin valores NA
datosCompletos <- na.omit(datos)

print(str(nrow(datos)))
print("datos completos")
print(str(nrow(datosCompletos)))

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
  
  
  
  h3("2. Presentar cantidad de registros"),
  #crea el comboBox para seleccionar el tipo de registro
  selectInput(inputId = "regTotales", label = "Seleccionar el tipo de registro que desea mostrar", choices = c("Registros totales", "Registros completos")),
  #se conecta con el servidor   
  verbatimTextOutput("txtRegistros"),
  
  
  
  h3("3. Mostrar N registros completos"),
  #habilita espacio de texto para ingresar número
  textInput("Nregistros", "Ingrese la cantidad de registros que desea ver:"),
  #boton que muestra los N registros
  actionButton(inputId = "N", "Mostrar registros"),
  #conecta con el servidor
  dataTableOutput("tablaN"),
  
  
  
  h3("4. Histograma"),
  #crea el comboBox para seleccionar la variable
  selectInput(inputId =  "var", label = "Seleccione una variable para generar histograma:", 
              colnames(datosCompletos[3:8])),
  #conecta con el servidor
  plotOutput('hist'),
  
  
  
  h3("5. Gráfico usando facetas"),
  #crea el comboBox para seleccionar el tipo de gráfico
  selectInput(inputId = "fac", label = "Seleccione tipo de gráfico", choices = c("Histograma", "Boxplot")),
  #crea el comboBox para seleccionar la primera variable
  selectInput(inputId = "x", label = "Seleccione primera variable (x)", colnames(datosCompletos[3:8])),
  #crea el comboBox para seleccionar la segunda variable
  selectInput(inputId = "y", label = "Seleccione segunda variable (y)", colnames(datosCompletos[3:8])),
  #crea el comboBox para seleccionar la faceta
  selectInput(inputId = "faceta", label = "Seleccione variable ha representar en la faceta", colnames(datosCompletos[3:8])),
  #crea el comboBox para seleccionar la orientación
  selectInput(inputId = "ver", label = "Seleccione como desea ver el gráfico", choices = c("Columnas", "Filas")),
  #manda a llamar el gráfico
  plotOutput("gFacetas"),
  
  
  
  h3("6. Consulta filtrada"),
  #crea el comboBox para seleccionar el sexo
  selectInput(inputId = "sex", label = "Seleccione el sexo del comprador:", choices = c("M", "F")),
  #Habilita campo de texto para ingresar la cantidad de compras
  textInput("Ncompras", "Ingrese la cantidad de compras:"),
  #Boton que muestra los registros
  actionButton(inputId = "C", "Mostrar registros"),
  #se comunica con el servidor
  dataTableOutput("tablaC")
  
)



# Servidor que muestra los outputs
server <- function(input, output){
  
  #FUNCIÓN 1
  #muestra los datos estadísticos de las variables
  output$summary <- renderPrint({
    summary(datosCompletos[input$sum])
  })
  
  
  #FUNCIÓN 2
  #muestra la cantidad de registros para datos completos y totales
  output$txtRegistros <- renderText({
    if(input$regTotales == "Registros totales"){ #verifica el tipo de registro
      as.character(nrow(datos))
    }else{
      as.character(nrow(datosCompletos))
    }
  })
  
  
  #FUNCIÓN 3
  #funcion que verifica que la entrada sea mayor a 1 y muestra los registros del número ingresado
  mostrarRegistro <- eventReactive(input$N,{
    x <- as.integer(input$Nregistros)
    if(x>=1){
      datosCompletos[1:x,] #selecciona las filas
    }else{ #mensaje de error si el número es menor a 1
      showModal(modalDialog(
        title = "ERROR","Ingrese un valor mayor a 1",easyClose = TRUE,footer = NULL))
    }
  })
  
  #muestra la tabla con los N datos
  output$tablaN <- renderDataTable({
    mostrarRegistro()
  })
  
  
  
  #FUNCION 4
  #Genera el gráfico a partir del output del id var
  output$hist <- renderPlot({
    ggplot(data =datosCompletos, aes_string(x = input$var, fill = input$var )) +geom_bar()+ ggtitle("Histograma")+ 
      xlab(input$var)
  })
  
  
  #FUNCION 5
  #Genera el gráfico 
  output$gFacetas <- renderPlot({
    if(input$fac == "Histograma"){#Verifica el tipo de gráfico que desea visualizar
      if(input$ver == "Columnas"){#verifica el tipo de orientación
        f <- paste('~', input$faceta)#crea la horientazción por columna 
        ggplot(datosCompletos, aes_string(x = input$x, y = input$y, fill = input$x )) + 
          geom_histogram(stat = "identity",position = position_dodge())+ 
          facet_grid(f)+ggtitle("Gráfico por facetas")
      }else{
        f <- paste(input$faceta,'~.')#crea la horientazción por fila 
        ggplot(datosCompletos, aes_string(x = input$x, y = input$y, fill = input$x )) + 
          geom_histogram(stat = "identity",position = position_dodge())+ 
          facet_grid(f)+ggtitle("Gráfico por facetas")
      }
    }
    else{
      if(input$ver == "Columnas"){
        f <- paste('~', input$faceta)
        ggplot(datosCompletos, aes_string(x = , y = input$y, group=input$x)) + geom_boxplot(aes_string(fill=input$x))+ 
          facet_grid(f)
      }else{
        f <- paste(input$faceta,'~.')
        ggplot(datosCompletos, aes_string(x = input$x, y = input$y,group=input$x)) + geom_boxplot(aes_string(fill=input$x))+
          facet_grid(f)
      }
    }
  })
  
  
  #FUNCIÓN 6
  #funcion que verifica el filtro de la consulta
  mostrarFiltro <- eventReactive(input$C, {
    sexo <- input$sex
    cantidad <- as.integer(input$Ncompras)
    if(cantidad>1){
      datosCompletos[datosCompletos$Gender == sexo & datosCompletos$Purchase > cantidad,]
    }else{
      showModal(modalDialog(
        title = "ERROR","Ingrese un valor mayor a 1",easyClose = TRUE,footer = NULL))
    }
  })
  
  #muestra la tabla con los N datos
  output$tablaC <- renderDataTable({
    mostrarFiltro()
  })
}



#Junta el servidor con la gui
shinyApp(ui = ui, server = server)

library(shiny)

resultado <- vector("numeric") #Variable to store the result

############Random Function----------------------------------------------------------
#ARGS:
#     fecha: fecha actual o elegida por el usuario
#     sorteo: nombre del sorteo
#     resultado: vector numerico en el cual se almacena el numero, su longitud depende del sorteo

calcularNumero <- function(fecha,sorteo,resultado){
    fechaAL<- as.numeric(fecha)
    set.seed(fechaAL)
    switch(sorteo,
            "euromillones"={
              combinacion <- c(1:50)
              reintegro <- c(1:12)
              tamC <- 5
              tamR <- 2
            },
            "primitiva"={
              combinacion <- c(1:49)
              reintegro <- c(0:9)
              tamC <- 6
              tamR <- 1 ##reintegro
            },
            "once"={
              combinacion <- c(0:9)
              reintegro <- c(1:55) #Numero de 'serie'
              tamC <- 5
              tamR <- 1
            })
    #sorteo
    resultado1 <-sample(combinacion, replace= FALSE , size=tamC)
    resultado2 <- sample(reintegro, replace = FALSE, size=tamR)
    resultado <- c(resultado1,"-",resultado2)
		return(resultado)
}



#######SHINY APP-------------------------------------------------------------------

#Define UI-----
ui<- fluidPage(
			 				navbarPage("The Lot Now",
													tabPanel("Generator",
														     titlePanel("Generador de numeros"),

														     sidebarLayout(
																							 sidebarPanel(
																														selectInput("lotSelect",
																																							 label= h3("Selecciona el sorteo"),
																																							 choices = list("La Primitiva "="primitiva",
																																															"Euromillones" = "euromillones",
																																															"La Once" = "once"),
																																							 selected = "La Primitiva"),

																														 dateInput("date", "Date:",language="es",weekstart=1,format="dd-mm-yyyy"),

                                                             br(),

                                                             helpText(p("La estructura de los siguientes sorteos es:"),br(),
                                                                                     p("-Euromillones: 6 números del 1 al 50 y uno del 1 al 12"),br(),
                                                                                     p("-La Once: 5 números del 0 al 9 y uno de serie del 1 al 55"),br(),
                                                                                     p("-La Primitiva: 6 números del 0 al 45, 1 complementario del 1 al 45 (aleatorio al comprar) y reintegro del 0 al 9"))


																 							  ),
															            			mainPanel(
                                                          h3("El sorteo seleccionado es"),
                                                          imageOutput("img"),
                                                          #h2(verbatimTextOutput("selected_lotSelect")),
																													h3("El numero adecuado para hoy es "),
                                                          #br(), #Espacio en blanco
																													h3(textOutput("result"),align="center")
																							 )
															  )
													),
													tabPanel("Check"),
													navbarMenu("More",
    																tabPanel("Sub-Component A"),
    																tabPanel("Sub-Component B"))
	     									  )

)

#Define server logic---
server <- function (input,output) {
											  output$selected_lotSelect<- renderText( {
													paste(input$lotSelect)
												})
											 		  func <-reactive({
																					calcularNumero(input$date,input$lotSelect,resultado)
																  	})
												output$result <-renderText({ paste(func())})

                        output$img<- renderImage( {

                          #OPCION 1: Usando if-else, permite decidir el tamaño de cada imagen independentientemente
                          #if(input$lotSelect == "euromillones"){
                          #  list(src = "euromillones.jpg", height = 200, width = 250)
                          #}
                          #else if( input$lotSelect == "once"){
                          #  list(src = "once.jpg", height = 200, width = 250)
                          #}
                          #else if(input$lotSelect == "primitiva"){
                          #  list(src = "primitiva.jpg", height = 200, width = 250)
                          #}
                          #OPCION 2: usando el Path, mas directo
                          filename <- normalizePath(file.path('./images',paste(input$lotSelect,".jpg", sep="")))
                          list(src = filename, height=400, width= 600)

                        },deleteFile=FALSE)


}



#Run the app ----

shinyApp(ui=ui, server=server)

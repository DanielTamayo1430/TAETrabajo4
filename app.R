library(shiny)
library(shinydashboard)
library(magick)
library(imager)
library("EBImage")
library(keras)


pred <- function(img_path) {
  magickObj <- image_read(img_path)%>% image_quantize(colorspace = 'gray') #use magick to read pgm extension
  cimgObj <- magick2cimg(magickObj, alpha = "rm")   #convert magick to cimg, for functionality
  cimgObj <- resize(cimgObj, w = 128, h = 120)
  
  train_images_ANN2 <- array(dim = c(1, 128, 120,1))
  train_images_ANN2[1,,,] <- cimgObj;
  
  preds <- sunglassesModel %>% predict_proba(train_images_ANN2)
  print(preds)
}



server <- shinyServer(function(input, output) {
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  
  
  output$images <- renderUI({
    if(is.null(input$files)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(files()),
             function(i)
             {
               imagename = paste0("image", i)
               #list(imageOutput(imagename),renderText(pred(files()$datapath[i])))
               
               if(pred(files()$datapath[i])[1,][1] < 0.5 && pred(files()$datapath[i])[1,][2] > 0.5){
                 output$resultado2 <- renderText("Resultado: Tiene Gafas")
                 output$resultado <- renderText("")
                 list(imageOutput(imagename,inline=T))
               }
               else{
                 output$resultado2 <- renderText("")
                 output$resultado <- renderText("Resultado: No Tiene Gafas")
                 list(imageOutput(imagename,inline=T))
               }
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    if(is.null(input$files)) return(NULL)
    for (i in 1:nrow(files()))
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        print(imagename)
        #pred(files()$datapath[my_i])
        print(paste0("prediction: ",pred(files()$datapath[my_i])))
        output[[imagename]] <- 
          renderImage({
            list(src = files()$datapath[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
})

ui <- shinyUI(fluidPage(
  titlePanel("Tiene gafas o no ?"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'files', 
                label = 'Seleccione una Imagen',
                multiple = TRUE,
                accept=c('image/png', 'image/jpeg'))
    ),
    mainPanel(
      tableOutput('files'),
      uiOutput('images')
    )
  )
))

jsfile <- "/canvasdrawing.js"

ui <- dashboardPage(
  dashboardHeader(title = "Detectando Digitos ! "),
  skin = "purple",

  dashboardSidebar(
    sidebarMenu(
      menuItem("Detección", tabName = "predicts", icon = icon("eye", lib = "glyphicon")),

      menuItem("Información", tabName = "about", icon = icon("circle-info", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(src = "http://leimi.github.io/drawingboard.js/bower_components/drawingboard.js/dist/drawingboard.min.js"),      
      
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
      tags$link(rel = 'stylesheet', href = '//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css')
    ),
    tabItems(
      tabItem(tabName = "predicts",
              fluidPage(theme = "custom.css",
                titlePanel("Empecemos ahora !"),
                sidebarLayout(
                  sidebarPanel(
                    tags$h1("Digita:"), 
                    
                    tags$div(id = "sketch",tags$canvas(id = "canvas")),
                    actionButton("CanvasClear", "CLEAR"),
                    
                    tags$script(src = jsfile),      
                    
                                        uiOutput('resultado')
                    
                  ),
                  mainPanel(
                    tableOutput('files'),
                    uiOutput('images')
                  )
                )
              )
      ),
      tabItem(tabName = "about",
              tags$div(
                tags$h1("Repositorio:"), 
                tags$br(),
                tags$ul(
                  tags$li("https://github.com/DanielTamayo1430/TAETrabajo4")
                )
              ),
              tags$div(
                tags$h1("Informe:"), 
                tags$br(),
                tags$ul(
                  tags$li("https://github.com/DanielTamayo1430/TAETrabajo4/TrabajoTAE4.pdf")
                )
              ),
              tags$div(
                tags$h1("Esta app se desarrollo por:"), 
                tags$br(),
                tags$ul(
                  tags$li("Daniel Felipe Tamayo Garcia - dftamayog@unal.edu.co")
                )
              )
      )
    )
  )
)

shinyApp(ui=ui,server=server)
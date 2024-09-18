#
# Die shiny-App zeigt, wie Bilder aussehen, nachdem die als Matrix mit einem
# Faltungskern gefaltet wurden.
#

library(shiny)

library(recolorize)
library(OpenImageR)

identity <- c(0, 0, 0, 0, 1, 0, 0, 0, 0)
ridge <- c(0, -1, 0, -1, 4, -1, 0, -1, 0)
edge_detection <- c(-1, -1, -1, -1, 8, -1, -1, -1, -1)
sharpen <-  c(0, -1, 0, -1, 5, -1, 0, -1, 0)
box_blur <- (1/9) * c(1, 1, 1, 1, 1, 1, 1, 1, 1)
horizontal <- c(1, 1, 1, -2, -2, -2, 1, 1, 1)
vertical <- c(1, -2, 1, 1, -2, 1, 1, -2, 1)
horizontal_2 <- c(1, 1, 1, 1, 1, 1, -2, -2, -2)
vertical_2 <- c(1, 1, -2, 1, 1, -2, 1, 1, -2)
relief <- c(-2, -1, 0, -1, 0, 1, 0, 1, 2)

source_list <- list()

source_list$`Zebra.png` <-
  "commons.wikimedia.org/wiki/File:Zebra_(24694097565).jpg, by Mussi Katz"

source_list$`Sternennacht.png` <- "moma.org/collection/works/79802"

source_list$`Noether_retusche_nachcoloriert.png` <-
  "commons.wikimedia.org/wiki/File:Noether_retusche_nachcoloriert.jpg"

source_list$`Albert_Einstein_1921_by_F_Schmutzer.png` <-
  "commons.wikimedia.org/wiki/File:Albert_Einstein_1921_by_F_Schmutzer.jpg, by Ferdinand Schmutzer"

source_list$`Ada_Lovelace.png` <-
  "artcollection.culture.gov.uk/artwork/2172/"

source_list$`ClaudeShannon_MFO3807.jpg` <-
  "opc.mfo.de/detail?photo_id=3807"


# random is defined in the reactive part

# Here we define the UI
ui <- fluidPage(

  # Application title
  titlePanel("Convolution Matrices"),
  fluidRow(
    column(
      width=6,
      tags$h3("Anselm Hudde", class = "title-subtitle", style = "margin-top: -6px;; font-size: 18px;")
    )
  ),

  withMathJax(),

  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "bild",
        label = "Image",
        choices = list(
          Zebra = "Zebra.png",
          `The Starry Night` = "Sternennacht.png",
          `Emmy Noether` = "Noether_retusche_nachcoloriert.png",
          `Albert Einstein` = "Albert_Einstein_1921_by_F_Schmutzer.png",
          `Ada Lovelace` = "Ada_Lovelace.png",
          `Claude Shannon` = "ClaudeShannon_MFO3807.jpg"),
        selected = "Zebra",
        multiple = FALSE)
    )
  ),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "Kernel",
        label = "Kernel",
        choices = list(
          "identity",
          "ridge",
          `edge detection` = "edge_detection",
          "sharpen",
          `box blur` = "box_blur",
          "horizontal",
          "vertical",
          "relief",
          "random"
        ),
        selected = "identity",
        multiple = FALSE)
    )
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("PicturePlot"),
    uiOutput("Kernel")
  )
)

# Here we define the server
server <- function(input, output) {

  output$PicturePlot <- renderPlot({

    random <- rt(9, df = 1)
    random <- random - mean(random) + 1/9

    img <- readImage(paste0("../figures/", input$bild))

    img <- img[, , 1:3]

    if(input$bild == "Zebra.png") {
      nrow = 2
    } else {
      nrow = 1
    }

    layout(matrix(1:2, nrow = nrow))
    par(mar = c(0, 0, 0, 0))
    plotImageArray(img)

    kernel <- matrix(
      get(input$Kernel),
      byrow = TRUE,
      nrow = 3
    )

    img_conv <- convolution(img, kernel)
    img_conv <- img_conv * (img_conv[, , ] > 0)
    img_conv <- img_conv * (img_conv[, , ] < 1) + (img_conv[, , ] >= 1)
    plotImageArray(img_conv[, , ])

    source <- source_list[[input$bild]]

    kernel <- round(kernel, 2)

    output$Kernel <- renderUI({

      withMathJax(paste0(
        "$$\\begin{pmatrix}",
        kernel[1, 1], "&", kernel[1, 2], "&", kernel[1, 3], "\\\\",
        kernel[2, 1], "&", kernel[2, 2], "&", kernel[2, 3], "\\\\",
        kernel[3, 1], "&", kernel[3, 2], "&", kernel[3, 3],
        "\\end{pmatrix}$$",
        "Source: ",
        source
      )
      )
    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)

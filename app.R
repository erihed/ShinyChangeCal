# Min app för byte av kalibreringskurva för en analyt. Appen tar txt-filer som input.

suppressPackageStartupMessages
library(shiny)
library(ggplot2)
library(data.table)
library(ggpubr)
library(here)
# Define UI for data upload app ----
ui <- fluidPage(
    # App title ----
  h1("Change of Calibration Curves"),
  tabsetPanel(
    tabPanel(
      title = "Select Files",
      fileInput("files", "Upload", multiple = TRUE, accept = c(".txt"))),
  
    tabPanel(
      title = "Table",
      dataTableOutput("tbl_out")),
           
    tabPanel(title = "Calibration Curves", 
             plotOutput("plot"),
             downloadButton("downloaddata", "Save Plot"))
      ))
        
      
# Define server logic required to chose files
server <- function(input, output) {
  
  lst1 <- reactive({
    validate(need(input$files != "", "select files..."))
    
    if (is.null(input$files)) {
      return(NULL)
    } else {
      
      path_list <- as.list(input$files$datapath)
      tbl_list <- lapply(input$files$datapath, read.table, header = TRUE, skip = 5, sep="\t")
      
      df <- do.call(rbind, tbl_list)
      
      #' Här behöve jag filtrera listan för att få fram enbart värden med "NY".
      #Skapar ny A1 med alla entries i Sample.Text som innehåller LK eller HK.
      A1 <- df[grep("LK|HK", df$Sample.Text), ]
      
      #Skapar ny A2 med alla entries i Sample.Text som innehåller n
      A2 <- df[grep("Ny|ny|NY", df$Sample.Text), ]
      
      #Skapar ny A3 med alla entries som innehåller N eller n men inte innehåller LK eller HK
      df <- setdiff(A2, A1)
      
      return(df)
    }
  })
  
  output$tbl_out <- renderDataTable({
    lst1()
  })
  
  output$plot <- renderPlot({
    
    linear <- function(k) {
      z <- list(xx = format(coef(k)[1], digits = 2),
                yy = format(abs(coef(k)[2]), digits = 3),
                r2 = format(summary(k)$r.squared, digits = 5));
      if (coef(k)[2] >= 0)  {
        eq <- substitute(italic(y) == xx + yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)
      } else {
        eq <- substitute(italic(y) == xx - yy %.% italic(x)*","~~italic(r)^2~"="~r2,z)   
      }
      as.character(as.expression(eq));               
    }
    
    fo = Std..Conc ~ Conc.
    p <- ggplot(data = lst1(), aes(x = Std..Conc, y = Conc.)) +
      geom_point(alpha = 0.3, color = "red", size = 4) +
      geom_smooth(method="lm",se=FALSE, alpha = 0.5) +
      annotate("text", x = 250, y = 500, label = linear(lm(fo, lst1())), 
               colour="black", size = 5, parse=TRUE)
    print(p)
    
  })
  output$downloaddata <- downloadHandler(
    filename = function() {"plot.jpeg"},
    content = function(file)
      
  ggsave(file, device = "jpeg") 
      
    )}

# Run the application 
shinyApp(ui = ui, server = server)


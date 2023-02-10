# application Shiny
library(shiny)
library(caret)
library(data.table)
library(plotly)
library(knitr)
library(forecast)

# usefull functions 
lapply(list.files("R", full.names = T), source)

# interface Shiny
ui = fluidPage(
  titlePanel("Interpolation de séries temporelles"),
  sidebarLayout(
    sidebarPanel(
      width = 3
      , fileInput("file", "Choose a CSV file")
      , h6("La première colonne doit contenir la date tant dis que la second doit contenir la série à compléter.")
      , selectInput(
        inputId = "method"
        , label = "Choisir la méthode d'interpolation"
        , choices = c(
          "Modèle linéaire" = "lm", 
          "Modèle autorégressif linéaire" = "slm",
          "Modèle semi-linéaire (spline)" = "gam",
          "Modèle autorégressif semi-linéaire" = "spm",
          "Forêt aléatoire" = "rf", 
          "Plus proches voisins" = "knn"
          )
        , selected = "Modèle linéaire")
      , conditionalPanel(
        condition = "input.method == 'rf'"
        , numericInput("ntree", "Nombre d'arbres", 100, min = 1, max = 5000)
        )
      , actionButton("predict", "Soumettre")
      , textOutput("accuracy")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique", plotlyOutput("plot", height = "700px")),
        tabPanel("Données", dataTableOutput("table"))
      )
    )
  )
)

server = function(input, output, session) {
  
  # Observe the predict button and show the modal dialog when it's clicked
  observeEvent(input$predict, { showModal(
    modalDialog(
      title = "Processing",
      "Please wait while the results are being generated.",
      footer = tagList()
    )
  ) }, ignoreInit = T)
  
  # Créer un data.frame complet
  mydata = eventReactive(input$predict, {
    # Read input file 
    inFile = input$file
    # If is.null then use simulated data else read input file 
    if(is.null(inFile)){
      seqDate = seq.Date(as.Date("2008-01-01"), to = Sys.Date(), by = "day")
      data = data.frame(time = seqDate, value = sin(seq(0, 9 * pi, length.out = length(seqDate))) + rnorm(length(seqDate), 0, 0.1))
      missing_rows = sample(1:nrow(data), size = round(nrow(data) * 0.1))
      data = data[-missing_rows, ]
    } else {
      data = read.csv(inFile$datapath, header = TRUE)
      names(data)[1:2] = c("time", "value")
    }
    # Complete data.frame with missing dates
    full_data = complete_data(data)
    # Output 
    full_data
  })
  
  # Obtenir l'accuracy en fonction du modèle choisi
  accuracy = eventReactive(input$predict, {
    # Mesure accuracy throw RMSE
    acc = evaluate_model(input$method, na.omit(mydata()), ntree = input$ntree)
    # Output
    acc
  })
  
  # Interpoler les valeurs manquantes
  imputed_data = eventReactive(input$predict, {
    # Comple missing values 
    data = impute_values(input$method, mydata(), ntree = input$ntree)
    # Remove the modal dialog when the interpolation is complete
    removeModal()
    # Output 
    data
  })
  
  # Afficher l'accuracy
  output$accuracy = renderText({
    if(is.null(mydata())) return(NULL)
    paste("Erreur quadratique moyenne (RMSE):", accuracy())
  })
  
  # Afficher le graphique
  output$plot = renderPlotly({
    if(is.null(mydata())) return(NULL)
    plot_ly(imputed_data(), x = ~time, y = ~value, 
            type = "scatter", mode = "markers", 
            marker = list(color = ifelse(imputed_data()$is_imputed == 1, "red", "blue")))
  })
  
  # Afficher les données
  output$table = renderDataTable({
    if(is.null(mydata())) return(NULL)
    imputed_data()
    }, options = list(pageLength = 15))

}

# Run app
shinyApp(ui, server)

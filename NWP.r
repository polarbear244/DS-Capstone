library(shiny)
library(dplyr)
library(stringr)

# Sample n-gram prediction function (replace with your trained model logic)
predict_next_word <- function(phrase, model_df) {
  phrase <- tolower(phrase)
  last_word <- str_split(phrase, " ")[[1]] %>% tail(1)
  
  candidates <- model_df %>%
    filter(prefix == last_word) %>%
    arrange(desc(probability))
  
  if (nrow(candidates) > 0) {
    return(candidates$next_word[1])
  } else {
    return("No prediction")
  }
}

# Sample bigram model (replace this with your actual trained data)
sample_model <- data.frame(
  prefix = c("the", "in", "on", "for", "data"),
  next_word = c("future", "world", "top", "example", "science"),
  probability = c(0.9, 0.85, 0.8, 0.75, 0.7),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  titlePanel("Predict the Next Word"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter a phrase:", ""),
      actionButton("submit_btn", "Predict")
    ),
    mainPanel(
      h3("Predicted Next Word:"),
      verbatimTextOutput("prediction_output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$submit_btn, {
    phrase <- input$input_text
    predicted <- predict_next_word(phrase, sample_model)
    output$prediction_output <- renderText({ predicted })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

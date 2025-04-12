library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)

# Load recipe data
recipe_data <- read_excel("./dt/World_Cuisine_Recipes.xlsx")

# Helper function to filter recipes
filter_recipes <- function(data, tastes, ingredients) {
  data %>%
    filter(
      str_detect(taste_tags, paste(tastes, collapse = "|")),
      sapply(ingredients, function(ing) str_detect(ingredients, regex(ing, ignore_case = TRUE))) %>% rowSums() > 0
    )
}

ui <- fluidPage(
  titlePanel("🌍 What Should Anna Cook Today?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tastes", "Choose tastes you like:", 
                  choices = unique(unlist(strsplit(recipe_data$taste_tags, ",\\s*"))),
                  multiple = TRUE),
      
      textInput("ingredients", "What ingredients do you have? (comma-separated)", ""),
      actionButton("suggest", "Suggest Recipes!"),
      hr(),
      fileInput("photo_upload", "Upload a photo of your cooked dish"),
      textInput("rating_input", "Give a rating (1 to 5 stars)"),
      actionButton("save_rating", "Save Rating and Photo")
    ),
    
    mainPanel(
      h3("🍽 Suggested Recipes"),
      DTOutput("recipe_table"),
      uiOutput("image_display")
    )
  )
)

server <- function(input, output, session) {
  selected_recipe <- reactiveVal(NULL)
  
  recipe_suggestions <- eventReactive(input$suggest, {
    req(input$tastes)
    req(input$ingredients)
    
    tastes <- input$tastes
    ingredients <- str_split(input$ingredients, ",\\s*")[[1]]
    
    filtered <- filter_recipes(recipe_data, tastes, ingredients)
    
    if (nrow(filtered) == 0) {
      return(data.frame(Message = "No matching recipes found. Try different tastes or ingredients."))
    }
    
    selected_recipe(NULL)  # reset
    filtered
  })
  
  output$recipe_table <- renderDT({
    recipes <- recipe_suggestions()
    datatable(recipes %>% select(food_name, category, cuisine, estimated_time_min, taste_tags, instructions), 
              selection = "single", options = list(pageLength = 5))
  })
  
  observeEvent(input$recipe_table_rows_selected, {
    idx <- input$recipe_table_rows_selected
    if (length(idx) > 0) {
      selected <- recipe_suggestions()[idx, ]
      selected_recipe(selected)
    }
  })
  
  output$image_display <- renderUI({
    req(selected_recipe())
    img_url <- selected_recipe()$image_url
    if (nzchar(img_url)) {
      tags$img(src = img_url, height = "300px")
    } else {
      tags$p("No image uploaded yet.")
    }
  })
  
  observeEvent(input$save_rating, {
    req(selected_recipe())
    idx <- which(recipe_data$food_name == selected_recipe()$food_name)
    
    # Save rating
    recipe_data$user_rating[idx] <<- input$rating_input
    
    # Save uploaded photo (just note path here)
    if (!is.null(input$photo_upload)) {
      save_path <- paste0("www/", input$photo_upload$name)
      file.copy(input$photo_upload$datapath, save_path, overwrite = TRUE)
      recipe_data$image_url[idx] <<- save_path
    }
    
    showNotification("✅ Rating and photo saved!")
  })
}

shinyApp(ui = ui, server = server)

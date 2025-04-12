library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)

# Load the recipe data
recipe_data <- read_excel("./dt/World_Cuisine_Checked_URLs.xlsx")

# Filtering function
filter_recipes <- function(data, tastes, ingredients, cuisine, max_time, category) {
  df <- data
  
  # Filter by cuisine
  if (!is.null(cuisine) && cuisine != "Any") {
    df <- df %>% filter(str_to_lower(cuisine) == str_to_lower(!!cuisine))
  }
  
  # Filter by category
  if (!is.null(category) && category != "Any") {
    df <- df %>% filter(str_to_lower(category) == str_to_lower(!!category))
  }
  
  # Filter by taste tags
  if (!is.null(tastes) && length(tastes) > 0) {
    df <- df %>%
      filter(
        sapply(tastes, function(t) str_detect(taste_tags, regex(t, ignore_case = TRUE))) %>% rowSums() == length(tastes)
      )
  }
  
  # Filter by ingredients
  if (nchar(ingredients) > 0) {
    ing_list <- str_split(ingredients, ",\\s*")[[1]]
    df <- df %>%
      filter(sapply(ing_list, function(ing) str_detect(ingredients, regex(ing, ignore_case = TRUE))) %>% rowSums() > 0)
  }
  
  # Filter by max cooking time
  if (!is.null(max_time)) {
    df <- df %>% filter(estimated_time_min <= max_time)
  }
  
  # Fallback if no result
  if (nrow(df) == 0) {
    df <- data[sample(nrow(data), 5), ]
  }
  
  return(df)
}

# UI
ui <- fluidPage(
  titlePanel("🌍 What Should Anna Cook Today?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tastes", "Choose tastes you like:",
                  choices = unique(unlist(strsplit(paste(recipe_data$taste_tags, collapse = ","), ",\\s*"))),
                  multiple = TRUE),
      
      textInput("ingredients", "What ingredients do you have? (comma-separated, optional)", ""),
      
      selectInput("cuisine", "Choose a cuisine (optional):",
                  choices = c("Any", sort(unique(recipe_data$cuisine))),
                  selected = "Any"),
      
      selectInput("category", "Choose a food category (optional):",
                  choices = c("Any", sort(unique(recipe_data$category))),
                  selected = "Any"),
      
      sliderInput("time_limit", "Maximum cooking time (minutes):", min = 10, max = 120, value = 60, step = 5),
      
      actionButton("suggest", "Suggest Recipes!"),
      hr(),
      fileInput("photo_upload", "Upload a photo of your cooked dish"),
      textInput("rating_input", "Give a rating (1 to 5 stars)"),
      actionButton("save_rating", "Save Rating and Photo")
    ),
    
    mainPanel(
      h3("🍽 Suggested Recipes"),
      DTOutput("recipe_table"),
      br(),
      uiOutput("image_display")
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_recipe <- reactiveVal(NULL)
  
  recipe_suggestions <- eventReactive(input$suggest, {
    filter_recipes(
      recipe_data,
      input$tastes,
      input$ingredients,
      input$cuisine,
      input$time_limit,
      input$category
    )
  })
  
  output$recipe_table <- renderDT({
    recipes <- recipe_suggestions()
    datatable(
      recipes %>% select(food_name, category, cuisine, taste_tags, estimated_time_min, instructions),
      selection = "single",
      options = list(pageLength = 5)
    )
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
    img_url <- selected_recipe()$image_display_url[[1]]
    wiki_url <- selected_recipe()$image_page_url[[1]]
    
    if (!is.null(img_url) && nzchar(img_url)) {
      tagList(
        h4("📸 Recipe Image"),
        tags$img(src = img_url, height = "300px", style = "margin-bottom:10px; border-radius:10px;"),
        br(),
        tags$a("🔗 View on Wikipedia", href = wiki_url, target = "_blank")
      )
    } else {
      tags$p("No image available.")
    }
  })
  
  observeEvent(input$save_rating, {
    req(selected_recipe())
    idx <- which(recipe_data$food_name == selected_recipe()$food_name)
    
    # Save rating
    recipe_data$user_rating[idx] <<- input$rating_input
    
    # Save uploaded photo (if any)
    if (!is.null(input$photo_upload)) {
      save_path <- paste0("www/", input$photo_upload$name)
      file.copy(input$photo_upload$datapath, save_path, overwrite = TRUE)
      recipe_data$image_display_url[idx] <<- save_path
    }
    
    showNotification("✅ Rating and photo saved (not persistent yet).")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

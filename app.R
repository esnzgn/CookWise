library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(DT)
library(fs)
library(digest)
library(rmarkdown)

# Load data
recipe_data <- read_excel("./dt/World_Cuisine_Checked_URLs.xlsx")

# Ensure upload folder and ratings file
dir_create("www/uploads")
ratings_path <- "www/ratings.csv"
if (!file.exists(ratings_path)) {
  write.csv(data.frame(food_name = character(), user_rating = character(), image_file = character(), stringsAsFactors = FALSE),
            ratings_path, row.names = FALSE)
}

# Language dictionary
translate <- function(key, lang) {
  dict <- list(
    "Choose tastes you like:" = c("en" = "Choose tastes you like:", "fi" = "Valitse mieltymykset:"),
    "What ingredients do you have?" = c("en" = "What ingredients do you have?", "fi" = "Mitä aineksia sinulla on?"),
    "Choose a cuisine" = c("en" = "Choose a cuisine", "fi" = "Valitse keittiö"),
    "Choose a food category" = c("en" = "Choose a food category", "fi" = "Valitse ruokalaji"),
    "Maximum cooking time (minutes):" = c("en" = "Maximum cooking time (minutes):", "fi" = "Korkein valmistusaika (minuutteina):"),
    "Suggested Recipes" = c("en" = "Suggested Recipes", "fi" = "Ehdotetut reseptit"),
    "Upload a photo" = c("en" = "Upload a photo", "fi" = "Lataa kuva"),
    "Save Rating & Photo" = c("en" = "Save Rating & Photo", "fi" = "Tallenna arvio ja kuva"),
    "Export to PDF" = c("en" = "Export to PDF", "fi" = "Vie PDF-muodossa")
  )
  return(dict[[key]][[lang]])
}

ui <- navbarPage(
  title = "🌍 What Should I Cook Today?",
  id = "tabs",
  
  tabPanel("Home",
           fluidPage(
             tags$head(
               tags$link(rel = "stylesheet", href = "custom.css"),
               tags$script(HTML("
                 $(document).on('shiny:inputchanged', function(event) {
                   if (event.name === 'darkmode') {
                     $('body').toggleClass('dark-mode', event.value);
                   }
                 });
               "))
             ),
             tags$img(src = "banner.png",
                      style = "width: 100%; height: auto; aspect-ratio: 3 / 1; object-fit: cover; border-radius: 12px; margin-bottom: 10px;"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("lang", "🌐 Language", choices = c("English" = "en", "Finnish" = "fi")),
                 checkboxInput("darkmode", label = "🌙 Dark Mode", FALSE),
                 selectInput("tastes", "", choices = NULL, multiple = TRUE),
                 textInput("ingredients", "", ""),
                 selectInput("cuisine", "", choices = NULL),
                 selectInput("category", "", choices = NULL),
                 sliderInput("time_limit", "", min = 10, max = 120, value = 60),
                 uiOutput("photo_upload_ui"),
                 textInput("rating_input", "⭐ Rating (1-5)", ""),
                 actionButton("suggest", "🍳 Suggest Recipes"),
                 actionButton("save_rating", "💾"),
                 actionButton("export_pdf", "🛒 Export Shopping List")
               ),
               mainPanel(
                 h3(textOutput("suggested_title")),
                 uiOutput("recipe_cards")
               )
             )
           )
  ),
  
  tabPanel("About Us",
           fluidPage(
             h2("👨‍💻 About the Developer"),
             tags$img(src = "my_image.jpg", height = "400px", style = "border-radius: 10px; margin-bottom: 20px;"),
             p("Hi! I’m Ehsan Zangene, a computational biologist and digital creator based in Helsinki. I created this app to help people decide what to cook every day with joy and simplicity."),
             h3("🏝 Built in Suomenlinna, Finland"),
             tags$img(src = "Suomenlinna.jpg", height = "600px", style = "border-radius: 10px; margin-bottom: 10px;"),
             p("This app was lovingly developed on the island of Suomenlinna — a UNESCO World Heritage site just off the coast of Helsinki. Surrounded by sea, stone walls, and serene cafés, it’s where creativity meets calm.")
           )
  )
)

server <- function(input, output, session) {
  observe({
    lang <- input$lang
    updateSelectInput(session, "tastes", label = translate("Choose tastes you like:", lang),
                      choices = unique(unlist(strsplit(paste(recipe_data$taste_tags, collapse = ","), ",\\s*"))))
    updateTextInput(session, "ingredients", label = translate("What ingredients do you have?", lang))
    updateSelectInput(session, "cuisine", label = translate("Choose a cuisine", lang),
                      choices = c("Any", sort(unique(recipe_data$cuisine))))
    updateSelectInput(session, "category", label = translate("Choose a food category", lang),
                      choices = c("Any", sort(unique(recipe_data$category))))
    updateSliderInput(session, "time_limit", label = translate("Maximum cooking time (minutes):", lang))
    updateActionButton(session, "save_rating", label = translate("Save Rating & Photo", lang))
    updateActionButton(session, "export_pdf", label = translate("Export to PDF", lang))
  })
  
  output$photo_upload_ui <- renderUI({
    fileInput("photo_upload", label = translate("Upload a photo", input$lang), accept = c("image/png", "image/jpeg"))
  })
  
  filtered <- eventReactive(input$suggest, {
    df <- recipe_data
    if (input$cuisine != "Any") df <- df %>% filter(str_to_lower(cuisine) == str_to_lower(input$cuisine))
    if (input$category != "Any") df <- df %>% filter(str_to_lower(category) == str_to_lower(input$category))
    if (length(input$tastes) > 0) {
      df <- df %>% filter(rowSums(sapply(input$tastes, function(t) str_detect(taste_tags, regex(t, TRUE)))) == length(input$tastes))
    }
    if (nchar(input$ingredients) > 0) {
      ings <- str_split(input$ingredients, ",\\s*")[[1]]
      df <- df %>% filter(rowSums(sapply(ings, function(i) str_detect(ingredients, regex(i, TRUE)))) > 0)
    }
    df <- df %>% filter(estimated_time_min <= input$time_limit)
    if (nrow(df) == 0) df <- recipe_data[sample(nrow(recipe_data), 5), ]
    df
  })
  
  output$suggested_title <- renderText({ translate("Suggested Recipes", input$lang) })
  
  output$recipe_cards <- renderUI({
    req(filtered())
    lapply(1:nrow(filtered()), function(i) {
      r <- filtered()[i, ]
      img <- if (!is.na(r$image_display_url) && nzchar(r$image_display_url)) r$image_display_url else "https://via.placeholder.com/300x200"
      tags$div(
        style = "border:1px solid #ccc; border-radius:12px; padding:12px; margin-bottom:12px;",
        tags$h4(r$food_name),
        tags$img(src = img, style = "max-width:100%; border-radius:10px;"),
        tags$p(paste0("⏱ ", r$estimated_time_min, " min | ", r$category, " | ", r$cuisine)),
        tags$p(strong("Tastes: "), r$taste_tags),
        tags$p(strong("Instructions: "), r$instructions)
      )
    })
  })
  
  observeEvent(input$save_rating, {
    r <- filtered()[1, ]
    img_path <- ""
    if (!is.null(input$photo_upload)) {
      name <- paste0("upload_", digest(Sys.time()), ".", tools::file_ext(input$photo_upload$name))
      save_path <- file.path("www/uploads", name)
      file.copy(input$photo_upload$datapath, save_path)
      img_path <- paste0("uploads/", name)
    }
    new <- data.frame(food_name = r$food_name, user_rating = input$rating_input, image_file = img_path, stringsAsFactors = FALSE)
    old <- read.csv(ratings_path, stringsAsFactors = FALSE)
    write.csv(bind_rows(old, new), ratings_path, row.names = FALSE)
    showNotification("✅ Saved!")
  })
  
  observeEvent(input$export_pdf, {
    r <- filtered()[1, ]
    params <- list(recipe = r)
    tempReport <- tempfile(fileext = ".Rmd")
    writeLines('
---
output: pdf_document
params:
  recipe: NA
---

# Shopping List

**Dish**: `r params$recipe$food_name`  
**Cuisine**: `r params$recipe$cuisine`  
**Category**: `r params$recipe$category`

## Ingredients
`r params$recipe$ingredients`

## Instructions
`r params$recipe$instructions`
    ', tempReport)
    out <- tempfile(fileext = ".pdf")
    rmarkdown::render(tempReport, output_file = out, params = params, envir = new.env())
    showNotification("📄 PDF exported successfully!")
  })
}

shinyApp(ui, server)

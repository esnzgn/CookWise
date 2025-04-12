# url checker
# Load required packages
library(readxl)
library(httr)
library(dplyr)

# Load your Excel file (update the path if needed)
file_path <- "./dt/World_Cuisine_Full_20Countries_Enhanced.xlsx"
df <- read_excel(file_path)

# Define a function to check a single URL
check_url <- function(url) {
  if (is.na(url) || url == "") return("EMPTY")
  tryCatch({
    res <- httr::HEAD(url, timeout(5))
    if (res$status_code == 200) {
      return("OK")
    } else {
      return(paste("BAD:", res$status_code))
    }
  }, error = function(e) {
    return("ERROR")
  })
}

# Add status columns
df$url_status_display <- sapply(df$image_display_url, check_url)
df$url_status_page <- sapply(df$image_page_url, check_url)

# Save the results to a new Excel file
writexl::write_xlsx(df, "./dt/World_Cuisine_Checked_URLs.xlsx")

cat("✅ URL checking complete. Output saved to 'World_Cuisine_Checked_URLs.xlsx'\n")

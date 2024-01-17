library(httr2)
library(googlesheets4)
library(purrr)

# Function that creates OAuth client for Google APIs
google_client <- function() {
  httr2::oauth_client(
    id = "[Your Client ID]",
    secret = "[Your Client secret]",
    token_url = "https://oauth2.googleapis.com/token",
    name = "itn-metric-google-docs"
  )
}

# Create a new HTTP request
open_agenda_url <- "https://docs.googleapis.com/v1/documents/1sPVsWeBZKn9A8sbfM666aRYwxVq5Emrhp0ws_R5X1n4"
req <- request(open_agenda_url)

# Authenticate and perform request
resp <- req %>%
  req_oauth_auth_code(
    client = google_client(),
    auth_url = "https://accounts.google.com/o/oauth2/v2/auth",
    scope = "https://www.googleapis.com/auth/documents"
  ) %>%
  req_perform()

# Extract content from Google Doc
resp_body_json <- resp %>% resp_body_json()
content <- resp_body_json$body$content

# Iterate and look for list of attendees
result <- c()

# Iterate through content of the document and extracts names of attendees
# by grabbing the values that come after the line 'When you join the meeting, please enter your name and institution below:'
for (ii in seq(1, length(content))) {
  if (!is.null(content[[ii]]$paragraph$elements)) {
    for (jj in seq(1, length(content[[ii]]$paragraph$elements))) {
      if (!is.null(content[[ii]]$paragraph$elements[[jj]]$textRun$content) &&
          (content[[ii]]$paragraph$elements[[jj]]$textRun$content == "When you join the meeting, please enter your name and institution below:\n" |
           content[[ii]]$paragraph$elements[[jj]]$textRun$content == "When you join the meeting, please enter your name and institution below:")) {
        kk <- ii + 1
        while (!is.null(content[[kk]]$paragraph$elements[[1]]$textRun$content) && content[[kk]]$paragraph$elements[[1]]$textRun$content != "\n") {
          result <- c(result, content[[kk]]$paragraph$elements[[1]]$textRun$content)
          kk <- kk + 1
        }
      }
    }
  } else {
    next
  }
}

# Remove '\n' after names
result <- gsub("\n$", "", result)

# Final result
result

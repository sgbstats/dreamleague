library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(progress)

# Base URL for the API
base_url <- "https://interests-api.parliament.uk/api"

# Function to get all current members with pagination
get_all_members <- function() {
  members <- list()
  skip <- 0
  take <- 100 # Increase page size to reduce requests
  
  message("Fetching all current members...")
  
  # First call to get total results
  first_url <- paste0(base_url, "/Members/Search?IsCurrentMember=true&skip=0&take=1")
  first_response <- GET(first_url)
  if (http_status(first_response)$category != "Success") {
      stop("API request failed with status: ", http_status(first_response)$message)
  }
  first_content <- fromJSON(content(first_response, "text"), flatten = TRUE)
  total_results <- first_content$totalResults
  
  pb <- progress_bar$new(
    format = "  downloading members [:bar] :percent in :elapsed",
    total = total_results, clear = FALSE, width= 60)

  while (skip < total_results) {
    url <- paste0(base_url, "/Members/Search?IsCurrentMember=true&skip=", skip, "&take=", take)
    response <- GET(url)
    
    if (http_status(response)$category != "Success") {
      warning("API request failed with status: ", http_status(response)$message)
      skip <- skip + take
      next
    }
    
    if (http_type(response) != "application/json") {
      warning("API did not return JSON")
      skip <- skip + take
      next
    }
    
    content <- fromJSON(content(response, "text"), flatten = TRUE)
    
    if (length(content$items) > 0) {
      members <- append(members, content$items)
      pb$tick(length(content$items))
    }
    
    skip <- skip + take
  }
  
  message(paste("\nFound", length(members), "current members."))
  return(members)
}

# Get all members
members_raw <- get_all_members()

# Create a tibble with member info
members_df <- tibble(
  id = map_chr(members_raw, list("value", "id")),
  name = map_chr(members_raw, list("value", "nameDisplayAs")),
  party = map_chr(members_raw, list("value", "latestParty", "name"))
)

# Function to process interests for a single member
process_member_interests <- function(member_id, member_name, member_party) {
  url <- paste0(base_url, "/Members/", member_id, "/Register")
  response <- GET(url)
  
  # If there are no interests, the API returns 204 No Content
  if (response$status_code == 204) {
    return(NULL)
  }
  
  if (http_status(response)$category != "Success") {
    warning(paste("Failed to fetch interests for member ID:", member_id, "Status:", http_status(response)$message))
    return(NULL)
  }
  
  if (http_type(response) != "application/json") {
    warning(paste("Interest data for member ID:", member_id, "is not JSON."))
    return(NULL)
  }
  
  interests_data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  if (length(interests_data$interests) == 0) {
    return(NULL)
  }
  
  interests_df <- map_dfr(interests_data$interests, ~{
    category <- .x$latestInterest$categoryName
    
    interest_details_list <- .x$latestInterest$interest
    
    # A more robust way to create the "details" string, handling NULLs
    details_str <- imap_chr(interest_details_list, ~{
        if(!is.null(.x) && !is.na(.x)) {
            paste(.y, .x, sep = ": ")
        } else {
            NA_character_
        }
    }) %>%
    na.omit() %>%
    paste(collapse = "; ")
    
    value <- interest_details_list$remuneration
    if (is.null(value)) {
        value <- NA_character_
    }

    tibble(
      name = member_name,
      party = member_party,
      category = category,
      details = details_str,
      value = as.character(value) # Ensure value is character
    )
  })
  
  return(interests_df)
}

# Get interests for all members and combine them
message("Fetching interests for each member...")
pb_interests <- progress_bar$new(
  format = "  fetching interests [:bar] :percent in :elapsed",
  total = nrow(members_df), clear = FALSE, width = 60)

all_interests_list <- vector("list", nrow(members_df))

for (i in 1:nrow(members_df)) {
  all_interests_list[[i]] <- process_member_interests(
    members_df$id[i],
    members_df$name[i],
    members_df$party[i]
  )
  Sys.sleep(0.05) # Be nice to the API
  pb_interests$tick()
}

all_interests_df <- bind_rows(all_interests_list)


# Save to CSV
output_file <- "member_interests.csv"
write_csv(all_interests_df, output_file)

message(paste("\nSuccessfully saved", nrow(all_interests_df), "interests to", output_file))
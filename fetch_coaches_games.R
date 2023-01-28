# Function to get all the coach URLs and return their games
fetch_coaches_games <- function() {
  
  coach.urls <- rvest::read_html(x = "https://afltables.com/afl/stats/coaches/coaches_idx.html") |>
    rvest::html_element(xpath = '//table[@class="sortable"]') |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    sub(pattern = " ",replacement = "%20") # fix Allan La Fontaine link
  
  message(paste("Getting matches for", length(coach.urls), "coaches")) # print start message
  
  pb <- suppressWarnings(dplyr::progress_estimated(length(coach.urls)))  # Create progress bar

  games_list <- coach.urls |>
    purrr::map_df(~ {
      pb$tick()$print() # progress bar updates
      rvest::read_html(x = paste0("http://afltables.com/afl/stats/coaches/", .x)) |>  # read coach page
        rvest::html_nodes("tbody") |># find table elements
        tail(n=1) |> # last table on page
        rvest::html_table() # create table
    }) |>
    dplyr::mutate(
      Coach_ID = cumsum(X1 == "1"),
      Coach = stringi::stri_replace_all_fixed(coach.urls[Coach_ID], c("_","%20","0","1","2",".html"), c(rep(" ",2),rep("",4)),vectorise_all = F),
      Crowd = as.integer(X12),
      Game_Number = as.integer(X1),
      Margin = as.integer(X10),
      Season = as.numeric(substr(X2,nchar(X2)-3,nchar(X2))),
      Round = sub("R", "", sub(",.*","",X2)),
      Team = dplyr::case_when(
        X4 == "Kangaroos" ~ "North Melbourne",
        X4 == "Footscray" ~ "Western Bulldogs",
        X4 == "South Melbourne" ~ "Sydney",
        TRUE ~ X4
      )) |>
    dplyr::select(Coach_ID, Coach, Game_Number, Season, Round, Venue = X11,Team, Team_Score = X6, Oppo = X7, Oppo_Score = X9, Result = X3, Margin, Crowd)
  
  return(games_list)
}

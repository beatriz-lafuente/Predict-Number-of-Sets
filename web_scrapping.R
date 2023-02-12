#### WEB SCRAPPING ####


# Import libraries
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)

# read page
url_base <- "https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate="
page <- url_base %>% read_html()

# create lists to receive all weeks
weeks <- list()

# extract all weeks in rankings page
weeks_pre <- page %>%
  html_nodes(xpath = '//ul[@data-value="rankDate"]') %>%
  html_text()
weeks_pre <- gsub("\\n+", "",
            gsub("\\r+", "",
            gsub("\\s {2,}", " ", weeks_pre))) %>%
            trimws()
            weeks_pre <- gsub("\\.", "-", weeks_pre)
            weeks_pre <- strsplit(weeks_pre, "   ")
            
# concatenate all weeks in list
for (i in weeks_pre) {
  weeks <- c(weeks, i)
}
            
# remove first line (1st week is duplicated)
weeks <- weeks[-1]

# create list to receive all player links
links <- list()

#loop every week and extract every player link
counter <- 1
for (x in weeks) {
  # create URL and read the page for every week
  url <- paste(url_base, x, sep = "")
  page_rankings <- url %>% read_html()
  Sys.sleep(runif(1,0.5,1.0))
  # extract every link
  player_links <- page_rankings %>%
    html_nodes(xpath = '//span[@class="player-cell-wrapper"]//@href') %>%  # no lint
    html_text()
  # concatenate into links list
  for (j in player_links) {
    links <- c(j, links)
  }
  # remove duplicated links
  links <- links[!duplicated(links)]
  # prints every week once its done to keep track
  print(paste0("Done extracting all links from week: ", counter, "/", length(weeks))) # no lint
  counter <- counter + 1
}
# remove duplicate links
distinct_links <- links[!duplicated(links)]

# change the link to point to activity/all games page where are the games
links_activity <- list()
for (i in distinct_links) {
  links_activity <- c(gsub("overview", "player-activity?year=all&matchType=Singles", i), links_activity) # no lint
}

# create matrix with the links
matrix_links <- matrix(links_activity, ncol = 1)

# write to csv
write.csv(matrix1, "links.csv", row.names = FALSE)


# Get games

# create matrix to store all games

matrix <- matrix(ncol = 15)
colnames(matrix) <- c("p_name", "p_height", "p_country", "p_hand", "p_born", "tournament", "tournament_location", "date", "ground", "prize", "round", "result", "score", "opponent", "opponent_rank") # no lint

# turn the text file with links to a data frame

links_activity <- read.csv("links.csv")
names(links_activity)[1] <- "Links"

# main loop: through all links and get information about all games
counter2 <- 1
for (row in 1:nrow(links_activity)) {
  
  #get value of each row from data frame
  link <- links_activity[row, "Links"]
  
  # create URL and read the page with all games of the player
  general_url <- "https://www.atptour.com"
  url_activity_all <- paste(general_url, link, sep = "")
  page_all_games <- url_activity_all %>% read_html()
  Sys.sleep(runif(1,0.5,1.07))
  
  # extract all info from player in the link
  
  # extract name
  
  name <- page_all_games %>%
    html_nodes(xpath = '//div[@class="player-profile-hero-name"]') %>%
    html_text()
  
  name <- gsub("\\n+", "",   # remove \n
               gsub("\\r+", "",
                    gsub("\\s {2,}", " ", name))) %>%
    trimws()
  if (length(name) == 0) {
    name <- NA
  }
  
# extract country code
  
  country <- page_all_games %>% html_nodes(xpath = '//div[@class="player-flag-code"]') %>% html_text() # no lint
  
  if (length(country) == 0) {
    country <- NA
  }
  
# extract height (in cm)
  
  height <- page_all_games %>%
    html_nodes(xpath = '//span[@class="table-height-cm-wrapper"]') %>%
    html_text() %>%
    str_extract("[0-9]+")
  if (length(height) == 0) {
    height <- NA
  }
  
# extract birth date (format yyyy.mm.dd)
  
  birth_date <- page_all_games %>%
    html_nodes(xpath = '//span[@class="table-birthday"]') %>%
    html_text() %>%
    trimws()
  birth_date <- gsub("[()]", "", birth_date)
  if (length(birth_date) == 0) {
    birth_date <- NA
  }
# extract hand
  
  country_hand <- page_all_games %>%
    html_nodes(xpath = '//div[@class="table-value"]') %>%
    html_text() %>%
    trimws()
  country_hand <- country_hand[1:2] # intermediate step to get both the country and hand
  
  hand <- str_extract(country_hand[2], "([^\\,]+)") # get hand played with
  if (length(hand) == 0) {
    hand <- NA
  }
  
# start extracting tournaments/games info
  
# extract tournament info
  
  # all info about the tournament
  
  all_info <- page_all_games %>% html_nodes(xpath = '//div[@class="activity-tournament-table"]')
  all_info <- gsub("\\s{2,}", " ", all_info)
  
  # extract variables from each tournament (loops through every tournament that player played)
  
  for (i in all_info) {
    
    # tournament name
    tourn_name <- str_extract(i, '(?<=data-use-ga=\"true\">)(.*)(?=</a> <span class=\"tourney-location\">)|(?<=<span class="tourney-title">)(.*)(?=</span> <span class=\"tourney-location\">)') %>% trimws() # no lint
    if (length(tourn_name) == 0) {
      tourn_name <- NA
    }
    
    tourn_location <- str_extract(i, '(?<=<span class=\"tourney-location\">)(.*)(?=</span> <span class=\"tourney-dates\">)') %>% trimws() # no lint
    if (length(tourn_location) == 0) {
      tourn_location <- NA
    }
    
    tourn_date <- str_extract(i, '(?<=</span> <span class=\"tourney-dates\">)(.*)(?=</span> </td> <td class=\"tourney-details-table-wrapper\">)') %>% trimws() # no lint
    if (length(tourn_date) == 0) {
      tourn_date <- NA
    }
    
    tourn_surface <- str_extract(i, '(?<=<div class=\"item-details\"> I <span class=\"item-value\">)(.*)(?=</span> </div> </div> </td> <td class=\"tourney-details prize-money\">)|(?<=<div class=\"item-details\"> O <span class=\"item-value\">)(.*)(?=</span> </div> </div> </td> <td class=\"tourney-details prize-money\">)|(?<=<div class=\"item-details\"> I <span class=\"item-value\">)(.*)(?=</span> </div> </div> </td> <td class=\"tourney-details prize-money\">)|(?<=<div class=\"item-details\"> O <span class=\"item-value\">)(.*)(?=</span> </div> </div> </td> <td class=\"tourney-details fin-commit\">)') %>% trimws() # no lint
    if (length(tourn_surface) == 0) {
      tourn_surface <- NA
    }
    
    tourn_prize <- str_extract(i, '(?<=Prize Money </div> </div> <div class=\"info-area\"> <div class=\"item-details\"> <span class=\"item-value\">)(.*)(?=</span> </div> </div> </td> <td class=\"tourney-details fin-commit\">)') %>% trimws() # no lint
    if (length(tourn_prize) == 0) {
      tourn_prize <- NA
    }
    
    # get only the info from the games section
    
    games <- str_extract_all(i, '(?<=\n<tr>\n<td>)(.*)(?=</a> </td> </tr>)|(?<=<tr>\n<td>)(.*)(?=</a> </td> </tr>)') # no lint
    
    # for each game get the variables (loops through every game in each tournament) # no lint
    
    for (x in games[[1]]) {
      round <- str_extract(x, '(?<=)(.*)(?=</td> <!-- Laver)') # no lint
      if (length(round) == 0) {
        round <- NA
      }
      
      opponent_rank <- str_extract(x, '(?<=different partners--> <td> )(.*)(?= </td> <td> <div class=\"day-table-flag\">)') # no lint
      if (length(opponent_rank) == 0) {
        opponent_rank <- NA
      }
      
      opponent <- str_extract(x, '(?<=ga-category=\"\" ga-use=\"true\"> )(.*)(?= </a> </div> </td> <td>)') # no lint
      if (length(opponent) == 0) {
        opponent <- NA
      }
      
      result <- str_extract(x, '(?<=</a> </div> </td> <td> )(.*)(?= </td> <td> <a )') # no lint
      if (length(result) == 0) {
        result <- NA
      }
      
      score <- str_extract(x, '(?<=ga-label=\"\" ga-action=\"\" ga-category=\"\" ga-use=\"true\"> )(.*)(?=$)') %>% trimws() %>% str_remove_all("<sup>[0-9]+</sup>") # no lint
      if (length(score) == 0) {
        score <- NA
      }
      
      # add games to matrix (each game is a line)
      matrix <- rbind(matrix, c(name, height, country, hand, birth_date, tourn_name, tourn_location, tourn_date, tourn_surface, tourn_prize, round, result, score, opponent, opponent_rank)) # no lint
    }
    
  }
  
  
  # prints every player once its done to keep track
  print(paste0("Done extracting all games from: ", name, " (", counter2, "/", nrow(links_activity), ")")) # no lint
  counter2 <- counter2 + 1
}

write.csv(matrix, "3_allgames.csv", row.names = FALSE)


# Germany games

all_games <- read.csv("3_all_games.csv") # no lint
all_games <- all_games %>% filter(grepl("Germany|Wetzlar|Braunschweig", tournament_location)) # no lint
write.csv(all_games, "4_gamesgermany.csv") # no lint

# merge games

games = read.csv("4_gamesgermany.csv") # no lint
games["player_rank"] <- NA
games["o_height"] <- NA
games["o_country"] <- NA
games["o_hand"] <- NA
games["o_born"] <- NA
for (i in 1:nrow(games)) { # no lint
  print(i)
  if (games[i, "p_name"] == "Delete") {
    next
  }
  for (j in 1:nrow(games)) {  # no lint
    
    if (games[i, "tournament"] == games[j, "tournament"] && games[i, "date"] == games[j, "date"] && games[i, "round"] == games[j, "round"] && games[i, "p_name"] == games[j, "opponent"] && games[j, "p_name"] == games[i, "opponent"]) {  # no lint
      games[i, "o_height"] <- games[j, "p_height"]
      games[i, "o_country"] <- games[j, "p_country"]
      games[i, "o_hand"] <- games[j, "p_hand"]
      games[i, "player_rank"] <- games[j, "opponent_rank"]
      games[i, "o_born"] <- games[j, "p_born"]
      games[j, "p_name"] <- "Delete"
      break
    }
  }
}
games <- games[!grepl("Delete", games$p_name), ]
write.csv(games, "gamesgermany_merged.csv") # no lint

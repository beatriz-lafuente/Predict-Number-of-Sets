#### CRISP-DM Project using ATP data ####


# Import libraries
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(VIM)
library(ggplot2)

data <- read.csv("5_gamesgermany_merged.csv") # no lint

# Creation of target variable

# n sets
data["n_sets"] <- NA
for (row in 1:nrow(data)) { # nolint
  if (is.na(data[row, "score"])) {
    next
  } else {
    data[row, "n_sets"] <- length(strsplit(data[row, "score"], " ")[[1]])
  }
}

#### Exploratory Analysis ####

# overall structure
dim(data)
str(data)
summary(data)

# Na's
colSums(is.na(data))
options(repr.plot.width = 10, repr.plot.height = 8)
visdat::vis_dat(data, sort_type = FALSE)

#### Exploratory Analysis (player variables: rank, height, country, hand, born) ####

# p_hand
ggplot(data = data, aes(x=p_hand,fill=p_hand))+ geom_bar()+geom_text(aes(label=..count..),vjust=-0.3, size=5.2, stat="count")+theme(legend.position="none", text = element_text(size = 20)) +
  xlab("Player hand") +
  ylab("Number of games")

# o_hand
ggplot(data = data, aes(x=o_hand,fill=o_hand))+ geom_bar()+geom_text(aes(label=..count..),vjust=-0.3, size=3.2, stat="count")+theme(legend.position="none") +
  xlab("Opponent hand") +
  ylab("Number of games")

# names of players with country as NA, (we can tell they are all Russian/Belorussian)
unique(data[is.na(data$p_country), "p_name"])
unique(data[is.na(data$p_country), "o_name"])

# box plot height opponent
boxplot(data$o_height,
        main = "Opponent height",
        xlab = "Height",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

# box plot height player
par(cex.main=2)
boxplot(data$p_height, main = "Player height")

# values of outliers
unique(boxplot.stats(data$p_height)$out)

# histogram with heights
hist(data$p_height)
hist(data$p_height,
     xlab = "height",
     main = "Histogram of player height",
     breaks = nrow(data)/1000
)

# rank
plot(data$player_rank, ylim=c(1,250))


#### Exploratory Analysis (tournament variables: ground, round, prize) ####

# tournaments with prize NA
unique(data[is.na(data$prize), "tournament"])

# round
graph <- ggplot(data = data, aes(x=reorder(round,round,function(x)-length(x)),fill=round))+ geom_bar()+geom_text(aes(label=..count..),vjust=-0.3, size=5.2, stat="count")+theme(legend.position="none", text = element_text(size = 14)) +
  xlab("Round") +
  ylab("number of games")
mygraphtheme <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (15)))
print(graph + mygraphtheme + ggtitle("Present rounds in the dataset"))

# tournaments with ground NA (almost all davis cup, and wolfsburg)
unique(data[is.na(data$ground), "tournament"])

ggplot(data = data, aes(x=ground,fill=ground))+ geom_bar()+geom_text(aes(label=..count..),vjust=-0.3, size=5.2, stat="count")+theme(legend.position="none", text = element_text(size = 20)) +
  xlab("Ground Type") +
  ylab("number of games")


#### Exploratory Analysis (game variables: score, n_sets) ####

# Bar plot 'n_sets'

graph1 <- ggplot(data = data, aes(x=reorder(n_sets,n_sets,
                                            function(x)-length(x)),fill=n_sets))+ geom_bar()+
  geom_text(aes(label=..count..),vjust=-0.3, size=5.2, stat="count")+
  theme(legend.position="none", text = element_text(size = 14)) +
  xlab("Number of sets") +
  ylab("Number of games")
mygraphtheme <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = (15)))
print(graph1 + mygraphtheme + ggtitle("Present number of sets in the dataset"))

# Bar plot between 'ground' e 'n_sets' (no apparent relation)
graph2 <- ggplot(data, aes(x = n_sets, fill = ground)) + geom_bar()
print(graph2 + mygraphtheme + ggtitle("ground and n_sets"))

# Faceted bar charts between 'ground' e 'n_sets'
ggplot(data, aes(x= n_sets)) +
  geom_bar() + 
  facet_wrap(~ground)

# Bar plot between 'result' e 'n_sets' (no apparent relation)
graph3 <- ggplot(data, aes(x = n_sets, fill = result)) + geom_bar()
print(graph3 + mygraphtheme + ggtitle("result and n_sets"))

# Bar plot between 'hand' e 'n_sets' (no apparent relation)
graph4 <- ggplot(data, aes(x = n_sets, fill = p_hand)) + geom_bar()
print(graph4 + mygraphtheme + ggtitle("player_hand and n_sets"))
graph5 <- ggplot(data, aes(x = n_sets, fill = o_hand)) + geom_bar()
print(graph5 + mygraphtheme + ggtitle("opponent_hand and n_sets"))

# Bar plot between 'round' e 'n_sets' (no apparent relation)
graph6 <- ggplot(data, aes(x = n_sets, fill = round)) + geom_bar()
print(graph6 + mygraphtheme + ggtitle("round and n_sets"))

# scores of games with 1 and 6 sets
unique(data[data$n_sets == 1, "score"])
unique(data[data$n_sets == 6, "score"])

# score variable as to be cleaned




#### Data Cleaning ####

# rank
for (i in 1:nrow(data)) { # no lint
  if (is.na(data[i, "player_rank"])) {
    print(paste(i, "of", nrow(data)))
    for (j in 1:nrow(data)) { # no lint
      if (data[i, "date"] == data[j, "date"] && data[i, "p_name"] == data[j, "p_name"] && !is.na(data[j, "player_rank"])) { # no lint
        data[i, "player_rank"] <- data[j, "player_rank"]
        break
      } else if (data[i, "date"] == data[j, "date"] && data[i, "p_name"] == data[j, "opponent"] && !is.na(data[j, "opponent_rank"])) { # no lint
        data[i, "player_rank"] <- data[j, "opponent_rank"]
        break
      }
    }
  }
}

# standardize prize in euros
for (row in 1:nrow(data)) { # no lint
  # if its NA skip line
  if (is.na(data[row, "prize"])) {
    next
  } else {
    # remove commas and the first character (currency symbol), and if it"s in dollars * 0.97 to convert to euros # no lint
    if (startsWith(data[row, "prize"], "$")) {
      data[row, "prize"] <- gsub("^.", "", gsub(",", "", data[row, "prize"]))
      data[row, "prize"] <- round(as.integer(data[row, "prize"]) * 0.97)
    } else {
      data[row, "prize"] <- as.integer(gsub("^.", "", gsub(",", "", data[row, "prize"]))) # no lint
    }
  }
}

# set prize = 0 where is NA
data["prize"][is.na(data["prize"])] <- 0

# alter anomalies in height values

# fixes wrong values in player and opponent heights
for (row in 1:nrow(data)) { # nolint
  if (data[row, "p_name"] == "Carlos Di Laura") {
    data[row, "p_height"] <- 175
  }
  if (data[row, "opponent"] == "Carlos Di Laura") {
    data[row, "o_height"] <- 175
  }
  if (data[row, "p_name"] == "Jorge Andrew" || data[row, "p_name"] == "Grant Stafford") { # no lint
    data[row, "p_height"] <- 188
  }
  if (data[row, "opponent"] == "Jorge Andrew" || data[row, "opponent"] == "Grant Stafford") {
    data[row, "o_height"] <- 188
  }
}

# set country <- RUS/BLR where country == NA
data["p_country"][is.na(data["p_country"])] <- "RUS/BLR"
data["o_country"][is.na(data["o_country"])] <- "RUS/BLR"

# set height NA where height = 0
data["o_height"][data["o_height"] == 0] <- NA
data["p_height"][data["p_height"] == 0] <- NA

# remove data with score (RET), (W/O) or (DEF) (there where no (UNP) in the data set) --> 1279 removed # no lint
data <- data[!grepl("(RET)|(DEF)|(W/O)", data$score), ]

# remove data in tournaments with  V | vs | vs. | v | v. (Davis Cup data) --> 241 removed # no lint
data <- data[!grepl(" V | vs | vs. | v | v. ", data$tournament), ]

# n sets tournament
data["type_tourn"] <- NA
for (i in 1:nrow(data)) { # no lint
  
  if (data[i, "tournament"] == "ATP Masters 1000 Essen" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "ATP Masters 1000 Hamburg" && data[i, "round"] == "Finals") { # no lint
    if (startsWith(data[i, "date"], "2007")) {
      data[i, "type_tourn"] <- 3
    } else if (startsWith(data[i, "date"], "2008")) {
      data[i, "type_tourn"] <- 3
    } else {
      data[i, "type_tourn"] <- 5
    }
  } else if (data[i, "tournament"] == "ATP Masters 1000 Stuttgart" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "ATP Tour World Championship" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Berlin") {
    if (startsWith(data[i, "date"], "1968") || startsWith(data[i, "date"], "1969") || startsWith(data[i, "date"], "1971") || startsWith(data[i, "date"], "1973")) { # no lint
      data[i, "type_tourn"] <- 5
    } else if ((startsWith(data[i, "date"], "1976") || startsWith(data[i, "date"], "1977") || startsWith(data[i, "date"], "1978") || startsWith(data[i, "date"], "1979")) && (data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Finals")) { # no lint
      data[i, "type_tourn"] <- 5
    } else if (startsWith(data[i, "date"], "1970") && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Quarter-Finals" || data[i, "round"] == "Round of 16" || data[i, "round"] == "Round of 32")) { # no lint
      data[i, "type_tourn"] <- 5
    } else {
      data[i, "type_tourn"] <- 3
    }
  } else if (data[i, "tournament"] == "Cologne WCT" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Dortmund WCT" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Dusseldorf") {
    if (startsWith(data[i, "date"], "1970") || startsWith(data[i, "date"], "1972") || startsWith(data[i, "date"], "1973")) { # no lint
      data[i, "type_tourn"] <- 5
    } else if (startsWith(data[i, "date"], "1974") && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals")) { # no lint
      data[i, "type_tourn"] <- 5
    } else {
      data[i, "type_tourn"] <- 3
    }
  }
  else if (data[i, "tournament"] == "Dusseldorf-1" && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Dusseldorf-2" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Essen WCT" && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Grand Slam Cup" && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && data[i, "round"] == "Finals" && (startsWith(data[i, "date"], "1983") || startsWith(data[i, "date"], "1984") || startsWith(data[i, "date"], "1985") || startsWith(data[i, "date"], "1986") || startsWith(data[i, "date"], "1987") || startsWith(data[i, "date"], "1988") || startsWith(data[i, "date"], "1989"))) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals") && (startsWith(data[i, "date"], "1975") || startsWith(data[i, "date"], "1976") || startsWith(data[i, "date"], "1977") || startsWith(data[i, "date"], "1978") || startsWith(data[i, "date"], "1979") || startsWith(data[i, "date"], "1980") || startsWith(data[i, "date"], "1981") || startsWith(data[i, "date"], "1982"))) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && startsWith(data[i, "date"], "1974") && (data[i, "round"] == "Quarter-Finals" || data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Finals")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && (startsWith(data[i, "date"], "1968") || startsWith(data[i, "date"], "1969") || startsWith(data[i, "date"], "1970") || startsWith(data[i, "date"], "1971"))) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && startsWith(data[i, "date"], "1973") && (data[i, "round"] == "Quarter-Finals" || data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Finals" || data[i, "round"] == "Round of 16")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Hamburg" && startsWith(data[i, "date"], "1972")) { # no lint
    if (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Quarter-Finals" || data[i, "round"] == "Round of 64") { # no lint
      data[i, "type_tourn"] <- 5
    } else if (data[i, "round"] == "Round of 16" && (data[i, "o_country"] == "ITA" || data[i, "o_country"] == "GER" || data[i, "o_country"] == "BEL")) { # no lint
      data[i, "type_tourn"] <- 5
    } else {
      data[i, "type_tourn"] <- 3
    }
  } else if (data[i, "tournament"] == "Munich" && (startsWith(data[i, "date"], "1976") || startsWith(data[i, "date"], "1977") || startsWith(data[i, "date"], "1978") || startsWith(data[i, "date"], "1979") || startsWith(data[i, "date"], "1980") || startsWith(data[i, "date"], "1981") || startsWith(data[i, "date"], "1984")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich" && (startsWith(data[i, "date"], "1970") || startsWith(data[i, "date"], "1971"))) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich" && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals") && startsWith(data[i, "date"], "1969")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich" && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Quarter-Finals") && startsWith(data[i, "date"], "1968")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich WCT" && (startsWith(data[i, "date"], "1982") || startsWith(data[i, "date"], "1983")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich-2" && (startsWith(data[i, "date"], "1974") || startsWith(data[i, "date"], "1975")) && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals")) { # no lint
    data[i, "type_tourn"] <- 5
  } else if (data[i, "tournament"] == "Munich-2" && (startsWith(data[i, "date"], "1982") || startsWith(data[i, "date"], "1983")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Munich-2" && startsWith(data[i, "date"], "1973")) { # no lint
    if (data[i, "round"] == "Semi-Finals" || data[i, "round"] == "Quarter-Finals" || data[i, "round"] == "Finals") { # no lint
      data[i, "type_tourn"] <- 5
    } else if (data[i, "round"] == "Round of 16" && data[i, "o_country"] == "MEX") { # no lint
      data[i, "type_tourn"] <- 5
    } else {
      data[i, "type_tourn"] <- 3
    }
  } else if (data[i, "tournament"] == "Stuttgart" && (startsWith(data[i, "date"], "1978") || startsWith(data[i, "date"], "1982") || startsWith(data[i, "date"], "1983") || startsWith(data[i, "date"], "1984") || startsWith(data[i, "date"], "2002") || startsWith(data[i, "date"], "2003") || startsWith(data[i, "date"], "2004") || startsWith(data[i, "date"], "2005") || startsWith(data[i, "date"], "2006")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Stuttgart" && startsWith(data[i, "date"], "1972") && (data[i, "round"] == "Finals" || data[i, "round"] == "Semi-Finals")) { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Stuttgart" && startsWith(data[i, "date"], "1975") && data[i, "round"] == "Semi-Finals") { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Stuttgart" && (startsWith(data[i, "date"], "1969") || startsWith(data[i, "date"], "1971") || startsWith(data[i, "date"], "1973") || startsWith(data[i, "date"], "1974") || startsWith(data[i, "date"], "1976") || startsWith(data[i, "date"], "1977"))) { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Stuttgart-1" && (startsWith(data[i, "date"], "1979") || startsWith(data[i, "date"], "1980") || startsWith(data[i, "date"], "1981") || startsWith(data[i, "date"], "1991") || startsWith(data[i, "date"], "1992") || startsWith(data[i, "date"], "1993") || startsWith(data[i, "date"], "1994") || startsWith(data[i, "date"], "1995") || startsWith(data[i, "date"], "1996") || startsWith(data[i, "date"], "1999") || startsWith(data[i, "date"], "2000")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
    
  } else if (data[i, "tournament"] == "Stuttgart-2" && (startsWith(data[i, "date"], "1979") || startsWith(data[i, "date"], "1980") || startsWith(data[i, "date"], "1981") || startsWith(data[i, "date"], "1990") || startsWith(data[i, "date"], "1991") || startsWith(data[i, "date"], "1992") || startsWith(data[i, "date"], "1993") || startsWith(data[i, "date"], "1994")) && data[i, "round"] == "Finals") { # no lint
    data[i, "type_tourn"] <- 5
    
  } else {
    data[i, "type_tourn"] <- 3
  }
}

# 670 removed

data <- data %>% filter(grepl(3, type_tourn))

# input carpet value in NA (wolfs burg tournament)
data$ground[is.na(data$ground)] <- 'Carpet'

# insert variable rank_level
data$player_rank[data$player_rank == '-'] <- 2500
data$opponent_rank[data$opponent_rank == '-'] <- 2500
data$player_rank <- strtoi(data$player_rank)
data$opponent_rank <- strtoi(data$opponent_rank)


# remove lines with Na's in hand, height and born (11116 lines lost)
data <- subset(data, !is.na(p_hand))
data <- subset(data, !is.na(o_hand))
data <- subset(data, !is.na(p_height))
data <- subset(data, !is.na(o_height))
data <- subset(data, !is.na(p_born))
data <- subset(data, !is.na(o_born))

# remove single game with round "3rd/4th Place Match"
data <- data[!grepl("3rd/4th Place Match", data$round), ]




#### Feature Engineering ####

# create data frame to compute variables
data_new_variables <- data

# rank difference
data_new_variables$rank_difference <- abs(data_new_variables$player_rank - data_new_variables$opponent_rank)

# height difference
data_new_variables$height_difference <- abs(data_new_variables$p_height - data_new_variables$o_height)

# age difference

# keep only the year of born variable
data_new_variables$p_year <- NA
data_new_variables$o_year <- NA
for (row in 1:nrow(data_new_variables)) {
  data_new_variables[row, "p_year"] <- substr(data_new_variables[row, "p_born"], 1, 4)
  data_new_variables[row, "o_year"] <- substr(data_new_variables[row, "o_born"], 1, 4)
}

data_new_variables$o_year <- strtoi(data_new_variables$o_year)
data_new_variables$p_year <- strtoi(data_new_variables$p_year)

# subtract both years
data_new_variables$age_difference <- abs(data_new_variables$p_year - data_new_variables$o_year)

# region variables
countries <- read.csv("6_countries.csv")
countries <- countries[,-2]

data_new_variables <- merge(data_new_variables, countries, by.x = "p_country", by.y = "code")
colnames(data_new_variables)[colnames(data_new_variables) == "sub.region"] ="p_country_sub_region"
data_new_variables <- merge(data_new_variables, countries, by.x = "o_country", by.y = "code")
colnames(data_new_variables)[colnames(data_new_variables) == "sub.region"] ="o_country_sub_region"

# variable compare regions and mark if same or different region
data_new_variables$regions_players <- NA

for (i in 1:nrow(data_new_variables)) {
  if (grepl("Europe|America|Australia", data_new_variables[i, "p_country_sub_region"], ignore.case = TRUE) & (grepl("Europe|America|Australia", data_new_variables[i, "o_country_sub_region"], ignore.case = TRUE))) {
    data_new_variables[i, "regions_players"] <- "Same"
  } else if (grepl("asia|esia|Africa", data_new_variables[i, "p_country_sub_region"], ignore.case = TRUE) & (grepl("asia|esia|Africa", data_new_variables[i, "o_country_sub_region"], ignore.case = TRUE))) {
    data_new_variables[i, "regions_players"] <- "Same"
  } else if (grepl("Europe|America|Australia", data_new_variables[i, "p_country_sub_region"], ignore.case = TRUE) & (grepl("asia|esia|Africa", data_new_variables[i, "o_country_sub_region"], ignore.case = TRUE))) {
    data_new_variables[i, "regions_players"] <- "Different"
  } else if (grepl("Europe|America|Australia", data_new_variables[i, "o_country_sub_region"], ignore.case = TRUE) & (grepl("asia|esia|Africa", data_new_variables[i, "p_country_sub_region"], ignore.case = TRUE))) {
    data_new_variables[i, "regions_players"] <- "Different"
  }
}
data_new_variables$regions_players <- as.factor(data_new_variables$regions_players)

# keep only variables of interest for ABT
abt <- data_new_variables

# ABT with regions
abt <- abt[, c(7,12,13,14,19,23,24,27,28,29,30,21)]

abt$p_hand <- as.factor(abt$p_hand)
abt$o_hand <- as.factor(abt$o_hand) 
abt$ground <- as.factor(abt$ground)
abt$prize <- as.integer(abt$prize)
abt$p_country_sub_region <- as.factor(abt$p_country_sub_region)
abt$o_country_sub_region <- as.factor(abt$o_country_sub_region)
abt$regions_players <- as.factor(abt$regions_players)
abt$n_sets <- as.factor(abt$n_sets)
  
# final ABT
abt2 <- abt[, -c(9, 10)]




#### Modelling ####


# Import libraries
library(plyr)
library(dplyr)
library(car)
library(caret)
library(ROCR)


#### Logistic Regression ####

# 1st iteration with unbalanced data and all ABT variables
data_reg_model1 <- abt2

# target variable as factor
data_reg_model1$n_sets <- as.factor(data_reg_model1$n_sets)

# Random division of data_reg_model1: Training (2/3) and Test (1/3)

set.seed(122)

index_1<-sample(1:nrow(data_reg_model1),round(nrow(data_reg_model1)*2/3))
train_1<-data_reg_model1[index_1,]
teste_1<-data_reg_model1[-index_1,]

# Creating the logistic regression Model
model<-glm(n_sets ~ ., data = train_1,family="binomial")

# Summary of the model
summary(model)

# Test of the created model (model) with the unseen data_reg_model1 from the test set (test_1)
model_res<-predict(model,teste_1,type="response")
model_table<-cbind(pred=round(model_res,3),n_sets=teste_1$n_sets)
model_table<-data.frame(model_table)

# Creation of the confusion matrix and calculation of the metrics (accuracy, sensitivity, specificity, etc)
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c("2","3"))
model_pred
confusionMatrix(model_pred,teste_1$n_sets)

# ROC curve
previsao<-prediction(model_res,teste_1$n_sets)
previsao
desemp<-performance(previsao,measure = "tpr",x.measure="fpr")
plot(desemp)

# AUC calculation
auc<-performance(previsao,measure="auc")
auc<-auc@y.values[[1]]
auc

# 2nd iteration with balanced data and all ABT variables

data_model2_bal <- data_reg_model1

# Data Set balancing study
# Calculation of the "Healthy" and "Sick" ratios of the target variable (n_sets)
prop.table(table(data_model2_bal$n_sets)) %>% round(2)

# Graphical representation of the target variable (n_sets)
plot(data_model2_bal$n_sets,main = "Target (n_sets)",xlab = "Values",ylab = "Number of obs.",col = "blue")

set.seed(101)
index_1<-sample(1:nrow(data_model2_bal),round(nrow(data_model2_bal)*2/3))
train_1<-data_model2_bal[index_1,]
teste_1<-data_model2_bal[-index_1,]

# Balancing
train_down <- downSample(train_1[,-10],train_1[,10])
names(train_down)[10] <- "n_sets"

# Graphical representation of the target variable (n_sets) - after balancing using under-sampling
plot(train_down$n_sets,main = "Target (n_sets) - after balancing data (under-sampling)",xlab = "Values", ylab = "number of obs.",col = "orange")

model<-glm(n_sets ~ ., data = train_down,family="binomial")

# Summary of model
summary(model)

# Testing the model
model_res<-predict(model,teste_1,type="response")
model_table<-cbind(pred=round(model_res,3),n_sets=teste_1$n_sets)
model_table<-data.frame(model_table)
head(model_table)

# Create confusion matrix and calculate the metrics
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c("2","3"))
confusionMatrix(model_pred,teste_1$n_sets)

# ROC
previsao<-prediction(model_res,teste_1$n_sets)
previsao
desemp<-performance(previsao,measure = "tpr",x.measure="fpr")
plot(desemp)

# AUC
auc<-performance(previsao,measure="auc")
auc<-auc@y.values[[1]]
auc

# 3rd iteration with balanced data and only ABT variables of the differences

data_reg_model3 <- data_model2_bal[, c(6,7,8,10)]

set.seed(101)
index_1<-sample(1:nrow(data_reg_model3),round(nrow(data_reg_model3)*2/3))
train_1<-data_reg_model3[index_1,]
teste_1<-data_reg_model3[-index_1,]

# Balacing
train_down <- downSample(train_1[,-4],train_1[,4])
names(train_down)[4] <- "n_sets"

# Graphical representation of the target variable (n_sets) - after balancing (train-down)
plot(train_down$n_sets,main = "Target (n_sets) - Train data of data_reg_model3  after
  balancing (under-sampling)",xlab = "Values", ylab = "number of obs.",col = "orange")

model<-glm(n_sets ~ ., data = train_down,family="binomial")

# Summary
summary(model)

# Test of the created model (model) with unseen data_reg_model3 from the test set (test_1)
model_res<-predict(model,teste_1,type="response")
model_table<-cbind(pred=round(model_res,3),n_sets=teste_1$n_sets)
model_table<-data.frame(model_table)
head(model_table)

# Creation of the confusion matrix and calculation of the metrics
model_res1<-ifelse(model_res>0.5,2,1)
model_pred<-factor(model_res1,labels=c("2","3"))
confusionMatrix(model_pred,teste_1$n_sets)

# ROC
previsao<-prediction(model_res,teste_1$n_sets)
previsao
desemp<-performance(previsao,measure = "tpr",x.measure="fpr")
plot(desemp)

# AUC
auc<-performance(previsao,measure="auc")
auc<-auc@y.values[[1]]
auc




#### Neural Networks ####


# Function that checks if the variables are multicollinear
round(cor(data_trees),3)

# Import libraries
library(ggplot2)
library(neuralnet)
library(caret)
library(partykit)
library(rpart)
library(rpart.plot)
library(pROC)


# Preparing the data

data_nnet_models <- abt

data_nnet_models <- data_nnet_models[!grepl("Ambidextrous", data_nnet_models$p_hand), ]
data_nnet_models <- data_nnet_models[!grepl("Ambidextrous", data_nnet_models$o_hand), ]
data_nnet_models <- data_nnet_models[!grepl("3rd/4th Place Match", data_nnet_models$round), ]
data_nnet_models <- data_nnet_models[!grepl("Round Robin", data_nnet_models$round), ]
data_nnet_models <- data_nnet_models[!grepl("Grass", data_nnet_models$ground), ]

# Encode variables

# hand
data_nnet_models$p_hand_num <- ifelse(data_nnet_models$p_hand == "Right-Handed",-1,+1)
data_nnet_models$o_hand_num <- ifelse(data_nnet_models$p_hand == "Right-Handed",-1,+1)        
data_nnet_models$o_hand <- NULL
data_nnet_models$p_hand <- NULL

# round
data_nnet_models$round_initial <- ifelse(grepl("Qualifying|64", data_nnet_models$round),1,0)
data_nnet_models$round_finals <- ifelse(grepl("Finals", data_nnet_models$round),1,0)
data_nnet_models$round <- NULL

# ground
data_nnet_models$ground_num <- ifelse(data_nnet_models$ground == "Clay",-1,+1)
data_nnet_models$ground <- NULL

# country region
data_nnet_models$region_aaa_eur <- ifelse(grepl("Asia|Africa|esia|Australia", data_nnet_models$p_country_sub_region) & grepl("Europe", data_nnet_models$o_country_sub_region),1,0)
data_nnet_models$region_aaa_ame <- ifelse(grepl("Asia|Africa|esia|Australia", data_nnet_models$p_country_sub_region) & grepl("America", data_nnet_models$o_country_sub_region),1,0)
data_nnet_models$region_eur_eur <- ifelse(grepl("Europe", data_nnet_models$p_country_sub_region) & grepl("Europe", data_nnet_models$o_country_sub_region),1,0)
data_nnet_models$region_eur_ame <- ifelse(grepl("Europe", data_nnet_models$p_country_sub_region) & grepl("America", data_nnet_models$o_country_sub_region),1,0)
data_nnet_models$region_ame_ame <- ifelse(grepl("America", data_nnet_models$p_country_sub_region) & grepl("America", data_nnet_models$o_country_sub_region),1,0)
data_nnet_models$p_country_sub_region <- NULL
data_nnet_models$o_country_sub_region <- NULL

# reorder
data_nnet_models <- data_nnet_models[c(1:5,7:16,6)]
data_nnet_models <- data_nnet_models[-c(5)]

dim(data_nnet_models)
str(data_nnet_models)

# scale the data
sca.dia <- scale(data_nnet_models[-15])

# Add back the outcome variable
sca.dia <- cbind(data_nnet_models[15],sca.dia)

# order variables so target is last (last command made it first)
sca.dia <- sca.dia[, c(2:15,1)]

# Randomize and split the data for training and testing 
set.seed(1000)
index <- sample(1:nrow(sca.dia), 2/3*nrow(sca.dia))

my.Train <- sca.dia[index,]
my.Test <- sca.dia[-index, ]

# Balance data
train_down <- downSample(my.Train[,-15],my.Train[,15])
names(train_down)[15] <- "n_sets"
plot(train_down$n_sets,main = "Target (n_sets) - Train data after balacing (under-sample)",xlab = "Values", ylab = "Number of obs.",col = "orange")

# reorder again for target to be first
train_down <- train_down[, c(15, 1:14)]
my.Test <- my.Test[, c(15, 1:14)]


#### 1st iteration with balanced data and only ABT variables of the differences

# Build and plot the network model.
my.nnetc1 <- neuralnet(n_sets ~ .,
                       data=train_down,hidden=6,
                       act.fct = 'logistic', linear.output = FALSE)
plot(my.nnetc1)

# Make predictions on the test data.
my.pred<- predict(my.nnetc1,my.Test)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions = my.pred)

# if else converts probabilities to factor values
# Use column 1 of my.results for the if else statement
# Column 1 values are probabilities for tested_negative

my.predList<- ifelse(my.results[1] > 0.5,1,2) # > .5 = "tested_negative" 

# Structure the confusion matrix
my.predList <- factor(my.predList,labels=c("2","3"))
confusionMatrix(my.predList,my.Test$n_sets)


#### 2nd Iteration

# Build and plot the network model.
my.nnetc2 <- neuralnet(n_sets ~ .,
                       data=train_down,hidden=4,
                       act.fct = 'logistic', linear.output = FALSE)
plot(my.nnetc2)

# Make predictions on the test data.
my.pred<- predict(my.nnetc2,my.Test)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions = my.pred)

# if else converts probabilities to factor values
# Use column 1 of my.results for the if else statement
# Column 1 values are probabilities for tested_negative

my.predList<- ifelse(my.results[1] > 0.5,1,2) #  >.5="tested_negative" 

# Structure the confusion matrix
my.predList <- factor(my.predList,labels=c("2","3"))
confusionMatrix(my.predList,my.Test$n_sets)


#### 3rd Iteration

# Build and plot the network model.
my.nnetc3 <- neuralnet(n_sets ~ .,
                       data=train_down,hidden=2,
                       act.fct = 'logistic', linear.output = FALSE)
plot(my.nnetc3)

# Make predictions on the test data.
my.pred<- predict(my.nnetc3,my.Test)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions = my.pred)

# if else converts probabilities to factor values
# Use column 1 of my.results for the if else statement
# Column 1 values are probabilities for tested_negative

my.predList<- ifelse(my.results[1] > 0.5,1,2) # >.5="tested_negative" 

# Structure the confusion matrix
my.predList <- factor(my.predList,labels=c("2","3"))
confusionMatrix(my.predList,my.Test$n_sets)

# Get only variables of the differences
data_nnet_models_differences <- data_nnet_models[, c(2,3,4,15)]

dim(data_nnet_models_differences)
str(data_nnet_models_differences)

# Scale the data
sca.dia <- scale(data_nnet_models_differences[-4])

# Add back the outcome variable
sca.dia <- cbind(data_nnet_models_differences[4],sca.dia)

# Order variables so target is last
sca.dia <- sca.dia[, c(2:4,1)]

# Randomize and split the data for training and testing 
set.seed(1000)
index <- sample(1:nrow(sca.dia), 2/3*nrow(sca.dia))

my.Train <- sca.dia[index,]
my.Test <- sca.dia[-index, ]

# Balance data
train_down <- downSample(my.Train[,-4],my.Train[,4])
names(train_down)[4] <- "n_sets"

# Reorder again for target to be first
train_down <- train_down[, c(4, 1:3)]
my.Test <- my.Test[, c(4, 1:3)]


#### 4th Iteration

# Build and plot the network model.
my.nnetc <- neuralnet(n_sets ~ .,
                      data=train_down,hidden=2,
                      act.fct = 'logistic', linear.output = FALSE)
plot(my.nnetc)

# Make predictions on the test data.
my.pred<- predict(my.nnetc,my.Test)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# if else converts probabilities to factor values
# Use column 1 of my.results for the if else statement
# Column 1 values are probabilities for tested_negative

my.predList<- ifelse(my.results[1] > 0.5,1,2) # >.5="tested_negative" 

# Structure the confusion matrix
my.predList <- factor(my.predList,labels=c("2","3"))

confusionMatrix(my.predList,my.Test$n_sets)


#### 5th Iteration

# Build and plot the network model.
my.nnetc <- neuralnet(n_sets ~ .,
                      data=train_down,hidden=6,
                      act.fct = 'logistic', linear.output = FALSE)

plot(my.nnetc)

# Make predictions on the test data.
my.pred<- predict(my.nnetc,my.Test)

# Make the table needed to create the confusion matrix.
my.results <-data.frame(Predictions =my.pred)

# if else converts probabilities to factor values
# Use column 1 of my.results for the if else statement
# Column 1 values are probabilities for tested_negative

my.predList<- ifelse(my.results[1] > 0.5,1,2) # >.5="tested_negative" 

# Structure the confusion matrix
my.predList <- factor(my.predList,labels=c("2","3"))

confusionMatrix(my.predList,my.Test$n_sets)




#### Decision Trees ####


library(partykit) # Toolkit with infrastructure for representing, summarizing and visualizing tree-structured regression and classification models
library(rpart) # Recursive partitioning for classification, regression and survival trees
library(rpart.plot) # Plot 'r part' models
library(ipred)
library(caret)
library(ROCR)

data_trees <- abt2


# Random division of data: Training (2/3) and Test (1/3)

set.seed(100)

index_1<-sample(1:nrow(data_trees),round(nrow(data_trees)*2/3))
train_1<-data_trees[index_1,]
teste_1<-data_trees[-index_1,]

# Balance the data
train_down <- downSample(train_1[,-10],train_1[,10])
names(train_down)[10] <- "n_sets"

# Graphical representation of the target variable (n_sets) with data from the train set after balancing (train_down)
plot(train_down$n_sets,main = "Target (n_sets) - Train data after balancing (under-sampling)",xlab = "Values", ylab = "Number of obs.",col = "orange")


#### 1st Iteration Decision Trees

# Creation of the Decision Tree Model, from the training set (train_1), using the CART Algorithm
# xval= number of cross validations
model_tree<-rpart(formula = n_sets~.,data=train_down,method="class",control=rpart.control(xval = 10, cp=0.001))
model_tree

# Summary of the Decision Tree Model created (model_tree), which allows determining the total number of nodes in the tree
model_tree_party<-as.party(model_tree)
model_tree_party

# Test the Model created (model_tree) based on unseen data from the test set (test_1)
# Calculation of predictions using the created model (model_tree)
model_tree_previsao<-predict(model_tree,newdat=teste_1,type="class")

# Create confusion matrix
confusionMatrix(model_tree_previsao,teste_1$n_sets)

# Create Complexity Table and graph cp vs xvalerror
model_tree$cptable
plotcp(model_tree)


#### 2nd Iteration - Creation of a new tree model by applying the pruning operation to the tree model_tree

model_tree_new<-prune(model_tree,cp=0.001555479)

# Summary of the model
model_tree_party<-as.party(model_tree_new)
model_tree_party

# Testing the model (model_tree_new)
model_tree_new_previsao<-predict(model_tree_new,newdat=teste_1,type="class")

# Create confusion matrix
confusionMatrix(model_tree_new_previsao,teste_1$n_sets)

# Graphical representation of the tree model using pruning
rpart.plot(model_tree_new,yesno=TRUE)


#### 3rd Iteration - Decision tree with only 4 independent variables (age_difference, height_difference, rank_difference, regions_players)

# Balance the data
data_tree_3iter <- abt2[, c(6,7,8,10)]
str(data_tree_3iter)

set.seed(100)

index_1<-sample(1:nrow(data_tree_3iter),round(nrow(data_tree_3iter)*2/3))
train_1<-data_tree_3iter[index_1,]
teste_1<-data_tree_3iter[-index_1,]

train_down <- downSample(train_1[,-4],train_1[,4])
names(train_down)[4] <- "n_sets"

# Graphical representation of the target variable (n_sets) with data from the train set after balancing (train_down)
plot(train_down$n_sets,main = "Target (n_sets) - Train data after balancing (under-sampling)",
     xlab = "Values", ylab = "number of obs.",col = "orange")

model_tree<-rpart(formula = n_sets~.,data=train_down,method="class",control=rpart.control(xval = 10, cp=0.001))
model_tree

# Summary of the model
model_tree_party<-as.party(model_tree)
model_tree_party

# Testing the model
# Making predictions
model_tree_previsao<-predict(model_tree,newdat=teste_1,type="class")

# Create confusion matrix
confusionMatrix(model_tree_previsao,teste_1$n_sets)


#### 4th Iteration - Bagging

data_trees <- abt2

# Random division of data: Training (2/3) and Test (1/3)

set.seed(100)

index_1<-sample(1:nrow(data_trees),round(nrow(data_trees)*2/3))
train_1<-data_trees[index_1,]
teste_1<-data_trees[-index_1,]

# Balance the data
train_down <- downSample(train_1[,-10],train_1[,10])
names(train_down)[10] <- "n_sets"

# Graphical representation of the target variable (n_sets) with data from the train set after balancing (train_down)
plot(train_down$n_sets,main = "Target (n_sets) - Train data after balancing (under-sampling)",
xlab = "Values", ylab = "Number of obs.",col = "orange")

model_bag1<-bagging(n_sets ~.,data=train_down,method="rpart",nbagg=100,coob=TRUE,Control=rpart.control(minsplit = 2, cp=0.01))
model_bag1

# Test Model created (model_bag1) based on unseen data from the test set
# Calculation of predictions using the created model (model_bag1)
model_bag1_previsao<-predict(model_bag1,teste_1)

# Graphic representation of predicted classes (no: 3; yes: 2)
plot(teste_1$n_sets,model_bag1_previsao,main="Predicted classification with Bagging Tree: Predicted vs Real",xlab="Real",ylab="Predicted")

# Creation of the confusion matrix and calculation of the metrics
confusionMatrix(model_bag1_previsao,teste_1$n_sets)


#### 5th Iteration - Boosting

# Creation of a tree model using the Boosting method
cv.control<-trainControl(method="cv",number=10,savePredictions="final")

model_boosting<-train(n_sets ~.,data=train_down,method="gbm",trControl=cv.control)
model_boosting

# Test the Model created (model_boosting) based on unseen data from the test set (test_1)
# Calculation of forecasts using the created model (model_boosting)
model_boosting_previsao<-predict(model_boosting,teste_1)
model_boosting_previsao

# Graphic representation of predicted classes
plot(teste_1$n_sets,model_boosting_previsao,main="Tree obtained with Boosting method: Predicted vs Real",
     xlab="Real", ylab="Predicted")
abline(0,1)

# Test of the Model created (model_boosting): Creation of the table with the actual values and estimates of the Sales variable for the unseen data of the test set
tabela<-data.frame(VReais=teste_1$n_sets,VPrevistos=model_boosting_previsao)

# Testing of the new model created (model_boosting): Addition of a column to the table that indicates the errors associated with the Sales estimates (obtained with model_boosting) for the unseen data of the test set

# tabela$error<-with(tabela,teste_1$n_sets-model_boosting_previsao)

# Table view
tabela

# Confusion matrix
confusionMatrix(model_boosting_previsao,teste_1$n_sets)


#### 6th Iteration - Random Forests

# Creation of a tree model using the Random Forests method
cv.control<-trainControl(method="cv",number=10,savePredictions="final")
model_forest<-train(n_sets ~.,data=train_down,method="ranger",metric="Accuracy",tuneLength=5,trControl=cv.control)
model_forest

# Test the Model created (model_forest) based on unseen data from the test set (test_1)
# Calculation of forecasts using the created model (model_forest)
model_forest_previsao<-predict(model_forest,teste_1)

# Graphic representation of predicted classes
plot(teste_1$n_sets,model_forest_previsao,main="Tree obtained with Random Forests method: Predicted vs
Real",xlab="Real",ylab="Predicted")
abline(0,1)

# Test of the Model created (model_forest)
# Creation of the table with the real values and estimates of the variable n_sets for the unseen data of the test set
tabela<-data.frame(VReais=teste_1$n_sets,VPrevistos=model_forest_previsao)

# Table view
tabela

# Confusion matrix
confusionMatrix(model_forest_previsao,teste_1$n_sets)




#### Models terminated (Conclusion) ####

# Best Model: 6th iteration of decision trees (Random forests)

  # Accuracy: 0.4833
  # Sensibility: 0.3961
  # Specificity: 0.6450

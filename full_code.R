library(caret)
library(dplyr)

# Loading the data

HallOfFame = read.table(file = '~/Documents/ISYE 7406 Project/Project/csv files/HallOfFame.csv', sep = ',', header = TRUE)
Pitching = read.table(file = '~/Documents/ISYE 7406 Project/Project/csv files/Pitching.csv', sep = ',', header = TRUE)
AllstarFull = read.table(file = '~/Documents/ISYE 7406 Project/Project/csv files/AllstarFull.csv', sep = ',', header = TRUE, fill = TRUE)
AwardsPlayers = read.table(file = '~/Documents/ISYE 7406 Project/Project/csv files/AwardsPlayers.csv', sep = ',', header = TRUE)
People = read.table(file = '~/Documents/ISYE 7406 Project/Project/csv files/People.csv', sep = ',', header = TRUE, fill = TRUE, quote="")

# Cleaning each table (summing totals, removing unneeded columns)

# ALL STAR GAMES

# removing columns
AllstarFull <- AllstarFull[,!names(AllstarFull) %in% 
                             c('yearID', 'gameNum', 'gameID', 'teamID', 'lgID')]

# changing GP to all 1 so that it can be counted to see total All Star Games
AllstarFull['GP'][AllstarFull['GP'] == 0] <- 1
AllstarFull['startingPos'][AllstarFull['startingPos'] != 0] <- 1

# converting NA values to 0
AllstarFull[is.na(AllstarFull)] <- 0

# getting the total All Star Games and All Star starts per player
AllstarFull <- AllstarFull %>% group_by(playerID) %>%
  summarise(totalASG = sum(GP), 
            totalSASG = sum(startingPos),
            .groups = 'drop') %>%
  as.data.frame()


# AWARDS

# removing columns
AwardsPlayers <- AwardsPlayers[,!names(AwardsPlayers) %in% 
                                 c('yearID', 'lgID', 'tie', 'notes')]

# changing award name to a 1, many different awards over the years are the same thing
AwardsPlayers['awardID'] <- 1

# getting the total awards per player
AwardsPlayers <- AwardsPlayers %>% group_by(playerID) %>%
  summarise(totalAwards = sum(awardID),
            .groups = 'drop') %>%
  as.data.frame()


# HALL OF FAME

# removing people voted in as managers, umpires, or pioneers/executives
# this analysis only consists of people who got in as players
HallOfFame <- HallOfFame[!(HallOfFame$category == 'Manager' 
                           | HallOfFame$category == 'Pioneer/Executive'
                           | HallOfFame$category == 'Umpire'),]

# removing columns that aren't important
HallOfFame <- HallOfFame[,!names(HallOfFame) %in% 
                           c('votedBy', 'needed', 'needed_note',
                             'yearID', 'ballots', 'votes', 'category')]

# removing rows that indicate the player was not voted in
# the final data frame will be updated to show which players are not in the HOF
HallOfFame <- HallOfFame[!(HallOfFame$inducted == 'N'),]


# PEOPLE

# extracting the first and last names of each player
People <- People[, c('playerID', 'nameFirst', 'nameLast')]


# PITCHING

# converting IP outs to IP
# dividing IP outs by 3 to get the number of innings pitched
Pitching$IP <- round((Pitching$IPouts / 3), 0)

# removing columns with NA values, certain stats weren't recorded until late 1900s
# also removing league, team, and year (league and team names have changed many times)
Pitching <- Pitching[,!names(Pitching) %in% 
                       c('yearID', 'stint', 'lgID', 'teamID',
                         'BAOpp', 'ERA', 'BFP', 'WP', 'IBB', 'HBP', 'SH',
                         'SF', 'GIDP', 'R', 'GF', 'BK', 'IPouts')]

# getting the totals of each column
Pitching <- Pitching %>% group_by(playerID) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  as.data.frame()

# converting NA values to 0
Pitching[is.na(Pitching)] <- 0

# removing pitchers with less than 10 games
# hitters can sometimes pitch a few times in a career in a blowout
Pitching <- Pitching[!(Pitching$G < 10),]


# MERGING THE DATA FRAMES

all_pitching_data <- merge(Pitching, HallOfFame, by = 'playerID', all.x = TRUE)
all_pitching_data <- merge(all_pitching_data, AllstarFull, by = 'playerID', all.x = TRUE) # left outer join
all_pitching_data <- merge(all_pitching_data, AwardsPlayers, by = 'playerID', all.x = TRUE)
all_pitching_data <- merge(People, all_pitching_data, all = FALSE)


# NA to 0
all_pitching_data[is.na(all_pitching_data)] <- 0

# HOF Y to 1
all_pitching_data['inducted'][all_pitching_data['inducted'] == 'Y'] <- 1

# place "inducted" at the front
all_pitching_data <- all_pitching_data %>% relocate(inducted, .after = nameLast)



# EDA

# Hall of Fame counts - bar plot

library(ggplot2)
ggplot(all_pitching_data, aes(x = inducted)) + geom_bar(fill = "lightblue") + theme_classic() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") + ggtitle("Hall of Fame Distribution")

HOF_count <- table(all_pitching_data$inducted)
HOF_y = round((84 / 7323) * 100, 2) # 1.15% have been voted in

# table of correlations

library(reshape2)
all_pitching_data$inducted <- as.numeric((as.character(all_pitching_data$inducted)))
numeric_columns <- all_pitching_data[,unlist(lapply(all_pitching_data, is.numeric))]
cors <- round(cor(numeric_columns), 2)

# corr plot

library(corrplot)
corrplot(cors, method = "square")

# in hall vs not in hall

in_hof = all_pitching_data[all_pitching_data$inducted == 1, ]
summary(in_hof$totalAwards)
summary(in_hof$totalASG)

not_in_hof = all_pitching_data[all_pitching_data$inducted == 0, ]
summary(not_in_hof$totalAwards)
summary(not_in_hof$totalASG)



# Data set without player ID and first / last name

pitching_final <- all_pitching_data[,4:20]

pitching_final$IP = as.integer(pitching_final$IP)
pitching_final$totalASG = as.integer(pitching_final$totalASG)
pitching_final$totalAwards = as.integer(pitching_final$totalAwards)
pitching_final$totalSASG = as.integer(pitching_final$totalSASG)
pitching_final$inducted = as.integer(pitching_final$inducted)

# Converting the binary y variable to a valid variable name for classification

pitching_final$inducted[pitching_final$inducted == 1] <- "yes"
pitching_final$inducted[pitching_final$inducted == 0] <- "no"

# 80% training 20% testing

set.seed(179)
flag = sample(1:7323, 1464, replace = FALSE)
hof_train = pitching_final[-flag,]
hof_test = pitching_final[flag,]

# Converting y to a factor

hof_train$inducted <- as.factor(hof_train$inducted)
hof_test$inducted <- as.factor(hof_test$inducted)

# True y values for the training and testing sets
true_hof_train <- hof_train$inducted
true_hof_test <- hof_test$inducted


# LOGISTIC REGRESSION

# Specify the type of training method used and the number of folds
logreg_ctrl <- trainControl(method = "cv",
                            number = 10,
                            savePredictions = "all",
                            classProbs = TRUE)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
logreg_model <- train(inducted ~.,
                      data = hof_train,
                      method = "glm",
                      family = binomial,
                      trControl = logreg_ctrl)

print(logreg_model)
summary(logreg_model)
varImp(logreg_model)
plot(varImp(logreg_model))

# New logistic regression model for testing data
logreg_modelv2 <- train(inducted ~ G + GS + CG + SV + IP + totalAwards,
                        data = hof_train,
                        method = "glm",
                        family = binomial,
                        trControl = logreg_ctrl)

# Training error using selected variables
logreg_pred_train <- predict(logreg_modelv2, hof_train[,2:17]) != hof_train$inducted
logreg_trainErr <- mean(predict(logreg_modelv2, hof_train[,2:17]) != hof_train$inducted)

# Testing error using selected variables
logreg_pred_test <- predict(logreg_modelv2, hof_test[,2:17]) != hof_test$inducted
logreg_testErr <- mean(predict(logreg_modelv2, hof_test[,2:17]) != hof_test$inducted)


# RANDOM FOREST

# Specify the type of training method used and the number of folds
rf_ctrl <- trainControl(method = "cv", number = 5)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
rf_model <- train(inducted ~.,
                  data = hof_train,
                  method = "rf",
                  trControl = rf_ctrl)

rf_model
plot(rf_model)
varImp(rf_model)
plot(varImp(rf_model))

# New random forest model
rf_modelv2 <- train(inducted ~ W + totalAwards + SHO + totalASG + IP,
                    data = hof_train,
                    method = "rf",
                    trControl = rf_ctrl)

# Training error using selected variables
rf_pred_train <- predict(rf_modelv2, hof_train[,2:17]) != hof_train$inducted
rf_trainErr <- mean(predict(rf_modelv2, hof_train[,2:17]) != hof_train$inducted)

# Testing error using selected variables
rf_pred_test <- predict(rf_modelv2, hof_test[,2:17]) != hof_test$inducted
rf_testErr <- mean(predict(rf_modelv2, hof_test[,2:17]) != hof_test$inducted)


# KNN

# Specify the type of training method used and the number of folds
knn_ctrl <- trainControl(method = "cv", number = 10)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
knn_model <- train(inducted ~.,
                   data = hof_train,
                   method = "knn",
                   metric = "Accuracy",
                   tuneGrid = expand.grid(k = 1:10),
                   trControl = knn_ctrl)

knn_model
plot(knn_model)
plot(varImp(knn_model))

# Training error
knn_pred_train <- predict(knn_model, hof_train[,2:17]) != hof_train$inducted
knn_trainErr <- mean(predict(knn_model, hof_train[,2:17]) != hof_train$inducted)

# Testing error
knn_pred_test <- predict(knn_model, hof_test[,2:17]) != hof_test$inducted
knn_testErr <- mean(predict(knn_model, hof_test[,2:17]) != hof_test$inducted)



# NAIVE BAYES

# Specify the type of training method used and the number of folds
nb_ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
nb_model <- train(inducted ~.,
                  data = hof_train,
                  method = "nb",
                  trControl = nb_ctrl)

nb_model
varImp(nb_model)
plot(varImp(nb_model))

# New Naive Bayes model for testing data
nb_modelv2 <- train(inducted ~ W + IP + L + H + SO + SHO + BB
                    + CG + totalAwards + ER + GS + G,
                    data = hof_train,
                    method = "nb",
                    trControl = nb_ctrl)

# Training error using selected variables
nb_pred <- predict(nb_modelv2, hof_train[,2:17])
nb_trainErr <- mean(nb_pred != hof_train$inducted)

# Testing error using selected variables
nb_testErr <- mean(predict(nb_modelv2, hof_test[,2:17]) != hof_test$inducted)



# ERROR TABLES

# Training error table

training_errors = c(logreg_trainErr, nb_trainErr, knn_trainErr, rf_trainErr)
labels = c("Logistic Regression", "Naive Bayes", "KNN k = 6", "Random Forest")
training_results = data.frame("model" = labels, "training error" = training_errors)


# Testing error table

testing_errors = c(logreg_testErr, nb_testErr, knn_testErr, rf_testErr)
testing_results = data.frame("model" = labels, "testing error" = testing_errors)

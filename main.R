# Packages
# --------------------------------------------------------

library(gdata)   # CSV
library(plyr)    # Preprocessing
library(ggplot2) # Visualization
library(lubridate)

# Clasification
library(xgboost)
library(randomForest)
library(gbm)

# Initialization
# --------------------------------------------------------

# Change the working directory
setwd( "Projects/DataScience/Animales")
getwd()

# Open the train
train <- read.csv("train.csv")
test <- read.csv("test.csv")
all <- rbind.fill(train, test)
id <- test$ID

# Seed
set.seed(1234)



# --------------------------------------------------------
#
# Preprocessing
#
# --------------------------------------------------------

# NAME
# -------------------------

# Name -> HasName ( True / False )
all$Name <- as.character( all$Name )
all$HasName[all$Name != ""] <- "True"
all$HasName[all$Name == ""] <- "False"
all$HasName <- as.factor( all$HasName )

all$NameLength <- sapply(all$Name, nchar )
all$NameLength <- as.factor(all$NameLength)

# Empty names -> Unknown
all$Name[all$Name == ""] <- "Unknown"

# Count names
names <- vector()

count_names <- function(x){
  
    if( !(is.na(names[x])) ){
      names[[x]] <<- names[[x]]+1
    }
    else{
      names[[x]] <<- 1
    }
  
    names[x]
}

get_count <- function(x){
  names[[x]]
}

for( name in all$Name ){
  count_names(name)
}

all$NameCount <- lapply(all$Name, get_count)
all$NameCount <- unlist(all$NameCount)
all$NameCount <- ifelse( all$NameCount > 10000, "Unknown", 
                 ifelse( all$NameCount >= 10, "Usual", "Unusual" ) )
all$NameCount <- as.factor(all$NameCount)


# BREED
# -------------------------

# Breed -> BreedMixed ( True / False )
all$Breed <- as.character( all$Breed )
all$BreedMix <- all$Breed
all$BreedMix[grep("Mix", all$BreedMix)] <- 'True'
all$BreedMix[all$BreedMix != "True"] <- "False"
all$BreedMix <- as.factor( all$BreedMix )


#ggplot( all, aes( x = BreedMix )) + geom_bar()

# Breed types
get_numbreed <- function(){
  
  breed_list <- c()
  
  for( data in all$Breed ){
    breed <- strsplit(data,"/")[[1]]
    
    for( type in breed ){
      type <- gsub(" Mix","",type)
      #if( !(type %in% breed_list) ){
      if(TRUE){
        breed_list <- c(type, breed_list)
      }
    }
    
  }
  
  breed_list
}


# Breed columns
delete_black <- function(x){
  x <- gsub("Black/","",x)
  x <- gsub("Black","",x)
  x
}

all$Breed <- lapply(all$Breed, delete_black)
all$Breed <- unlist(all$Breed)

get_breed <- function(x){
  
  split_breed <- gsub(" Mix","",strsplit(x, "/")[[1]][1])
  
  split_breed
}

get_breed2 <- function(x){
  
  split_breed <- gsub(" Mix","",strsplit(x, "/")[[1]][2])
  
  if(is.na(split_breed))
    split_breed <- "Unknown"
  
  split_breed
}

all$BreedA <- lapply(all$Breed,get_breed)
#all$BreedB <- lapply(all$Breed,get_breed2)

all$BreedA <- unlist(all$BreedA)
#all$BreedB <- unlist(all$BreedB)

all$BreedA <- as.factor(all$BreedA)
#all$BreedB <- as.factor(all$BreedB)



# Breed -> AnimalSize

small <- c("Domestic Shorthair", "Chihuahua Shorthair", "Domestic Medium Hair", "Siamese", 
           "Miniature Poodle", "Rat Terrier", "Jack Russell Terrier", "Yorkshire Terrier",
           "Shih Tzu", "Domestic Longhair", "Miniature Schnauzer", "Pug", "Miniature Pinscher",
           "Boston Terrier", "Pomeranian", "Maine Coon", "Toy Poodle", "Manx", "Pekingese",
           "Norwich Terrier","Chihuahua Longhair", "Cairn Terrier", "Maltese", "Cardigan Welsh Corgi",
           "Snowshoe", "Lhasa Apso", "Dachshund Wirehair", "Dachshund Longhair", "Russian Blue",
           "Norfolk Terrier", "Pembroke Welsh Corgi", "Bruss Griffon", "Bichon Frise", "West Highland",
           "Papillon", "Himalayan")

medium <- c("Australian Cattle Dog","Pit Bull","Australian Shepherd","Beagle", "Border Collie",
            "Australian Kelpie", "Pit Bull", "Dachshund","Australian Kelpie", "Staffordshire",
            "Cocker Spaniel", "American Pit Bull Terrier", "Border Terrier", "English Bulldog",
            "Bull Terrier", "Tan Hound","Wire Hair Fox Terrier", "Shiba Inu", "Standard Schnauzer",
            "American Staffordshire Terrier", "Basset Hound", "Queensland Heeler","Manchester Terrier",
            "Chinese Sharpei", "Collie Smooth", "Soft Coated Wheaten Terrier", "Parson Russell Terrier",
            "Harrier", "Basenji")

big <- c("Labrador Retriever", "German Shepherd", "Siberian Husky","Rottweiler","Pointer", "Boxer",
         "Great Pyrenees","American Bulldog", "Anatol Shepherd", "Chow Chow", "Doberman Pinsch",
         "Blue Lacy","Carolina Dog","Whippet", "Shetland Sheepdog", "Redbone Hound", "Weimaraner",
         "Belgian Malinois" ,"Vizsla","German Shorthair Pointer","Dogo Argentino", "Greyhound",
         "Alaskan Husky", "Standard Poodle", "Catahoula", "Plott Hound", "Golden Retriever",
         "Mouth Cur", "Flat Coat Retriever", "Great Dane", "Mastiff", "Rhod Ridgeback", "Akita",
         "Collie Rough","Dalmatian","Italian Greyhound")

all$AnimalSize <- ifelse( all$BreedA %in% small, "Small",
                  ifelse( all$BreedA %in% big, "Big", "Medium"))
all$AnimalSize <- as.factor(all$AnimalSize)
#summary(all$AnimalSize)
#ggplot( all, aes( x = AnimalSize )) + geom_bar()



# AnimalStatus
# -------------------------

# Sex -> Male / Female / Unknown
all$SexuponOutcome <- as.character(all$SexuponOutcome)
all$Sex <- sapply( all$SexuponOutcome, function(x){strsplit(x,' ')[[1]][2]} )
all$Sex <- as.factor(all$Sex)

all$Status <- sapply( all$SexuponOutcome, function(x){strsplit(x,' ')[[1]][1]} )
all$Status <- as.factor(all$Status)

all$SexuponOutcome <- as.factor(all$SexuponOutcome)



# AGE
# -------------------------

age_calc <- function (x){
  data <- strsplit(gsub('s','',x), ' ')
  number <- as.integer(data[[1]][1])
  scale <- data[[1]][2]
  factor <- ifelse(scale == 'day', 1, ifelse(scale == 'week', 7, ifelse(scale == 'month', 30, ifelse(scale == 'year', 365, NA))))
  age <- factor*number
  age
}

all$Age <- sapply(all$AgeuponOutcome, age_calc )
age_mean <- mean(all$Age, na.rm = T)

all$Age <- ifelse( is.na(all$Age), age_mean, all$Age )
all$Age <- ifelse( all$Age < 180, "Baby", ifelse( all$Age < 540, "Young", "Adult" ) )
all$Age <- as.factor(all$Age)

#qplot(train$Age, geom="histogram")
#ggplot( train, aes( x = Age )) + geom_bar()



# DATETIME
# -------------------------

all$DateTime <- as.character(all$DateTime)

# Year
all$Year <- year( all$DateTime )
all$Year <- as.factor( all$Year )

# Month
all$Month <- month( all$DateTime )
all$Month <- as.factor( all$Month )

# Day
all$Day <- day( all$DateTime )
all$Day <- as.factor( all$Day )

# Week Day
all$WeekDay <- wday( all$DateTime )
all$WeekDay <- as.factor(all$WeekDay)

# Hour
all$Hour <- as.factor(hour(all$DateTime))

# Season
#all$Month <- as.numeric(all$Month)
#all$Season <- ifelse( all$Month == 12 | all$Month <= 2, "Winter",
#              ifelse( all$Month >= 3 & all$Month <= 5, "Spring",
#              ifelse( all$Month >= 6 & all$Month <= 8, "Summer", "Autumn" )))
#all$Season <- as.factor(all$Season)
#summary(all$Season)



# COLOR
# -------------------------

# Number of colors -> MAX = 2
get_numcolor <- function (x){
  num_colors <- strsplit(x, "/")[[1]]
  length(num_colors)
}

all$Color <- as.character(all$Color)
all$NumColor <- lapply(all$Color, get_numcolor)
all$NumColor <- unlist(all$NumColor)
all$NumColor <- as.factor(all$NumColor)

# Colors

colors <- c("Black","White","Brown","Blue","Orange","Calico","Chocolate","Gold","Red","Tan","Tortie","Yellow")

get_color <- function(x){
  
  split_color <- strsplit(x, "/")[[1]]
  
  for( iter in colors ){
    
    if( grepl(iter,split_color[1]))
      return(iter)
    
  }
  
  "Unknown"
}

get_color2 <- function(x){
  
  split_color <- strsplit(x, "/")[[1]]
  
  for( iter in colors ){
    
    if( grepl(iter,split_color[2]))
      return(iter)
    
  }
  
  "Unknown"
}

all$ColorA <- lapply(all$Color,get_color)
all$ColorB <- lapply(all$Color,get_color2)

all$ColorA <- unlist(all$ColorA)
all$ColorB <- unlist(all$ColorB)

all$ColorA <- as.factor(all$ColorA)
all$ColorB <- as.factor(all$ColorB)

# Divide data

all$ID <- NULL
all$AnimalID <- NULL
all$OutcomeSubtype <- NULL
all$Color <- NULL
all$Breed <- NULL
#all$BreedA <- NULL
all$Name <- NULL
all$DateTime <- NULL
all$AgeuponOutcome <- NULL

train <- all[1:26729,]
test <- all[26730:nrow(all),]
remove(all)
test$OutcomeType <- NULL

# --------------------------------------------------------
#
# Classification
#
# --------------------------------------------------------

colnames(train)


# XGBoost
# --------------------------------------------------------

y <- as.numeric(train$OutcomeType)-1
y <- data.matrix(y)

train_features <- data.frame(sapply( train, as.numeric ))
train_features$OutcomeType <- NULL

train_features <- data.matrix(train_features)
test_features <- data.matrix(test)


# CV
# ---------------------

cv.nround = 300
cv.depth = 8
cv.eta = 0.05
cv.nfold = 5
cv.esr = 25

params <- list("objective" = "multi:softprob", 
                 "nthread" = 2, 
                 "eta" = cv.eta, 
                 "max.depth" = cv.depth,
                 "subsample" = 0.8,
                 "maximize" = FALSE, 
                 "verbose" = TRUE, 
                 "early.stop.round" = cv.esr, 
                 "eval_metric" = "mlogloss", 
                 "num_class" = 5)

bst.cv = xgb.cv(param=params, 
            data = data.matrix(train), 
            label = y,
            nfold = cv.nfold,
            nrounds = cv.nround)


# cv error plot
cv_error <- bst.cv$test.mlogloss.mean
min <- which.min(cv_error)
print(paste(min, cv_error[min]))


# Single XGB
# ---------------------

#bstDMatrix <- xgboost(data = data.matrix(train), maximize = FALSE, label=y, max.depth = 10, eval_metric = "mlogloss", eta = 0.05, nthread = 4, nround = 150, objective = "multi:softprob", num_class = 5)

bstDMatrix <- xgboost(param = cv.param, data = train_features, maximize = FALSE, label=y, max.depth = cv.depth, eval_metric = "mlogloss", eta = cv.eta, nthread = 2, nround = min, objective = "multi:softprob", num_class = 5)

pred <- predict(bstDMatrix, data.matrix(test))

probs <- t(matrix(pred, nrow=5, ncol=length(pred)/5))
probs <- cbind( 'ID' = id, probs )
colnames(probs) <- c('ID', 'Adoption', 'Died', 'Euthanasia', 'Return_to_owner', 'Transfer')

head(probs)

# Saving results
write.csv(probs, file="submit_11_xgboost.csv", col.names = T, row.names = F)


# XGBoost ensemble
# ---------------------

num_xgb_ens = 200

results <- vector("list", length=num_xgb_ens)
for( i in 1:num_xgb_ens){
  print( paste("Training model: ", i ) )
  model <- xgboost( param = params, data = train_features, label = y, nround = min)
  results[[i]] <- predict(model, newdata = test_features)
}

pred <- colMeans(do.call(rbind, results))

probs <- t(matrix(pred, nrow=5, ncol=length(pred)/5))
probs <- cbind( 'ID' = id, probs )
colnames(probs) <- c('ID', 'Adoption', 'Died', 'Euthanasia', 'Return_to_owner', 'Transfer')

write.csv(probs, file = "xgb_ensemble_200.csv", row.names = F)
remove(results)
remove(pred)

# RandomForest
# --------------------------------------------------------

train$Year <- as.numeric(train$Year)
train$Month <- as.numeric(train$Month)
#train$Age <- as.numeric(train$Age)

train$OutcomeType <- as.factor(train$OutcomeType)
train$NameCount <- as.factor(train$NameCount)
test$NameCount <- as.factor(test$NameCount)

summary(train$NameCount)
summary(train$AnimalSize)

summary(train$Age)

rf.outcomes = randomForest(OutcomeType ~ AnimalType+SexuponOutcome+Year+Month+Age+BreedMix+Sex+HasName+Sterilized+WeekDay+Season+Hour+ColorA+ColorB+AnimalSize+NameCount, 
                           data = train, 
                           n.trees = 300,
                           importance = T,
                           node.size = 6)

rf.pred = predict(rf.outcomes, newdata = train, type = "response")

# Plotting relative importance of the variables
plot(rf.outcomes)
varImpPlot(rf.outcomes)

# make predicion on animal outcome
rf.pred.test = predict(rf.outcomes, test, type = "response")
head(rf.pred.test)

# Output file for submission
sol = data.frame(ID = id, rf.pred.test)
head(sol)
write.csv(sol, file = "submission_08_rf.csv", row.names = F)


# Ensemble
# ---------------------

rf_models <- vector("list", length = 5)
for(i in 1:5){
  print(paste('training model:', i))
  rf_models[[i]] <- randomForest(OutcomeType~., data = train, 
                                 mtry = 4, ntree = 1000, do.trace = T)
}


rf_model <- do.call(combine, rf_models)

predrf <- predict(rf_model, newdata = test, type = "prob")
predrf <- data.frame(sample$ID, predrf)
colnames(predrf) <- names(sample)
write.csv(predrf, file = "rf_test.csv", row.names = F) 
write.csv(caca, file = "submit_rf_ensemble.csv", row.names = F) 




# GBM
# --------------------------------------------------------

train_features <- data.frame(sapply( train, as.numeric ))
head(train)

gbm1 <- gbm(
  OutcomeType ~ .,
  data=train_features,
  distribution="multinomial",
  shrinkage=0.05,
  n.trees=300,
  interaction.depth=6L,
  train.fraction=0.8,
  keep.data=FALSE,
  verbose=TRUE
)

pred_gbm <- predict(gbm1,test,type="response")
dim(pred_gbm) <- c(nrow(test),5)
colnames(pred_gbm) <- levels(train$OutcomeType)
options(scipen=100)

head(pred_gbm)
res <- data.frame(ID = id, pred_gbm)
head(res)

write.csv(res, file="gbm_1100.csv", col.names = T, row.names = F)

gbmImp <- varImp


# Ensemble GBM
# ---------------------

num_gbm <- 100
predictions <- vector("list", length=10)

results <- vector("list", length=num_gbm)
for( i in 1:num_gbm){
  print( paste("Training model: ", i ) )
  model <- gbm(
    OutcomeType ~ .,
    data=train_features,
    distribution="multinomial",
    shrinkage=0.1,
    n.trees=300,
    interaction.depth=6L,
    train.fraction=0.8,
    keep.data=FALSE,
    verbose=TRUE
  )
  
  results[[i]] <- predict(model, test, type="response")

}

pred <- colMeans(do.call(rbind, results))
dim(pred) <- c(nrow(test),5)
colnames(pred) <- levels(train$OutcomeType)
options(scipen=100)

head(pred_gbm)
res <- data.frame(ID = id, pred)
head(res)

write.csv(res, file="gbm_ensemble.csv", col.names = T, row.names = F)


# Mix Ensemble
# --------------------------------------------------------


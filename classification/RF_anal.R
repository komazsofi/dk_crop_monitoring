library(randomForest)
library(caret)
library(e1071)
library(dplyr)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/AggrTrainingClasses/")

database=read.csv("forRF_try1.csv")
#database_c=na.omit(database)

database_c= database %>% select_if(~ !any(is.na(.)))

# prep

database_c$class <- as.factor(database_c$class)
table(database_c$class)

set.seed(1234)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats=5,
                          search = "grid",
                          p=0.75)

ind <- sample(2, nrow(database_c), replace = TRUE, prob = c(0.7, 0.3))
train <- database_c[ind==1,]
test <- database_c[ind==2,]

# RF

rf <- train(class~.,data=train[,c(2,4:62)],
                        method = "rf",
                        trControl = trControl,
                        importance=T)

print(rf)
varImp(rf)

# prediction

pred <- predict(rf, test[,c(2,4:62)])
confusionMatrix(pred, test$class)
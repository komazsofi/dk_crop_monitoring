library(randomForest)
library(caret)
library(e1071)
library(dplyr)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_3/layers/forClassification/")

database=read.csv("AggrTrainingClasses_Intersected.csv")
database_c=na.omit(database)

database_c_sel=database_c[c(2:15)]
names(database_c_sel)=names(database_c[c(3:16)])

#database_c= database %>% select_if(~ !any(is.na(.)))

# prep

database_c_sel$class <- as.factor(database_c_sel$class)
table(database_c_sel$class)

set.seed(1234)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats=5,
                          search = "grid",
                          p=0.75)

ind <- sample(2, nrow(database_c_sel), replace = TRUE, prob = c(0.7, 0.3))
train <- database_c_sel[ind==1,]
test <- database_c_sel[ind==2,]

table(train$class)
table(test$class)

# RF

rf <- train(class~.,data=train,
                        method = "rf",
                        trControl = trControl,
                        importance=T)

rf2 <- train(class~.,data=train[c(1:10)],
            method = "rf",
            trControl = trControl,
            importance=T)

rf3 <- train(class~.,data=train[c(1:8)],
            method = "rf",
            trControl = trControl,
            importance=T)

print(rf)
varImp(rf)

varImpPlot(rf$finalModel)

saveRDS(rf,"rf_model_all.rds")
rf=readRDS("rf_model_all.rds")

# prediction

pred <- predict(rf, test)
confusionMatrix(pred, test$class)
conf_m_norm=prop.table(table(pred, test$class),margin=2)
conf_m=table(pred, test$class)

write.csv(conf_m_norm,"conf_m_norm.csv")
write.csv(conf_m,"conf_m.csv")

pred <- predict(rf2, test[c(1:10)])
confusionMatrix(pred, test$class)

pred <- predict(rf3, test[c(1:8)])
confusionMatrix(pred, test$class)
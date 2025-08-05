library(caret)
dane <- read.csv("C:/Users/kuba/OneDrive/Pulpit/uczenie maszynowe/dane/stroke.csv", sep=";")

#usuwanie brakow danych
sum(sapply(dane, function(x) sum(is.na(x))))
dane <-na.omit(dane)
str(dane1)
table(dane1$Stroke)
#zamiana zmiennych kategorycznych 
#gender i evermarried na 0 1 

dummy_data <- dummyVars(~ work_type, data = dane)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane))
dane1 <- cbind(dane, dummy_data_transformed)

dummy_data <- dummyVars(~ Residence_type, data = dane1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane1))
dane1 <- cbind(dane1, dummy_data_transformed)

dummy_data <- dummyVars(~ smoking_status, data = dane1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane1))
dane1 <- cbind(dane1, dummy_data_transformed)

dane1$gender <- ifelse(dane1$gender == "Male", 1, 0)
dane1$ever_married <- ifelse(dane1$ever_married == "Yes", 1, 0)
dane1$Stroke <- dane1$stroke

#dane1$`work_typeSelf-employed`<- colnames(c("work_typeSelfemployed"))
dane1 <- dane1[,-c(6,7,10,11)]

dane1$Stroke <- factor(dane1$Stroke,levels = c("0", "1"), labels = c("nie", "tak"))
str(dane1)
#Podział danych
set.seed(123)
ind <- sample(2, nrow(dane1), replace = TRUE, prob = c(0.7, 0.3))
train_set <- dane1[ind==1,]
test_set <- dane1[ind==2,]


# K-NN
trControl <- trainControl(method = "repeatedcv", #repeated cross-validation
                          number = 10,  # number of resampling iterations
                          repeats = 1,  # sets of folds to for repeated cross-validation
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)  # classProbs needed for ROC
set.seed(1234)
fit <- train(Stroke ~ ., 
             data = train_set,
             tuneGrid   = expand.grid(k = 1:10),
             method = "knn",
             tuneLength = 20,
             metric     = "ROC", 
             trControl = trControl,
             preProc = c("center", "scale"))  # necessary task

# Model performance
fit
plot(fit)
varImp(fit)

pred <- predict(fit, newdata = test_set )
confusionMatrix(pred, test_set$Stroke, positive = 'tak' )



fit <- train(Stroke ~ ., 
             data = train_set,
             #tuneGrid   = expand.grid(k = 1:10),
             method = "kknn",
             tuneLength = 20,
             metric     = "ROC", 
             trControl = trControl,
             preProc = c("center", "scale"))  # necessary task

yes# Model performance
fit
plot(fit)
varImp(fit)

pred <- predict(fit, newdata = test_set )
confusionMatrix(pred, test_set$Stroke, positive = 'tak' )


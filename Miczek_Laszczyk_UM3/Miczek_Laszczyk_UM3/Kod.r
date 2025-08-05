install.packages("adabag")
library(rpart.plot)
library(mlr)
library(pROC)
library(caret)
library(rpart)
library(adabag)
dane <-  read.csv("Heart2.csv", sep=";")

str(dane)
dane$heartDisease <- as.factor(dane$heartDisease)
#Podział danych
set.seed(123)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.7, 0.3))
train_set <- dane[ind==1,]
test_set <- dane[ind==2,]


results <- data.frame(MaxDepth = integer(0), TrainingAccuracy = numeric(0), TestAccuracy = numeric(0))

for (max_depth in 1:10) {
  # Budowa drzewa z określoną głębokością
  model <- rpart(heartDisease ~ ., data = train_set, method = 'class', control = rpart.control(maxdepth = max_depth))
  
  # Prognozowanie na zbiorze treningowym
  predicted <- predict(model, train_set, type = 'class')
  train_accuracy <- sum(predicted == train_set$heartDisease) / nrow(train_set)
  
  # Prognozowanie na zbiorze testowym
  predicted1 <- predict(model, test_set, type = 'class')
  test_accuracy <- sum(predicted1 == test_set$heartDisease) / nrow(test_set)
  
  # Dodawanie wyników do ramki danych
  results <- rbind(results, data.frame(MaxDepth = max_depth, TrainingAccuracy = train_accuracy, TestAccuracy = test_accuracy))
}

# Wyświetlenie wyników
print(results)

test_set <- test_set[,-c(3,10,11,12,13)]
train_set <- train_set[,-c(3,10,11,12,13)]

#Hyperparameter Tuning training with mlr
getParamSet("classif.rpart")
traintask <- makeClassifTask(
  data=train_set, 
  target="heartDisease"
)
# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV",iter = 5,predict = "both")
# Define Measure
measure = acc
learner <- makeLearner("classif.rpart", predict.type = "prob", parms = list(split = "information"))

param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:5),
  makeDiscreteParam("cp", values = 0),
  makeDiscreteParam("minsplit", values=1:20),
  makeDiscreteParam('xval',value =10)
)

dt_tuneparam_multi <- tuneParams(learner=learner, 
                                 task=traintask, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid, 
                                 control=control_grid, 
                                 show.info = TRUE)

# Extracting best Parameters from Multi Search
best_params = setHyperPars( 
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam_multi$x
)

best_model_multi <- mlr::train(best_params, traintask)
best_tree_model <- best_model_multi$learner.model
rpart.plot(best_tree_model,rounding = FALSE)
predicted2 <- predict(best_tree_model, newdata = test_set, type = "class")
confusionMatrix(predicted2, test_set$heartDisease)

#przycinanie 
best_tree_model$cptable
best_cp <- best_tree_model$cptable[which.min(best_tree_model$cptable[,"xerror"]),"CP"]
plotcp(best_tree_model)

# Teraz możemy przyciąć nasze drzewo
pruned_tree1 <- prune(best_tree_model, cp = 0.528169014)
pruned_tree2 <- prune(best_tree_model, cp = 0.049295775)
pruned_tree3 <- prune(best_tree_model, cp = 0.017605634)
pruned_tree4 <- prune(best_tree_model, cp = 0.000000000)
#testowanie przycietego drzewa na zbiorze treningowym 
predicted3 <- predict(pruned_tree1, newdata = train_set, type = "class")
confusionMatrix(predicted3, train_set$heartDisease)
# Testowanie przyciętego drzewa na zbiorze testowym
predicted4 <- predict(pruned_tree1, newdata = test_set, type = "class")
confusionMatrix(predicted4, test_set$heartDisease)

#testowanie przycietego drzewa na zbiorze treningowym 
predicted3 <- predict(pruned_tree2, newdata = train_set, type = "class")
confusionMatrix(predicted3, train_set$heartDisease)
# Testowanie przyciętego drzewa na zbiorze testowym
predicted4 <- predict(pruned_tree2, newdata = test_set, type = "class")
confusionMatrix(predicted4, test_set$heartDisease)

#testowanie przycietego drzewa na zbiorze treningowym 
predicted3 <- predict(pruned_tree3, newdata = train_set, type = "class")
confusionMatrix(predicted3, train_set$heartDisease)
# Testowanie przyciętego drzewa na zbiorze testowym
predicted4 <- predict(pruned_tree3, newdata = test_set, type = "class")
confusionMatrix(predicted4, test_set$heartDisease)

#testowanie przycietego drzewa na zbiorze treningowym 
predicted3 <- predict(pruned_tree4, newdata = train_set, type = "class")
confusionMatrix(predicted3, train_set$heartDisease)
# Testowanie przyciętego drzewa na zbiorze testowym
predicted4 <- predict(pruned_tree4, newdata = test_set, type = "class")
confusionMatrix(predicted4, test_set$heartDisease)



test_set <- test_set[,-c(3,10,11,12,13)]
train_set <- train_set[,-c(3,10,11,12,13)]

#Hyperparameter Tuning training with mlr
getParamSet("classif.rpart")
traintask <- makeClassifTask(
  data=train_set, 
  target="heartDisease"
)
# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV",iter = 5,predict = "both")
# Define Measure
measure = acc
learner <- makeLearner("classif.rpart", predict.type = "prob", parms = list(split = "information"))

param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=1:10),
  makeDiscreteParam("cp", values = 0),
  makeDiscreteParam("minsplit", values=1:40),
  makeDiscreteParam('xval',value =10)
)

dt_tuneparam_multi <- tuneParams(learner=learner, 
                                 task=traintask, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid, 
                                 control=control_grid, 
                                 show.info = TRUE)

# Extracting best Parameters from Multi Search
best_params = setHyperPars( 
  makeLearner("classif.rpart", predict.type = "prob"), 
  par.vals = dt_tuneparam_multi$x
)

best_model_multi <- mlr::train(best_params, traintask)
best_tree_model <- best_model_multi$learner.model
rpart.plot(best_tree_model)
predicted2 <- predict(best_tree_model, newdata = test_set, type = "class")
confusionMatrix(predicted2, test_set$heartDisease)

probabilities <- predict(best_tree_model, newdata = test_set, type = "prob")[,2]
roc_obj <- roc(test_set$heartDisease, probabilities)
auc <- round(auc(test_set$heartDisease, probabilities),4)

#przycinanie 
best_tree_model$cptable
best_cp <- best_tree_model$cptable[which.min(best_tree_model$cptable[,"xerror"]),"CP"]
plotcp(best_tree_model)
# Teraz możemy przyciąć nasze drzewo
pruned_tree <- prune(best_tree_model, cp = best_cp)
print(pruned_tree)
rpart.plot(pruned_tree,roundint = FALSE)

#testowanie przycietego drzewa na zbiorze treningowym 
predicted3 <- predict(pruned_tree, newdata = train_set, type = "class")
confusionMatrix(predicted3, train_set$heartDisease)
# Testowanie przyciętego drzewa na zbiorze testowym
predicted4 <- predict(pruned_tree, newdata = test_set, type = "class")
confusionMatrix(predicted4, test_set$heartDisease)


#sprawdzenie ktora zmienna ma najwieksze znaczenie 
importance <- varImp(pruned_tree)
print(importance)

#do wykresu
probabilities <- predict(pruned_tree, newdata = test_set, type = "prob")[,2]
roc_obj <- roc(test_set$heartDisease, probabilities)
auc <- round(auc(test_set$heartDisease, probabilities),4)

# Rysowanie krzywej ROC
ggroc(roc_obj,colour = 'steelblue',size=2,legacy.axes = TRUE) +
  geom_abline(linetype = "dashed") +
  theme(panel.border = element_rect(color = 'black',fill = NA,size = 1),
        panel.background = element_rect(fill='gray95'),
        plot.background = element_rect(color = 'black', size = 1) 
  )+
  ggtitle(paste0('Krzywa ROC ', '(AUC = ', auc, ')')) +
  labs(x = "1 - Swoistość",
       y = "Swoistość")





























#bagging 
model1 <- bagging(heartDisease~., data=train_set)
pred <- predict.bagging(model1, newdata=test_set)
pred1 <- as.factor(pred$class)
confusionMatrix(pred1,test_set$heartDisease)

model2 <- bagging(heartDisease~., data=train_set,mfinal = 150, control = rpart.control(maxdepth = 4, cp = 0.01))
pred <- predict.bagging(model2, newdata=test_set)
pred2 <- as.factor(pred$class)
confusionMatrix(pred2,test_set$heartDisease)









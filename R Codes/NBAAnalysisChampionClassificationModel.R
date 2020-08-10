##Import NBA Salary Data
NBASalaryAnalysisData <- read.csv("C:/Users/dpesl/Desktop/NBASalaryAnalysisData.csv",
                                  header = TRUE)
NBASalaryAnalysisData2020 <- read.csv("C:/Users/dpesl/Desktop/NBASalaryAnalysisData2020.csv",
                                      header = TRUE)

##Remove first column (row numbers)
NBASalaryAnalysisData <- NBASalaryAnalysisData[,-1]
NBASalaryAnalysisData2020 <- NBASalaryAnalysisData2020[,-1]

##install.packages("matrixStats")
library(matrixStats)

##Let us separate perGame and perPoss metrics
MasterPerGame <- NBASalaryAnalysisData[,-(78:121)]
MasterPerGame2020 <- NBASalaryAnalysisData2020[,-(76:119)]
MasterPerGame[,9] <- as.character(MasterPerGame[,9])
for (i in 1:dim(MasterPerGame)[1]) {
  if(MasterPerGame[i,9] == 'CHAMPIONS'){
    MasterPerGame[i,9] <- 0
  }
  if(MasterPerGame[i,9] == 'FINALS'){
    MasterPerGame[i,9] <- 1
  }
  if(MasterPerGame[i,9] == 'CFINALS'){
    MasterPerGame[i,9] <- 2
  }
  if(MasterPerGame[i,9] == '2R'){
    MasterPerGame[i,9] <- 3
  }
  if(MasterPerGame[i,9] == '1R'){
    MasterPerGame[i,9] <- 4
  }
  if(MasterPerGame[i,9] == 'MISSED'){
    MasterPerGame[i,9] <- 5
  }
}
MasterPerGame[,9] <- as.numeric(MasterPerGame[,9])
##Note that we scale variables according to season
##this is done because we want to avoid running into problems with
##changes in game plans (we will see whether teams are better at 3pts compared
##to league in a paricular season, vs over 29 seasons)
##then we re-scale all together
MasterPerGame2020[,-c((1:5),8)] <- scale(MasterPerGame2020[,-c((1:5),8)])
MasterPerGame[(1:27),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(1:27),-c((1:5),7,(9:10))])
MasterPerGame[(28:54),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(28:54),-c((1:5),7,(9:10))])
MasterPerGame[(55:81),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(55:81),-c((1:5),7,(9:10))])
MasterPerGame[(82:108),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(82:108),-c((1:5),7,(9:10))])
MasterPerGame[(109:135),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(109:135),-c((1:5),7,(9:10))])
MasterPerGame[(136:164),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(136:164),-c((1:5),7,(9:10))])
MasterPerGame[(165:193),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(165:193),-c((1:5),7,(9:10))])
MasterPerGame[(194:222),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(194:222),-c((1:5),7,(9:10))])
MasterPerGame[(223:251),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(223:251),-c((1:5),7,(9:10))])
MasterPerGame[(252:280),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(252:280),-c((1:5),7,(9:10))])
MasterPerGame[(281:309),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(281:309),-c((1:5),7,(9:10))])
MasterPerGame[(310:338),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(310:338),-c((1:5),7,(9:10))])
MasterPerGame[(339:367),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(339:367),-c((1:5),7,(9:10))])
MasterPerGame[(368:396),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(368:396),-c((1:5),7,(9:10))])
MasterPerGame[(397:426),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(397:426),-c((1:5),7,(9:10))])
MasterPerGame[(427:456),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(427:456),-c((1:5),7,(9:10))])
MasterPerGame[(457:486),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(457:486),-c((1:5),7,(9:10))])
MasterPerGame[(487:516),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(487:516),-c((1:5),7,(9:10))])
MasterPerGame[(517:546),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(517:546),-c((1:5),7,(9:10))])
MasterPerGame[(547:576),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(547:576),-c((1:5),7,(9:10))])
MasterPerGame[(577:606),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(577:606),-c((1:5),7,(9:10))])
MasterPerGame[(607:636),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(607:636),-c((1:5),7,(9:10))])
MasterPerGame[(637:666),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(637:666),-c((1:5),7,(9:10))])
MasterPerGame[(667:696),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(667:696),-c((1:5),7,(9:10))])
MasterPerGame[(697:726),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(697:726),-c((1:5),7,(9:10))])
MasterPerGame[(727:756),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(727:756),-c((1:5),7,(9:10))])
MasterPerGame[(757:786),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(757:786),-c((1:5),7,(9:10))])
MasterPerGame[(787:816),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(787:816),-c((1:5),7,(9:10))])
MasterPerGame[(817:846),-c((1:5),7,(9:10))] <- scale(MasterPerGame[(817:846),-c((1:5),7,(9:10))])
MasterPerGame2020[,-c((1:5),8)] <- (MasterPerGame2020[,-c((1:5),8)] - colMeans(MasterPerGame[,-c((1:5),7,9,10)]))/colSds(as.matrix(MasterPerGame[,-c((1:5),7,9,10)]))
MasterPerGame[,-c((1:5),7,(9:10))] <- scale(MasterPerGame[,-c((1:5),7,(9:10))])
MasterPerGame <- MasterPerGame[,-c((1:5),7,10,(12:14),19,20,34,35)]
MasterPerGame2020 <- MasterPerGame2020[,-c((1:5),8,(10:12),17,18,32,33)]
MasterPerPoss <- NBASalaryAnalysisData[,-(34:77)]
MasterPerPoss[,9] <- as.character(MasterPerPoss[,9])
for (i in 1:dim(MasterPerPoss)[1]) {
  if(MasterPerPoss[i,9] == 'CHAMPIONS'){
    MasterPerPoss[i,9] <- 0
  }
  if(MasterPerPoss[i,9] == 'FINALS'){
    MasterPerPoss[i,9] <- 1
  }
  if(MasterPerPoss[i,9] == 'CFINALS'){
    MasterPerPoss[i,9] <- 2
  }
  if(MasterPerPoss[i,9] == '2R'){
    MasterPerPoss[i,9] <- 3
  }
  if(MasterPerPoss[i,9] == '1R'){
    MasterPerPoss[i,9] <- 4
  }
  if(MasterPerPoss[i,9] == 'MISSED'){
    MasterPerPoss[i,9] <- 5
  }
}
MasterPerPoss[,9] <- as.numeric(MasterPerPoss[,9])
MasterPerPoss[(1:27),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(1:27),-c((1:5),7,(9:10))])
MasterPerPoss[(28:54),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(28:54),-c((1:5),7,(9:10))])
MasterPerPoss[(55:81),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(55:81),-c((1:5),7,(9:10))])
MasterPerPoss[(82:108),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(82:108),-c((1:5),7,(9:10))])
MasterPerPoss[(109:135),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(109:135),-c((1:5),7,(9:10))])
MasterPerPoss[(136:164),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(136:164),-c((1:5),7,(9:10))])
MasterPerPoss[(165:193),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(165:193),-c((1:5),7,(9:10))])
MasterPerPoss[(194:222),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(194:222),-c((1:5),7,(9:10))])
MasterPerPoss[(223:251),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(223:251),-c((1:5),7,(9:10))])
MasterPerPoss[(252:280),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(252:280),-c((1:5),7,(9:10))])
MasterPerPoss[(281:309),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(281:309),-c((1:5),7,(9:10))])
MasterPerPoss[(310:338),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(310:338),-c((1:5),7,(9:10))])
MasterPerPoss[(339:367),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(339:367),-c((1:5),7,(9:10))])
MasterPerPoss[(368:396),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(368:396),-c((1:5),7,(9:10))])
MasterPerPoss[(397:426),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(397:426),-c((1:5),7,(9:10))])
MasterPerPoss[(427:456),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(427:456),-c((1:5),7,(9:10))])
MasterPerPoss[(457:486),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(457:486),-c((1:5),7,(9:10))])
MasterPerPoss[(487:516),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(487:516),-c((1:5),7,(9:10))])
MasterPerPoss[(517:546),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(517:546),-c((1:5),7,(9:10))])
MasterPerPoss[(547:576),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(547:576),-c((1:5),7,(9:10))])
MasterPerPoss[(577:606),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(577:606),-c((1:5),7,(9:10))])
MasterPerPoss[(607:636),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(607:636),-c((1:5),7,(9:10))])
MasterPerPoss[(637:666),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(637:666),-c((1:5),7,(9:10))])
MasterPerPoss[(667:696),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(667:696),-c((1:5),7,(9:10))])
MasterPerPoss[(697:726),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(697:726),-c((1:5),7,(9:10))])
MasterPerPoss[(727:756),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(727:756),-c((1:5),7,(9:10))])
MasterPerPoss[(757:786),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(757:786),-c((1:5),7,(9:10))])
MasterPerPoss[(787:816),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(787:816),-c((1:5),7,(9:10))])
MasterPerPoss[(817:846),-c((1:5),7,(9:10))] <- scale(MasterPerPoss[(817:846),-c((1:5),7,(9:10))])
MasterPerPoss[,-c((1:5),7,(9:10))] <- scale(MasterPerPoss[,-c((1:5),7,(9:10))])
MasterPerPoss <- MasterPerPoss[,-c((1:5),7,10,(12:14),19,20,34,35)]

set.seed(2)
samplesize <- floor(0.25 * nrow(MasterPerGame))
Fold1index <- sample(seq_len(nrow(MasterPerGame)), samplesize)
PerGameFold1 <- MasterPerGame[Fold1index,]
Fold2index <- sample(seq_len(nrow(MasterPerGame[-Fold1index,])), samplesize)
PerGameFold2 <- MasterPerGame[Fold2index,]
Fold3index <- sample(seq_len(nrow(MasterPerGame[-c(Fold1index,Fold2index),])), (nrow(MasterPerGame)-2*samplesize)/2)
PerGameFold3 <- MasterPerGame[Fold3index,]
Fold4index <- sample(seq_len(nrow(MasterPerGame[-c(Fold1index,Fold2index,Fold3index),])), (nrow(MasterPerGame)-2*samplesize)/2)
PerGameFold4 <- MasterPerGame[Fold4index,]


ytrain <- ceiling(MasterPerGame$finish/5)
xtrain <- MasterPerGame[,-3]
datatrain <- cbind(ytrain, xtrain)
##Generalized Linear Model Feature Selection
library(caret)
set.seed(2)
cntrl <- rfeControl(functions = lrFuncs, method = "cv", number = 4, repeats = 10)
model.glm <- rfe(datatrain[,(2:63)], as.factor(datatrain[,1]), rfeControl = cntrl, sizes = c(5:25), method = "glm", family = "binomial")
model.glm
model.glm$optVariables

##Discriminant Analysis Feature Selection
##Linear Discriminant
##install.packages("MASS")
library(MASS)
set.seed(2)
cntrl <- rfeControl(functions = ldaFuncs, method = "cv", number = 4, repeats = 10)
model.lda <- rfe(datatrain[,(2:63)], as.factor(datatrain[,1]), rfeControl = cntrl, sizes = c(5:25))
model.lda
model.lda$optVariables

##KNN Feature Selection
##Note we cannot apply rfe methods to KNN
##thus, we shall take variables with importance above 20%
model.knn <- train(as.factor(ytrain)~., data = datatrain,
                   trControl = trainControl(method = "cv", number = 4),
                   preProcess = c("center", "scale"), tuneGrid = expand.grid(k = seq(1,100, by = 1)),
                   method = "knn")
var.imp.knn <- varImp(model.knn)
print(var.imp.knn)
plot(var.imp.knn, top = 20)
knnval <- as.numeric(model.knn$bestTune)

##install.packages("ggplot2")
library(ggplot2)
##install.packages("MLmetrics")
library(MLmetrics)

##Championship Analysis
##1st fold = validation set
MSEglm <- 0
Accuracyglm <- 0
Precisionglm <- 0
Recallglm <- 0
F1glm <- 0
AUCglm <- 0
ConfusMatglm <- vector(mode = "list", length = 4)
MSElda <- 0
Accuracylda <- 0
Precisionlda <- 0
Recalllda <- 0
F1lda <- 0
AUClda <- 0
ConfusMatlda <- vector(mode = "list", length = 4)
MSEknn <- 0
Accuracyknn <- 0
Precisionknn <- 0
Recallknn <- 0
F1knn <- 0
AUCknn <- 0
ConfusMatknn <- vector(mode = "list", length = 4)
ytrain <- ceiling(rbind(cbind(PerGameFold2[,3]),cbind(PerGameFold3[,3]),cbind(PerGameFold4[,3]))/5)
xtrain <- rbind(PerGameFold2[,-3],PerGameFold3[,-3],PerGameFold4[,-3])
datatrain <- cbind(ytrain, xtrain)
ytest <- ceiling(PerGameFold1[,3]/5)
xtest <- cbind(PerGameFold1[,-3])
datatest <- cbind(ytest, xtest)

##Logistic Regression
##install.packages("pROC")
library(pROC)
model.glm <- glm(ytrain~pctTOVOpponentMisc + pctDRBOpponentMisc +
                   paceTeamMisc + fg3mPerGameTeam + trbPerGameOpponent,
                 data = datatrain, family = binomial)
glmtest <- predict(model.glm, datatest, type = "response")
for (i in 1:length(glmtest)) {
  if(glmtest[i] <= 0.5){
    glmtest[i] <- as.factor(0)
  }
  if(glmtest[i] > 0.5){
    glmtest[i] <- as.factor(1)
  }
}
ConfusMatglm[[1]] <- ConfusionMatrix(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))
ConfusMatglm[[1]]
Accuracyglm <- Accuracyglm + ifelse(is.nan(Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest))))
Precisionglm <- Precisionglm + ifelse(is.nan(Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
Recallglm <- Recallglm + ifelse(is.nan(Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
F1glm <- F1glm + ifelse(is.nan(F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
ROCtest <- roc(datatest$ytest, glmtest)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCglm <- AUCglm + AUC(glmtest, datatest$ytest)
MSEglm <- MSEglm + MSE(glmtest, datatest$ytest)

##Discriminant Models
##Linear Discriminant
model.lda <- lda(ytrain~ winsTeam + nrtgTeamMisc + marginVictoryTeam +
                   Ranking + drtgTeamMisc + pctFG2PerGameTeam + pctFGPerGameTeam +
                   pctEFGTeamOppMisc + pctFGPerGameOpponent + pctEFGTeamMisc +
                   ortgTeamMisc + blkPerGameOpponent + pctFG2PerGameOpponent,
                 data = datatrain)
model.lda
ldatest <- predict(model.lda, datatest)
ConfusMatlda[[1]] <-ConfusionMatrix(ldatest$class, datatest$ytest)
ConfusMatlda[[1]]
Accuracylda <- Accuracylda + ifelse(is.nan(Accuracy(ldatest$class, datatest$ytest)),0,Accuracy(ldatest$class, datatest$ytest))
Precisionlda <- Precisionlda + ifelse(is.nan(Precision(datatest$ytest, ldatest$class)),0,Precision(datatest$ytest, ldatest$class))
Recalllda <- Recalllda + ifelse(is.nan(Recall(datatest$ytest, ldatest$class)),0,Recall(datatest$ytest, ldatest$class))
F1lda <- F1lda + ifelse(is.nan(F1_Score(datatest$ytest, ldatest$class)),0,F1_Score(datatest$ytest, ldatest$class))
ROCtest <- roc(datatest$ytest, ldatest$posterior[,1])
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUClda <- AUClda + AUC(ldatest$class, datatest$ytest)
MSElda <- MSElda + MSE(as.numeric(ldatest$class), datatest$ytest)

##K Nearest Neighbours Model
model.knn <- knn3(formula = as.factor(ytrain)~ nrtgTeamMisc + marginVictoryTeam +
                    winsTeam + Ranking + drtgTeamMisc +
                    pctFG2PerGameTeam + pctEFGTeamMisc +
                    pctFGPerGameTeam + pctFG3PerGameOpponent +
                    pctEFGTeamOppMisc + blkPerGameOpponent +
                    pctFGPerGameOpponent + ortgTeamMisc +
                    astPerGameTeam + pctTrueShootingeTeamMisc,
                  data = datatrain, k = knnval)
knntest <- predict(model.knn, datatest, type = "class")
ConfusMatknn[[1]] <- ConfusionMatrix(knntest, datatest$ytest)
ConfusMatknn[[1]]
Accuracyknn <- Accuracyknn + ifelse(is.nan(Accuracy(knntest, datatest$ytest)),0,Accuracy(knntest, datatest$ytest))
Precisionknn <- Precisionknn + ifelse(is.nan(Precision(datatest$ytest, knntest)),0,Precision(datatest$ytest, knntest))
Recallknn <- Recallknn + ifelse(is.nan(Recall(datatest$ytest, knntest)),0,Recall(datatest$ytest, knntest))
F1knn <- F1knn + ifelse(is.nan(F1_Score(datatest$ytest, knntest)),0,F1_Score(datatest$ytest, knntest))
ROCtest <- roc(datatest$ytest, as.numeric(knntest)-1)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCknn <- AUCknn + AUC(knntest, datatest$ytest)
MSEknn <- MSEknn + MSE(as.numeric(knntest)-1, datatest$ytest)

##2nd fold = validation set
ytrain <- ceiling(rbind(cbind(PerGameFold1[,3]),cbind(PerGameFold3[,3]),cbind(PerGameFold4[,3]))/5)
xtrain <- rbind(PerGameFold1[,-3],PerGameFold3[,-3],PerGameFold4[,-3])
datatrain <- cbind(ytrain, xtrain)
ytest <- ceiling(PerGameFold2[,3]/5)
xtest <- cbind(PerGameFold2[,-3])
datatest <- cbind(ytest, xtest)

##Logistic Regression
model.glm <- glm(ytrain~pctTOVOpponentMisc + pctDRBOpponentMisc +
                   paceTeamMisc + fg3mPerGameTeam + trbPerGameOpponent,
                 data = datatrain, family = binomial)
glmtest <- predict(model.glm, datatest, type = "response")
for (i in 1:length(glmtest)) {
  if(glmtest[i] <= 0.5){
    glmtest[i] <- as.factor(0)
  }
  if(glmtest[i] > 0.5){
    glmtest[i] <- as.factor(1)
  }
}
ConfusMatglm[[2]] <- ConfusionMatrix(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))
ConfusMatglm[[2]]
Accuracyglm <- Accuracyglm + ifelse(is.nan(Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest))))
Precisionglm <- Precisionglm + ifelse(is.nan(Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
Recallglm <- Recallglm + ifelse(is.nan(Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
F1glm <- F1glm + ifelse(is.nan(F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
ROCtest <- roc(datatest$ytest, glmtest)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCglm <- AUCglm + AUC(glmtest, datatest$ytest)
MSEglm <- MSEglm + MSE(glmtest, datatest$ytest)

##Discriminant Models
##Linear Discriminant
model.lda <- lda(ytrain~ winsTeam + nrtgTeamMisc + marginVictoryTeam +
                   Ranking + drtgTeamMisc + pctFG2PerGameTeam + pctFGPerGameTeam +
                   pctEFGTeamOppMisc + pctFGPerGameOpponent + pctEFGTeamMisc +
                   ortgTeamMisc + blkPerGameOpponent + pctFG2PerGameOpponent,
                 data = datatrain)
model.lda
ldatest <- predict(model.lda, datatest)
ConfusMatlda[[2]] <- ConfusionMatrix(ldatest$class, datatest$ytest)
ConfusMatlda[[2]]
Accuracylda <- Accuracylda + ifelse(is.nan(Accuracy(ldatest$class, datatest$ytest)),0,Accuracy(ldatest$class, datatest$ytest))
Precisionlda <- Precisionlda + ifelse(is.nan(Precision(datatest$ytest, ldatest$class)),0,Precision(datatest$ytest, ldatest$class))
Recalllda <- Recalllda + ifelse(is.nan(Recall(datatest$ytest, ldatest$class)),0,Recall(datatest$ytest, ldatest$class))
F1lda <- F1lda + ifelse(is.nan(F1_Score(datatest$ytest, ldatest$class)),0,F1_Score(datatest$ytest, ldatest$class))
ROCtest <- roc(datatest$ytest, ldatest$posterior[,1])
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUClda <- AUClda + AUC(ldatest$class, datatest$ytest)
MSElda <- MSElda + MSE(as.numeric(ldatest$class), datatest$ytest)

##K Nearest Neighbours Model
model.knn <- knn3(formula = as.factor(ytrain)~ nrtgTeamMisc + marginVictoryTeam +
                    winsTeam + Ranking + drtgTeamMisc +
                    pctFG2PerGameTeam + pctEFGTeamMisc +
                    pctFGPerGameTeam + pctFG3PerGameOpponent +
                    pctEFGTeamOppMisc + blkPerGameOpponent +
                    pctFGPerGameOpponent + ortgTeamMisc +
                    astPerGameTeam + pctTrueShootingeTeamMisc,
                  data = datatrain, k = knnval)
knntest <- predict(model.knn, datatest, type = "class")
ConfusMatknn[[2]] <- ConfusionMatrix(knntest, datatest$ytest)
ConfusMatknn[[2]]
Accuracyknn <- Accuracyknn + ifelse(is.nan(Accuracy(knntest, datatest$ytest)),0,Accuracy(knntest, datatest$ytest))
Precisionknn <- Precisionknn + ifelse(is.nan(Precision(datatest$ytest, knntest)),0,Precision(datatest$ytest, knntest))
Recallknn <- Recallknn + ifelse(is.nan(Recall(datatest$ytest, knntest)),0,Recall(datatest$ytest, knntest))
F1knn <- F1knn + ifelse(is.nan(F1_Score(datatest$ytest, knntest)),0,F1_Score(datatest$ytest, knntest))
ROCtest <- roc(datatest$ytest, as.numeric(knntest)-1)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCknn <- AUCknn + AUC(knntest, datatest$ytest)
MSEknn <- MSEknn + MSE(as.numeric(knntest)-1, datatest$ytest)

##3rd fold = validation set
ytrain <- ceiling(rbind(cbind(PerGameFold1[,3]),cbind(PerGameFold2[,3]),cbind(PerGameFold4[,3]))/5)
xtrain <- rbind(PerGameFold1[,-3],PerGameFold2[,-3],PerGameFold4[,-3])
datatrain <- cbind(ytrain, xtrain)
ytest <- ceiling(PerGameFold3[,3]/5)
xtest <- cbind(PerGameFold3[,-3])
datatest <- cbind(ytest, xtest)

##Logistic Regression
model.glm <- glm(ytrain~pctTOVOpponentMisc + pctDRBOpponentMisc +
                   paceTeamMisc + fg3mPerGameTeam + trbPerGameOpponent,
                 data = datatrain, family = binomial)
glmtest <- predict(model.glm, datatest, type = "response")
for (i in 1:length(glmtest)) {
  if(glmtest[i] <= 0.5){
    glmtest[i] <- as.factor(0)
  }
  if(glmtest[i] > 0.5){
    glmtest[i] <- as.factor(1)
  }
}
ConfusMatglm[[3]] <- ConfusionMatrix(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))
ConfusMatglm[[3]]
Accuracyglm <- Accuracyglm + ifelse(is.nan(Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest))))
Precisionglm <- Precisionglm + ifelse(is.nan(Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
Recallglm <- Recallglm + ifelse(is.nan(Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
F1glm <- F1glm + ifelse(is.nan(F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
ROCtest <- roc(datatest$ytest, glmtest)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCglm <- AUCglm + AUC(glmtest, datatest$ytest)
MSEglm <- MSEglm + MSE(glmtest, datatest$ytest)

##Discriminant Models
##Linear Discriminant
model.lda <- lda(ytrain~ winsTeam + nrtgTeamMisc + marginVictoryTeam +
                   Ranking + drtgTeamMisc + pctFG2PerGameTeam + pctFGPerGameTeam +
                   pctEFGTeamOppMisc + pctFGPerGameOpponent + pctEFGTeamMisc +
                   ortgTeamMisc + blkPerGameOpponent + pctFG2PerGameOpponent,
                 data = datatrain)
model.lda
ldatest <- predict(model.lda, datatest)
ConfusMatlda[[3]] <- ConfusionMatrix(ldatest$class, datatest$ytest)
ConfusMatlda[[3]]
Accuracylda <- Accuracylda + ifelse(is.nan(Accuracy(ldatest$class, datatest$ytest)),0,Accuracy(ldatest$class, datatest$ytest))
Precisionlda <- Precisionlda + ifelse(is.nan(Precision(datatest$ytest, ldatest$class)),0,Precision(datatest$ytest, ldatest$class))
Recalllda <- Recalllda + ifelse(is.nan(Recall(datatest$ytest, ldatest$class)),0,Recall(datatest$ytest, ldatest$class))
F1lda <- F1lda + ifelse(is.nan(F1_Score(datatest$ytest, ldatest$class)),0,F1_Score(datatest$ytest, ldatest$class))
ROCtest <- roc(datatest$ytest, ldatest$posterior[,1])
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUClda <- AUClda + AUC(ldatest$class, datatest$ytest)
MSElda <- MSElda + MSE(as.numeric(ldatest$class), datatest$ytest)

##K Nearest Neighbours Model
model.knn <- knn3(formula = as.factor(ytrain)~ nrtgTeamMisc + marginVictoryTeam +
                    winsTeam + Ranking + drtgTeamMisc +
                    pctFG2PerGameTeam + pctEFGTeamMisc +
                    pctFGPerGameTeam + pctFG3PerGameOpponent +
                    pctEFGTeamOppMisc + blkPerGameOpponent +
                    pctFGPerGameOpponent + ortgTeamMisc +
                    astPerGameTeam + pctTrueShootingeTeamMisc,
                  data = datatrain, k = knnval)
knntest <- predict(model.knn, datatest, type = "class")
ConfusMatknn[[3]] <- ConfusionMatrix(knntest, datatest$ytest)
ConfusMatknn[[3]]
Accuracyknn <- Accuracyknn + ifelse(is.nan(Accuracy(knntest, datatest$ytest)),0,Accuracy(knntest, datatest$ytest))
Precisionknn <- Precisionknn + ifelse(is.nan(Precision(datatest$ytest, knntest)),0,Precision(datatest$ytest, knntest))
Recallknn <- Recallknn + ifelse(is.nan(Recall(datatest$ytest, knntest)),0,Recall(datatest$ytest, knntest))
F1knn <- F1knn + ifelse(is.nan(F1_Score(datatest$ytest, knntest)),0,F1_Score(datatest$ytest, knntest))
ROCtest <- roc(datatest$ytest, as.numeric(knntest)-1)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCknn <- AUCknn + AUC(knntest, datatest$ytest)
MSEknn <- MSEknn + MSE(as.numeric(knntest)-1, datatest$ytest)

##4th fold = validation set
ytrain <- ceiling(rbind(cbind(PerGameFold1[,3]),cbind(PerGameFold2[,3]),cbind(PerGameFold3[,3]))/5)
xtrain <- rbind(PerGameFold1[,-3],PerGameFold2[,-3],PerGameFold3[,-3])
datatrain <- cbind(ytrain, xtrain)
ytest <- ceiling(PerGameFold4[,3]/5)
xtest <- cbind(PerGameFold4[,-3])
datatest <- cbind(ytest, xtest)

##Logistic Regression
model.glm <- glm(ytrain~pctTOVOpponentMisc + pctDRBOpponentMisc +
                   paceTeamMisc + fg3mPerGameTeam + trbPerGameOpponent,
                 data = datatrain, family = binomial)
glmtest <- predict(model.glm, datatest, type = "response")
for (i in 1:length(glmtest)) {
  if(glmtest[i] <= 0.5){
    glmtest[i] <- as.factor(0)
  }
  if(glmtest[i] > 0.5){
    glmtest[i] <- as.factor(1)
  }
}
ConfusMatglm[[4]] <- ConfusionMatrix(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))
ConfusMatglm[[4]]
Accuracyglm <- Accuracyglm + ifelse(is.nan(Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Accuracy(factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)), factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest))))
Precisionglm <- Precisionglm + ifelse(is.nan(Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Precision(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
Recallglm <- Recallglm + ifelse(is.nan(Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,Recall(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
F1glm <- F1glm + ifelse(is.nan(F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest)))),0,F1_Score(factor(datatest$ytest, levels=min(datatest$ytest):max(datatest$ytest)), factor(glmtest, levels=min(datatest$ytest):max(datatest$ytest))))
ROCtest <- roc(datatest$ytest, glmtest)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCglm <- AUCglm + AUC(glmtest, datatest$ytest)
MSEglm <- MSEglm + MSE(glmtest, datatest$ytest)

##Discriminant Models
##Linear Discriminant
model.lda <- lda(ytrain~ winsTeam + nrtgTeamMisc + marginVictoryTeam +
                   Ranking + drtgTeamMisc + pctFG2PerGameTeam + pctFGPerGameTeam +
                   pctEFGTeamOppMisc + pctFGPerGameOpponent + pctEFGTeamMisc +
                   ortgTeamMisc + blkPerGameOpponent + pctFG2PerGameOpponent,
                 data = datatrain)
model.lda
ldatest <- predict(model.lda, datatest)
ConfusMatlda[[4]] <- ConfusionMatrix(ldatest$class, datatest$ytest)
ConfusMatlda[[4]]
Accuracylda <- Accuracylda + ifelse(is.nan(Accuracy(ldatest$class, datatest$ytest)),0,Accuracy(ldatest$class, datatest$ytest))
Precisionlda <- Precisionlda + ifelse(is.nan(Precision(datatest$ytest, ldatest$class)),0,Precision(datatest$ytest, ldatest$class))
Recalllda <- Recalllda + ifelse(is.nan(Recall(datatest$ytest, ldatest$class)),0,Recall(datatest$ytest, ldatest$class))
F1lda <- F1lda + ifelse(is.nan(F1_Score(datatest$ytest, ldatest$class)),0,F1_Score(datatest$ytest, ldatest$class))
ROCtest <- roc(datatest$ytest, ldatest$posterior[,1])
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUClda <- AUClda + AUC(ldatest$class, datatest$ytest)
MSElda <- MSElda + MSE(as.numeric(ldatest$class), datatest$ytest)

##K Nearest Neighbours Model
model.knn <- knn3(formula = as.factor(ytrain)~ nrtgTeamMisc + marginVictoryTeam +
                    winsTeam + Ranking + drtgTeamMisc +
                    pctFG2PerGameTeam + pctEFGTeamMisc +
                    pctFGPerGameTeam + pctFG3PerGameOpponent +
                    pctEFGTeamOppMisc + blkPerGameOpponent +
                    pctFGPerGameOpponent + ortgTeamMisc +
                    astPerGameTeam + pctTrueShootingeTeamMisc,
                  data = datatrain, k = knnval)
knntest <- predict(model.knn, datatest, type = "class")
ConfusMatknn[[4]] <- ConfusionMatrix(knntest, datatest$ytest)
ConfusMatknn[[4]]
Accuracyknn <- Accuracyknn + ifelse(is.nan(Accuracy(knntest, datatest$ytest)),0,Accuracy(knntest, datatest$ytest))
Precisionknn <- Precisionknn + ifelse(is.nan(Precision(datatest$ytest, knntest)),0,Precision(datatest$ytest, knntest))
Recallknn <- Recallknn + ifelse(is.nan(Recall(datatest$ytest, knntest)),0,Recall(datatest$ytest, knntest))
F1knn <- F1knn + ifelse(is.nan(F1_Score(datatest$ytest, knntest)),0,F1_Score(datatest$ytest, knntest))
ROCtest <- roc(datatest$ytest, as.numeric(knntest)-1)
ggroc(ROCtest, colour = "blue", linetype = 1, size =2) +
  ggtitle("ROCtest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), colour = "black", linetype = 2) +
  theme_gray()
AUCknn <- AUCknn + AUC(knntest, datatest$ytest)
MSEknn <- MSEknn + MSE(as.numeric(knntest)-1, datatest$ytest)

##Let us take a look at our metrics for each model
##Logistic Regression
MSEglm/4
Accuracyglm/4
Precisionglm/4
Recallglm/4
F1glm/4
AUCglm/4
ConfusMatglm

##Linear Discriminant
MSElda/4
Accuracylda/4
Precisionlda/4
Recalllda/4
F1lda/4
AUClda/4
ConfusMatlda

##K Nearest Neighbours
MSEknn/4
Accuracyknn/4
Precisionknn/4
Recallknn/4
F1knn/4
AUCknn/4
ConfusMatknn

##2020 Season Predictions
ytrain <- ceiling(MasterPerGame$finish/5)
xtrain <- MasterPerGame[,-3]
datatrain <- cbind(ytrain, xtrain)
xtest <- MasterPerGame2020

##Logistic Regression
model.glm <- glm(ytrain~Ranking + tovPerGameOpponent + blkPerGameOpponent +
                   fg3mPerGameTeam + ptsPerGameOpponent +
                   ftmPerGameOpponent + pctTOVOpponentMisc,
                 data = datatrain, family = binomial)
glmtest <- predict(model.glm, xtest, type = "response")
for (i in 1:length(glmtest)) {
  if(glmtest[i] <= 0.5){
    glmtest[i] <- 0
  }
  if(glmtest[i] > 0.5){
    glmtest[i] <- 1
  }
}
glmtest
for (i in 1:length(glmtest)) {
  if(glmtest[i] == 0){
    print(as.character(NBASalaryAnalysisData2020$Team[i]))
  }
}

##Logistic Regression Model suggests that the Los Angeles Lakers are a
##Championship Level team

##Discriminant Analysis
##Linear Discriminant
model.lda <- lda(ytrain~ winsTeam + nrtgTeamMisc + marginVictoryTeam +
                   Ranking + drtgTeamMisc + pctFG2PerGameTeam + pctFGPerGameTeam +
                   pctEFGTeamOppMisc + pctFGPerGameOpponent + pctEFGTeamMisc +
                   ortgTeamMisc + blkPerGameOpponent + pctFG2PerGameOpponent,
                 data = datatrain)
model.lda
ldatest <- predict(model.lda, xtest)
ldatest$class
for (i in 1:length(ldatest$class)) {
  if(ldatest$class[i] == 0){
    print(as.character(NBASalaryAnalysisData2020$Team[i]))
  }
}

##Thus we can see that our LDA model cannot predict a champion for this year

##K Nearest Neighbours
model.knn <- knn3(formula = as.factor(ytrain)~ nrtgTeamMisc + marginVictoryTeam +
                    winsTeam + Ranking + drtgTeamMisc +
                    pctFG2PerGameTeam + pctEFGTeamMisc +
                    pctFGPerGameTeam + pctFG3PerGameOpponent +
                    pctEFGTeamOppMisc + blkPerGameOpponent +
                    pctFGPerGameOpponent + ortgTeamMisc +
                    astPerGameTeam + pctTrueShootingeTeamMisc,
                  data = datatrain, k = knnval)
knntest <- predict(model.knn, xtest, type = "class")
knntest
for (i in 1:length(knntest)) {
  if(knntest[i] == 0){
    print(as.character(NBASalaryAnalysisData2020$Team[i]))
  }
}

##Likewise, we see that we cannot predict a champion for this year
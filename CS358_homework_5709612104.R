library(tidyverse)
library(ggplot2)
library(readr)  
library( RWeka )
library ( ggplot2 ) 
library(e1071)
library( rpart )
library( caret )
library( pROC )
mushroom <- read_csv("mushrooms.csv") 
mushroom
set.seed( 4 )

partitionData <- function( data, fractionOfDataForTrainingData = 0.6 )
{
  numberOfRows <- nrow( data )
  randomRows   <- runif( numberOfRows )
  flag         <- randomRows <= fractionOfDataForTrainingData
  trainingData <- data[ flag, ]
  testingData  <- data[ !flag, ]
  dataSetSplit <- list( trainingData = trainingData, testingData = testingData )
  dataSetSplit
}
paritionedData <- partitionData( mushroom )
trainingData   <- paritionedData$trainingData
testingData    <- paritionedData$testingData

gainRatioResult <- GainRatioAttributeEval( class ~ . , data = mushroom )
print( sort( gainRatioResult, decreasing = TRUE ))


hist <- ggplot( mushroom, aes( x = odor, fill = class )) 
hist <- hist + geom_bar(stat='count', position='dodge') + labs(x = 'Odor', y = 'Count of Class')
hist

schema        <- class ~ odor
actualResults <- ifelse( testingData$class == "p", TRUE, FALSE )

getConfusionMatrix <- function( probabilityOfTests, actuals = actualResults, threshold = 0.4 )
{
  predictions    <- ifelse ( probabilityOfTests > threshold, 'p', 'e' )
  confusionMatrix( testingData$class, predictions )
}

plotROCCurve <- function( predictionResults, title, color = "red" )
{
  plot( roc( testingData$class, predictionResults, direction="<" ), 
        print.auc=TRUE, col = color, lwd = 3, main = title )
}

dt.Model      <- rpart( schema, data = trainingData )
dt.Prediction <- predict( dt.Model, newdata = testingData )

print( getConfusionMatrix( dt.Prediction[, 2] ))
plotROCCurve( dt.Prediction[, 2], "ROC Curve for Decision Tree")
plot(dt.Model)
text(dt.Model,use.n= TRUE,all =TRUE,cex =2)

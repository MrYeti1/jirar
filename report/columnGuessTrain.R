#http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/
# for installing slam (dependancy of tm) on R3.3 and mac
library(tm)
library(caret)

statusColours <- c("secondsInColumns.Open" = "white", "secondsInColumns.Backlog" = "white",
                   "secondsInColumns.Selected.for.Development" = "white",
                   "secondsInColumns.Analysis.In" = "red", "secondsInColumns.Analysis.Out" = "white",
                   "secondsInColumns.Selected" = "red",
                   "secondsInColumns.Selected.for.Release" = "red",
                   "secondsInColumns.Release.Blocked" = "black",
                   "secondsInColumns.Next" = "red",
                   
                   "secondsInColumns.Test.Analysis.In" = "red", "secondsInColumns.Test.Analysis.Out" = "white",
                   "secondsInColumns.Test.Analysis.Done" = "white",
                   "secondsInColumns.Elaboration.In" = "red", "secondsInColumns.Elaboration.Out" = "white", "secondsInColumns.Elaboration..Out" = "white", "secondsInColumns.Elaboration.Done" = "white",
                   "secondsInColumns.Elab.In" = "red", "secondsInColumns.Elab.Out" = "white",
                   "secondsInColumns.Tech.Elab.In" = "red", "secondsInColumns.Tech.Elab.Out" = "white",
                   
                   "secondsInColumns.3.Amigos.In" = "red", "secondsInColumns.3.Amigos.Out" = "white",
                   "secondsInColumns.Ready.for.Dev" = "white",
                   "secondsInColumns.Implementation.In" = "orange", "secondsInColumns.Prioritised" = "orange",
                   "secondsInColumns.Imp.In" = "orange",
                   "secondsInColumns.Build.In" = "orange",
                   
                   "secondsInColumns.Implementation.Out" = "white", 
                   "secondsInColumns.Imp.Out" = "white",
                   "secondsInColumns.Build.Done" = "white",
                   "secondsInColumns.Ready.For.Code.Review" = "white",
                   
                   "secondsInColumns.Implementation.Done" = "white","secondsInColumns.Demo.d" = "white",
                   "secondsInColumns.In.Progress" = "orange",
                   
                   "secondsInColumns.Review.In" = "orange", "secondsInColumns.Review.Out" = "white",
                   "secondsInColumns.In.Code.Review" = "orange",
                   "secondsInColumns.Code.Review.In" = "orange", "secondsInColumns.Code.Review.Out" = "white",
                   "secondsInColumns.Tech.Review.In" = "orange", "secondsInColumns.Tech.Review.Done" = "white",
                   "secondsInColumns.Code.Review.Done" = "white",
                   "secondsInColumns.Awaiting.Review" = "orange", "secondsInColumns.Review.Done" = "white",                           "secondsInColumns.Raise.Change" = "orange", "secondsInColumns.Change.Wait" = "white",
                   "secondsInColumns.Ready.for.Test" = "white",
                   
                   "secondsInColumns.Code.Review...Demo.In" = "orange", "secondsInColumns.Code.Review...Demo.Out" = "white",
                   "secondsInColumns.In.Review" = "orange",
                   "secondsInColumns.Demo.In" = "orange", "secondsInColumns.Demo.Out" = "white",
                   "secondsInColumns.UAT.In" = "orange", "secondsInColumns.UAT.Out" = "white",
                   "secondsInColumns.UAT.Test.In" = "orange", "secondsInColumns.UAT.Test.Done" = "white",
                   "secondsInColumns.BR.In" = "orange", "secondsInColumns.BR.Done" = "white",
                   
                   "secondsInColumns.In.Test" = "yellow",
                   "secondsInColumns.Test.In" = "yellow", "secondsInColumns.Test.Out" = "white",
                   "secondsInColumns.Testing.In" = "yellow", "secondsInColumns.Testing.Done" = "white",
                   "secondsInColumns.Test.Done" = "white",
                   "secondsInColumns.Functional.Test.In" = "yellow",
                   "secondsInColumns.Functional.Test.Done" = "white",
                   "secondsInColumns.Ready.For.Live" = "white",
                   
                   "secondsInColumns.Merged" = "green",
                   "secondsInColumns.Merge.In" = "green",
                   "secondsInColumns.Merge.Done" = "white",
                   
                   
                   "secondsInColumns.Deploy.To.Test.In" = "green",
                   "secondsInColumns.Functional.Test.Deployment.In" = "green",
                   "secondsInColumns.Deploy" = "green",
                   "secondsInColumns.Deploy.to.Test.Out" = "white",
                   "secondsInColumns.Functional.Test.Deployment.Done" = "white",
                   
                   "secondsInColumns.Deployed.to.Test.Environment" = "green",
                   "secondsInColumns.Deployed.to.Staging.Environment" = "green",
                   "secondsInColumns.Deployed.to.Staging.Environmen" = "green",
                   "secondsInColumns.Regression.Test.Deployment.In" = "green",
                   "secondsInColumns.UAT.Test.Deployment.In" = "green",
                   "secondsInColumns.Staging" = "green",
                   
                   "secondsInColumns.Regression.Test.Deployment.Done" = "white",
                   "secondsInColumns.UAT.Test.Deployment.Done" = "white",
                   "secondsInColumns.Regression.In" = "green",
                   "secondsInColumns.Regression.Done" = "white",
                   
                   
                   "secondsInColumns.Ready.For.Release" = "green",
                   "secondsInColumns.Release" = "green",
                   "secondsInColumns.Change.Imp" = "green",
                   "secondsInColumns.Release.Validation" = "green",
                   "secondsInColumns.In.Live" = "green",
                   
                   "secondsInColumns.Closed" = "black",
                   "secondsInColumns.Resolved" = "black",
                   "secondsInColumns.Done" = "black"
                   
)
possibleColours <- levels(as.factor(statusColours))

#Error if one of the colour names is not a valid R variable name
stopifnot(all(make.names(possibleColours) == possibleColours))

trainRatio <- 0.65
trainSize <- floor(length(statusColours) * trainRatio)
set.seed(1323)
trainInd <- sample(seq_len(length(statusColours)), trainSize)


statusColours.train <- statusColours[trainInd]
statusColours.test <- statusColours[-trainInd]

statusColours.train <- data.frame(colname = names(statusColours.train), colour=statusColours.train, row.names=NULL) %>% mutate(colname = as.character(colname)) %>% mutate(colwords = strsplit(colname, split="[.]"))
statusColours.test <- data.frame(colname = names(statusColours.test), colour=statusColours.test, row.names=NULL) %>% mutate(colname = as.character(colname)) %>% mutate(colwords = strsplit(colname, split="[.]"))

#statusColours.test <- data.frame(colname = names(statusColours.test), colour=factor(statusColours.test, levels=possibleColours), row.names=NULL) %>% mutate(colname = as.character(colname)) %>% mutate(colwords = strsplit(colname, split="[.]"))

####

corpus <- tm::VCorpus(tm::VectorSource(statusColours.train$colwords))
tdm <- tm::DocumentTermMatrix(corpus, control=list(stemming = T, stopwords=FALSE, wordLengths=c(2,Inf)))
(termFrequency <- as.matrix(tdm) %>% margin.table(2) )
statusTDM.train <- as.matrix(tdm)
#statusTDM.train <- cbind(statusTDM.train, factor(statusColours.train$colour, levels=possibleColours))
#colnames(statusTDM.train)[ncol(statusTDM.train)] <- 'colour'
statusTDM.train <- as.data.frame(statusTDM.train)
statusTDM.train$colour <- factor(statusColours.train$colour, levels=possibleColours)
#install.packages(c("tm", "caret", "arm", "e1071"))

ctrl <- caret::trainControl(method = "repeatedcv", repeats = 10,
                     classProbs = F
                     ) #, summaryFunction = multiClassSummary)

colColourFit <- caret::train(colour ~ ., data = statusTDM.train, 
                  method = "svmLinear",
                  #metric = "ROC",
                  preProc = c("zv"),# "center", "scale"),
                  trControl = ctrl)

#Write colColourFit
#TODO Check this writes to the correct folder when run (the report folder)
save(colColourFit, file="colColourFit.Rdata")
colColourTdm <- tdm
save(colColourTdm, file="colColourTdm.Rdata")


statusTDM.pred <- predict(colColourFit, newdata = statusTDM.train, type="raw")
#factor(possibleColours[round(abs(predict(colColourFit, newdata = statusTDM.train[, !(colnames(statusTDM.train) == "colour")])))], levels=possibleColours)
  
caret::confusionMatrix(statusTDM.pred, statusColours.train$colour)
resultTable <- table(
  orig=statusColours.train$colour,
  guess=statusTDM.pred
)
resultTable

propTable <- prop.table(resultTable, margin=1)
#Diagonal = probability that the guess and original are the same in both. Measure the mean of each original.
percCorrect <- ((diag(nrow(resultTable)) * propTable) %>% margin.table(1) %>% mean() * 100 ) %>% round()
ggplot(as.data.frame(propTable), aes(x=orig, y=guess, fill=Freq)) + geom_tile() + viridis::scale_fill_viridis() + coord_equal() + ggtitle(paste0(percCorrect, "% of guessed colours equal original"), subtitle = "Yellow diagonal line suggests all guesses are correct")


## TEST DATA

corpus.test <- tm::VCorpus(tm::VectorSource(statusColours.test$colwords))
tdm.test <- tm::DocumentTermMatrix(corpus.test, control=list(dictionary=tm::Terms(tdm), stemming = T, stopwords=FALSE, wordLengths=c(2,Inf)))
tdm.test %>% as.matrix()

statusTDM.test <- as.data.frame(as.matrix(tdm.test))
statusTDM.test$colour <- factor(statusColours.test$colour, levels=possibleColours)
statusTDM.pred.test <- factor(predict(colColourFit, newdata=as.matrix(tdm.test)), levels=possibleColours)

caret::confusionMatrix(statusTDM.pred.test, statusTDM.test$colour)
resultTable <- table(
  orig=statusTDM.test$colour,
  guess=statusTDM.pred.test
)
resultTable
propTable <- prop.table(resultTable, margin=1)
#Diagonal = probability that the guess and original are the same in both. Measure the mean of each original.
rowCorrectTable <- (diag(nrow(resultTable)) * propTable) %>% margin.table(1)
rowCorrectTable[rowCorrectTable %>% is.na] <- 1
percCorrect <- (rowCorrectTable %>% mean() * 100 ) %>% round()
ggplot(as.data.frame(propTable), aes(x=orig, y=guess, fill=Freq)) + geom_tile() + viridis::scale_fill_viridis() + coord_equal() + ggtitle(paste0(percCorrect, "% of guessed colours equal original"), subtitle = "Yellow diagonal line suggests all guesses are correct")


########
## Use columnGuess.R function to use the saved models



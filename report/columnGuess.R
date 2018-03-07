#http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/
# for installing slam (dependancy of tm) on R3.3 and mac
library(tm)
library(caret)
library(dplyr)

########
## Using
########
#actualColumns <- paste0("secondsInColumns.", c('Open', 'Reopened', "Test.Analysis.In", '3 Amigos In', '3 Amigos Out', 'Implementation In', 'Implementation Out', 'Review In', 'Review Out', 'Test In', 'Test Out', 'Merged', 'Deployed to Test Environment', 'Deployed to Staging Environment', 'Release Validation', 'Resolved', 'Closed')) %>% make.names()
#actualColumns <- summaryResolvedMelt$variable %>% unique()
#guessedColours <- guessColumnColours(actualColumns, colColourFit, colColourTdm)

guessColumnColours <- function(columnNames, colColourFit, colColourTdm) {
  actualInput <- data.frame(colname = columnNames) %>% mutate(colname = as.character(colname)) %>% mutate(colwords = strsplit(colname, split="[.]"))
  
  actualCorpus <- tm::VCorpus(tm::VectorSource(actualInput$colwords))
  actualTDM <- tm::DocumentTermMatrix(actualCorpus, control=list(dictionary=tm::Terms(colColourTdm), stemming = T, stopwords=FALSE, wordLengths=c(2,Inf)))
  actualPredictions <- predict(colColourFit, newdata=as.matrix(actualTDM))
  
  
  actualMap <- data.frame(colname = columnNames, colour = actualPredictions)
  
  brewMap <- c("red" = "Reds", "green" = "Greens", "yellow" = "YlOrRd", "orange" = "Oranges", "white"= "white", "black"="Greys")
  brewIgnore <- c("white")
  brewFlip <- c("Greens", "Oranges", "Reds", "Greys")
  
  
  a <- actualMap %>%
      mutate(brewColour = brewMap[as.character(colour)]) %>% 
      group_by(colour, brewColour) %>% 
      summarise(n=n()) %>% 
      na.omit %>% 
      mutate(colours = 
         ifelse(brewColour %in% brewIgnore, 
            list(rep(brewColour, n)), 
            ifelse(brewColour %in% brewFlip, 
               list(rev(RColorBrewer::brewer.pal(n, brewColour)) %>% head(n)), 
               list(RColorBrewer::brewer.pal(n, brewColour) %>% head(n))
            )
                
         )
      )
  
  b <- actualMap %>% 
    group_by(colour) %>% 
    mutate(
      rn = row_number(colour),
      newColour = a[a$colour == colour,"colours"]$colours[[1]][rn]
    )
  
  #Preview colour scale - TODO pull this into an extra function
  #ggplot(b, aes(x=forcats::fct_rev(forcats::fct_inorder(colname)), y=5, fill=newColour, label=colour)) + geom_col() + scale_fill_identity() + coord_flip() + geom_text(aes(y=2.5)) + xlab("Column Name") + theme_minimal() + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  
  c <- b$newColour
  names(c) <- b$colname
  return(c)
}


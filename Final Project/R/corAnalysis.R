#------------------------------------------------------------------------------#
#                                 corAnalysis                                  #
#------------------------------------------------------------------------------#
#' corAnalysis
#'
#' \code{corAnalysis} Compares correlation between predictors with the 
#' correlation between predictor and response.
#'
#' @param x Dataframe containing data for preprocessing
#' @param target Character string containing the name of the target variable
#' @param n Numeric indicating the maximum number of predictors to return
#'
#' @return Dataframe with correlation comparisons
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
corAnalysis <- function(x, target, n = NULL) {
  
  # Extract predictors
  vars <- colnames(x)
  vars <- vars[vars != target]
  
  # Obtain relative importance
  pri <- importance(x, target)
  
  # Obtain correlations
  corData <- analyzR::correlation(x)
  assocData <- analyzR::association(x)
  corTbl <- corData$table
  assocTbl <- assocData$table
  ca <- rbind(corTbl, assocTbl)
  ca <- data.frame(x = ca$Variable.1, y = ca$Variable.2, r = ca$r, stringsAsFactors = FALSE)
  
  # Merge in relative importance
  analysis <- merge(ca, pri, by.x = 'x', by.y = 'term')
  analysis <- merge(analysis, pri, by.x = 'y', by.y = 'term')
  analysis <- analysis %>% mutate(best = ifelse(aR2.x > aR2.y, x, y))
  analysis <- analysis %>% mutate(worst = ifelse(aR2.x < aR2.y, x, y))
  analysis <- analysis %>% mutate(bestR2 = ifelse(aR2.x > aR2.y,  aR2.x, aR2.y))
  analysis <- analysis %>% mutate(info = bestR2 - abs(r)) %>% arrange(-info)
  
  return(analysis)
}

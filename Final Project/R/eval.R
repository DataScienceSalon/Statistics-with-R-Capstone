#------------------------------------------------------------------------------#
#                                  eval                                        #
#------------------------------------------------------------------------------#
#' eval
#'
#' \code{corAnalysis} Compares correlation between predictors with the 
#' correlation between predictor and response.
#'
#' @param train Dataframe containing training data
#' @param test Dataframe containing test data
#' @param mainAR2 Minimum adjusted R2 for main effects
#' @param interAR2 Minimum adjusted R2 for interaction effects
#' @param target Character string containing the name of the target variable
#'
#' @return Dataframe with correlation comparisons
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
eval <- function(train, test, mainAR2, interAR2, target) {

  # Obtain most important terms
  pri <- importance(train, target)
  topTerms <- pri %>% filter(aR2 > mainAR2) %>% arrange(-aR2) %>% dplyr::select(term)
  
  # Optain interactions
  effects <- interactions(train, target)
  topInteractions <- effects %>% filter(aR2 > interAR2 & pValue < 0.05) %>% arrange(-aR2)
  inter <- paste(sapply(seq(1:nrow(topInteractions)), function(j) {
    paste(topInteractions$Var1[j], topInteractions$Var2[j], sep = "*")
  }), collapse = "+")
  
  # Create formulas
  f <- list()
  if (nrow(topTerms) > 0) {
    for (i in 1:nrow(topTerms)) {
      terms <- paste(topTerms$term[1:i], collapse = "+")
      if (nrow(topInteractions) > 0) {
        for (j in 1:nrow(topInteractions)) {
          inter <- paste(sapply(seq(1:j), function(j) {
            paste(topInteractions$Var1[j], topInteractions$Var2[j], sep = "*")
          }), collapse = "+")
          combined <- paste(terms, inter, sep = "+")
          fmla <- as.formula(paste(target, combined, sep = "~"))  
          f <- c(f, fmla)
        } 
      } else {
        fmla <- as.formula(paste(target, terms, sep = "~"))  
        f <- c(f, fmla)
      }
    }
  } else {
      if (nrow(topInteractions) > 0) {
        for (j in 1:nrow(topInteractions)) {
          inter <- paste(sapply(seq(1:j), function(j) {
            paste(topInteractions$Var1[j], topInteractions$Var2[j], sep = "*")
          }), collapse = "+")
          fmla <- as.formula(paste(target, inter, sep = "~"))  
          f <- c(f, fmla)
        } 
      } else {
        return()
      }
  }
  
  # Evaluate models in step
  yTrain <- exp(train$price.log)
  yTest <- exp(test$price.log)
  eval_lm <- lapply(seq_along(f), function(i) {
    e <- list()
    e$n <- i
    e$model <- paste("lm Model #", i)
    e$formula <- f[[i]]
    
    e$m <- lm(e$formula, data = train)
    e$terms <- length(attr(terms(e$m), "term.labels"))
    
    yHat <- predict(e$m, data = train)
    yHat <- exp(yHat)
    e$RMSE_train <- sqrt(mean((yTrain - yHat)^2)) 
    
    yHat <- predict(e$m, newdata = test)
    yHat <- exp(yHat)
    e$RMSE_test <- sqrt(mean((yTest - yHat)^2)) 
    e
  })
  
  eval_step <- lapply(seq_along(f), function(i) {
    
    e <- list()
    e$n <- i
    e$model <- paste("Step Model #", i)
    e$formula <- f[[i]]
    m <- lm(e$formula, data = train)
    e$m <- stepAIC(m, direction = 'both', trace = FALSE)
    e$terms <- length(attr(terms(e$m), "term.labels"))
    
    yHat <- predict(e$m, data = train)
    yHat <- exp(yHat)
    e$RMSE_train <- sqrt(mean((yTrain - yHat)^2)) 
    
    yHat <- predict(e$m, newdata = test)
    yHat <- exp(yHat)
    e$RMSE_test <- sqrt(mean((yTest - yHat)^2)) 
    
    e
  })
  
  evalData <- c(eval_lm, eval_step)
  evalData <- lapply(seq_along(evalData), function(i) {
    evalData[[i]]$idx <- i
    evalData[[i]]
  })
  
  eval_tbl <- rbindlist(lapply(seq_along(evalData), function(i) {
    tbl <- list()
    tbl$idx <- i
    tbl$model <- evalData[[i]]$model
    tbl$terms <- evalData[[i]]$terms
    tbl$RMSE_train <- evalData[[i]]$RMSE_train
    tbl$RMSE_test <- evalData[[i]]$RMSE_test
    tbl
  }))
  
  r <- list()
  r$data <- evalData
  r$table <- eval_tbl

  return(r)
}
#==============================================================================#
#                                BMA Evaluate                                  #
#==============================================================================#
#' bmaEvaluate
#'
#' \code{bmaEvaluate} Performs predictions on each model and evaluates MSE.
#'
#' @param models List of candidate BAS.lm models
#' @return List containing the top 10 models by MSE as well as a matrix
#' of 'BMA', 'HPM', 'MPM', and 'BPM' estimators for each prior.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bmaEvaluate <- function(models) {
  
  estimators <- c("BMA", "HPM", "MPM", "BPM")
  
  eval <- rbindlist(lapply(models, function(m) {
    report <- rbindlist(lapply(estimators, function(e) {
      pred <- predict(object = m, estimator = e)
      yHat <- pred$fit
      df <- data.frame(Prior = m$prior,
                       Desc = m$priorDesc,
                       Estimator = e,
                       MSE = mean((m$Y - yHat)^2),
                       Intercept = ("Intercept" %in% pred$best.vars),
                       Lot.Area = ("Lot.Area" %in% pred$best.vars),
                       Land.SlopeMod = ("Land.SlopeMod" %in% pred$best.vars),
                       Land.SlopeSev = ("Land.SlopeSev" %in% pred$best.vars),
                       Year.Built = ("Year.Built" %in% pred$best.vars),
                       Year.Remod.Add = ("Year.Remod.Add" %in% pred$best.vars),
                       Bedroom.AbvGr = ("Bedroom.AbvGr" %in% pred$best.vars))
      df
    }))
  }))
  result <- list()
  result$top10 <- (eval %>% dplyr::select(Desc, Estimator, MSE) %>% arrange(MSE))[1:10,]
  result$performance <- dcast(eval, Desc ~ Estimator, value.var = "MSE")
  result$full <- eval
  return(result)
}
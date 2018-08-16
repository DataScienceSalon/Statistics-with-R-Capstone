#------------------------------------------------------------------------------#
#                                 importance                                   #
#------------------------------------------------------------------------------#
#' importance
#'
#' \code{importance} Computes predictor relative importance by adjusted R2
#'
#' @param x Dataframe containing data for preprocessing
#' @param target Character string containing the name of the target variable
#'
#' @return Dataframe with predictor proportion of variance explained
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
importance <- function(x, target) {
  
  # Extract predictors
  vars <- colnames(x)
  vars <- vars[vars != target]
  
  # Obtain predictor relative importance 
  pri <- rbindlist(lapply(vars, function(v) {
    fmla <- as.formula(paste(target, v, sep = "~"))
    m <- lm(fmla, data = x)
    mAov <- anova(m)
    e <- list()
    e$term <- v
    e$aR2 <- glance(m)$adj.r.squared
    e
  }))
  
  return(pri)
}

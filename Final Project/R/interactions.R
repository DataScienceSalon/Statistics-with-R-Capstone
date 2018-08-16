#------------------------------------------------------------------------------#
#                                 interactions                                 #
#------------------------------------------------------------------------------#
#' interactions
#'
#' \code{interactions} Tests pairwise interaction effects
#'
#' @param x Dataframe containing data for preprocessing
#' @param target Character string containing the name of the target variable
#'
#' @return Dataframe with correlation comparisons
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
interactions <- function(x, target) {
  
  # Extract predictors
  vars <- colnames(x)
  vars <- vars[vars != target]
  
  pairs <- expand.grid(list(vars, vars),KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  pairs <- pairs %>% filter(Var1 != Var2)
  
  effects <- as.data.frame(rbindlist(lapply(seq(1:nrow(pairs)), function(p) {
    fmla <- as.formula(paste(target, paste(paste(pairs[p,], collapse = "+"),  "+", paste(pairs[p,], collapse = "*")), sep = "~"))
    m <- lm(fmla, data = x)
    v <- anova(m)
    e <- list()
    e$Var1 <- pairs[p,1]
    e$Var2 <- pairs[p,2]
    e$aR2 <- glance(m)$adj.r.squared
    e$pValue <- v$`Pr(>F)`[3]
    e
  })))
  
  cols <- c('Var1','Var2')
  newdf <- effects[,cols]
  for (i in 1:nrow(effects)){
    newdf[i, ] <- sort(effects[i,cols])
  }
  
  effects <- effects[!duplicated(newdf),]
  
  significant <- effects %>% filter(pValue < .05 & aR2 > .75) %>% arrange(-aR2)
  
  return(effects)
}

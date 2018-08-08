#------------------------------------------------------------------------------#
#                                bivariate                                     #
#------------------------------------------------------------------------------#
#' bivariate
#'
#' \code{bivariate} Performas bivariate analysis
#' 
#' @param x Dataframe to be summarized
#' @param target Column name of target variable.
#'
#' @return A list containing summary dataframes and plots
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
bivariate <- function(x, target) {
  
  # Conducted qualitative bivariate analysis
  nums <- colnames(x)[sapply(x, is.numeric)]
  nums <- sort(nums)
  biQuant <- bivariateQuant(x, nums, target)
  
  # Conducted qualitative bivariate analysis
  cats <- colnames(x)[sapply(x, is.factor)]
  cats <- sort(cats)
  biQual <- bivariateQual(x, cats, target)
  
  analysis <- list()
  analysis$quant <- biQuant
  analysis$qual <- biQual
  
  return(analysis)
}
  
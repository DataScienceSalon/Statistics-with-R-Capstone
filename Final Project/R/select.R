#------------------------------------------------------------------------------#
#                                   select                                     #
#------------------------------------------------------------------------------#
#' select
#'
#' \code{select} Removes variables from data set that don't meet regression 
#' assumptions.
#'
#' @param data Dataframe containing data for preprocessing
#'
#' @return df Dataframe with processed data
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
select <- function(data) {
  
  df <- data
  
  # Remove select variables
  df$Lot.Frontage <- NULL
  df$Street <- NULL
  df$Alley <- NULL
  df$Land.Contour <- NULL
  df$Utilities <- NULL
  df$Land.Slope <- NULL
  df$Condition.1 <- NULL
  df$Condition.2 <- NULL
  df$Roof.Matl <- NULL
  df$Mas.Vnr.Area <- NULL
  df$Bsmt.Cond <- NULL
  df$BsmtFin.Type.2 <- NULL
  df$BsmtFin.SF.2 <- NULL
  df$Heating <- NULL
  df$Central.Air <- NULL
  df$Electrical <- NULL
  df$Low.Qual.Fin.SF <- NULL
  df$Functional <- NULL
  df$Paved.Drive <- NULL
  df$Enclosed.Porch <- NULL
  df$X3Ssn.Porch <- NULL
  df$Screen.Porch <- NULL
  df$Pool.Area <- NULL
  df$Pool.QC <- NULL
  df$Fence <- NULL
  df$Misc.Feature <- NULL
  df$Misc.Val <- NULL
  df$Sale.Type <- NULL
  df$Sale.Condition <- NULL
  
  
  save(df, file = file.path("../data/interim",paste0(fileName, ".Rdata")))
  write.csv(file = file.path("../data/interim",paste0(fileName, ".csv")), df)
  
  return(df)
}
  
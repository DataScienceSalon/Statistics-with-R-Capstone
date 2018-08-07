#------------------------------------------------------------------------------#
#                             convertOrdinals                                  #
#------------------------------------------------------------------------------#
#' convertOrdinals
#'
#' \code{convertOrdinals} Converts ordinal variables to numeric
#'
#' @param df Dataframe containing data for preprocessing
#'
#' @return df Dataframe with processed data
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
convertOrdinals <- function(df) {
  
  df <- ames_train
  
  df$Lot.Shape <- factor(df$Lot.Shape, levels = c("IR3", "IR2", "IR1", "Reg"))
  df$Lot.Shape <- as.numeric(df$Lot.Shape)
  
  df$Utilities <- factor(df$Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"))
  df$Utilities <- as.numeric(df$Utilities)
  
  df$Land.Slope <- factor(df$Land.Slope, levels = c("Sev", "Mod", "Gtl"))
  df$Land.Slope <- as.numeric(df$Land.Slope)
  
  df$Exter.Qual <- factor(df$Exter.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Exter.Qual <- as.numeric(df$Exter.Qual)

  df$Exter.Cond <- factor(df$Exter.Cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Exter.Cond <- as.numeric(df$Exter.Cond)
  
  df$Bsmt.Qual <- factor(df$Bsmt.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Bsmt.Qual <- as.numeric(df$Bsmt.Qual)
  df$Bsmt.Qual <- ifelse(is.na(df$Bsmt.Qual), 0, df$Bsmt.Qual)
  
  df$Bsmt.Cond <- factor(df$Bsmt.Cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Bsmt.Cond <- as.numeric(df$Bsmt.Cond)
  df$Bsmt.Cond <- ifelse(is.na(df$Bsmt.Cond), 0, df$Bsmt.Cond)
  
  df$Bsmt.Exposure <- factor(df$Bsmt.Exposure, levels = c("No", "Mn", "Av", "Gd"))
  df$Bsmt.Exposure <- as.numeric(df$Bsmt.Exposure)
  df$Bsmt.Exposure <- ifelse(is.na(df$Bsmt.Exposure), 0, df$Bsmt.Exposure)
  
  df$BsmtFin.Type.1 <- factor(df$BsmtFin.Type.1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
  df$BsmtFin.Type.1 <- as.numeric(df$BsmtFin.Type.1)
  df$BsmtFin.Type.1 <- ifelse(is.na(df$BsmtFin.Type.1), 0, df$BsmtFin.Type.1)
  
  df$BsmtFin.Type.2 <- factor(df$BsmtFin.Type.2, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
  df$BsmtFin.Type.2 <- as.numeric(df$BsmtFin.Type.2)
  df$BsmtFin.Type.2 <- ifelse(is.na(df$BsmtFin.Type.2), 0, df$BsmtFin.Type.2)
  
  df$Heating.QC <- factor(df$Heating.QC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Heating.QC <- as.numeric(df$Heating.QC)
  
  df$Electrical <- factor(df$Electrical, levels = c("Mixed", "FuseP", "FuseF", "FuseA", "SBrkr"))
  df$Electrical <- as.numeric(df$Electrical)
  
  df$Kitchen.Qual <- factor(df$Kitchen.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Kitchen.Qual <- as.numeric(df$Kitchen.Qual)
  
  df$Functional <- factor(df$Functional, levels = c("Sal", "Sev", "Maj2",
                                                    "Maj1", "Mod", "Min2",
                                                    "Min1", "Typ"))
  df$Functional <- as.numeric(df$Functional)
  
  df$Fireplace.Qu <- factor(df$Fireplace.Qu, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Fireplace.Qu <- as.numeric(df$Fireplace.Qu)
  df$Fireplace.Qu <- ifelse(is.na(df$Fireplace.Qu), 0, df$Fireplace.Qu)
  
  df$Garage.Finish <- factor(df$Garage.Finish, levels = c("Unf", "Rfn", "Fin"))
  df$Garage.Finish <- as.numeric(df$Garage.Finish)
  df$Garage.Finish <- ifelse(is.na(df$Garage.Finish), 0, df$Garage.Finish)
  
  df$Garage.Qual <- factor(df$Garage.Qual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Garage.Qual <- as.numeric(df$Garage.Qual)
  df$Garage.Qual <- ifelse(is.na(df$Garage.Qual), 0, df$Garage.Qual)
  
  df$Garage.Cond <- factor(df$Garage.Cond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Garage.Cond <- as.numeric(df$Garage.Cond)
  df$Garage.Cond <- ifelse(is.na(df$Garage.Cond), 0, df$Garage.Cond)
  
  df$Paved.Drive <- factor(df$Paved.Drive, levels = c("N", "P", "Y"))
  df$Paved.Drive <- as.numeric(df$Paved.Drive)
  
  df$Pool.QC <- factor(df$Pool.QC, levels = c("Fa", "TA", "Gd", "Ex"))
  df$Pool.QC <- as.numeric(df$Pool.QC)
  df$Pool.QC <- ifelse(is.na(df$Pool.QC), 0, df$Pool.QC)
  
  df$Fence <- factor(df$Fence, levels = c("MnWw", "GdWo", "MnPrv", "GdPrv"))
  df$Fence <- as.numeric(df$Fence)
  df$Fence <- ifelse(is.na(df$Fence), 0, df$Fence)
  
  return(df)
}

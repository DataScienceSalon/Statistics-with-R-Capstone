#------------------------------------------------------------------------------#
#                               preprocess                                     #
#------------------------------------------------------------------------------#
#' preprocess
#'
#' \code{preprocess} Preprocesses the data based upon findings from IDA
#'
#' @param data Dataframe containing data for preprocessing
#'
#' @return df Dataframe with processed data
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
preprocess <- function(data) {
  
  df <- data
  df <- df %>% filter(Sale.Condition == "Normal")
  
  # Convert numeric categorical variables to factors
  df <- reformat(df)
  
  # Create new variables
  df$age <- 2018 - df$Year.Built 
  df$age.remod <- 2018 = df$Year.Remod.Add
  df$Has.Bsmt.Full.Bath <- ifelse(df$Bsmt.Full.Bath == 0, FALSE, TRUE)
  df$Has.Half.Bath <- ifelse(df$Half.Bath > 0, TRUE, FALSE)
  df$Has.Wood.Deck.SF <- ifelse(df$Wood.Deck.SF > 0, TRUE, FALSE)
  df$Has.Open.Porch.SF <- ifelse(df$Open.Porch.SF > 0, TRUE, FALSE)
  
  # Log transfer skewed numeric variables
  df$log.Area <- log(df$area)
  df$log.Price <- log(df$price)
  df$log.Lot.Area <- log(df$Lot.Area)
  df$log.Mas.Vnr.Area <- log(df$Mas.Vnr.Area)
  df$log.BsmtFin.SF.1 <- log(df$BsmtFin.SF.1)
  df$log.Bsmt.Unf.SF <- log(df$Bsmt.Unf.SF)
  df$log.X1st.Flr.SF <- log(df$X1st.Flr.SF)
  df$log.age <- log(df$age)
  df$log.age.remod <- log(df$age.remod)
  
  # Collapse categorical levels
  lvls <- c("40", "45", "75", "85", "180", "190")
  df$MS.SubClass <- ifelse(df$MS.SubClass %in% lvls, "Other", df$MS.SubClass)
  df$MS.SubClass <- factor(df$MS.SubClass)
  
  df$MS.Zoning <- ifelse(df$MS.Zoning ==  "RL", df$MS.Zoning , "Other")
  df$MS.Zoning <- factor(df$MS.Zoning)
  
  df$Lot.Shape <- ifelse(df$Lot.Shape == "Reg", df$Lot.Shape, "Other")
  df$Lot.Shape <- factor(df$Lot.Shape)
  
  df$Lot.Config <- ifelse(df$Lot.Config %in% c("FR2", "FR3"), "FR", df$Lot.Config)
  df$Lot.Config <- factor(df$Lot.Config)
  
  lvls <- c("Blueste", "Greens", "GrnHill", "Landmrk", "NPkVill")
  df$Neighborhood <- ifelse(df$Neighborhood %in% lvls, "Other", df$Neighborhood)
  df$Neighborhood <- factor(df$Neighborhood)
  
  lvls <- c("1.5Unf", "2.5Fin", "2.5Unf")
  df$House.Style <- ifelse(df$House.Style %in% lvls, "Other", df$House.Style)
  df$House.Style <- factor(df$House.Style)
  
  lvls <- c(1,2,3)
  df$Overall.Qual <- ifelse(df$Overall.Qual == 10, 9, df$Overall.Qual)
  df$Overall.Qual <- ifelse(df$Overall.Qual %in%  lvls, 2, df$Overall.Qual)
  df$Overall.Qual <- factor(df$Overall.Qual, 
                              levels = c(9,8,7,6,5,4,2),
                              labels = c("Excellent", "Very Good", "Good", 
                                         "Above Average", "Average", 
                                         "Below Average", "Poor"))
  
  lvls <- c(1,2,3)
  df$Overall.Cond <- ifelse(df$Overall.Cond == 10, 9, df$Overall.Cond)
  df$Overall.Cond <- ifelse(df$Overall.Cond %in%  lvls, 2, df$Overall.Cond)
  df$Overall.Cond <- factor(df$Overall.Cond, 
                            levels = c(9,8,7,6,5,4,2),
                            labels = c("Excellent", "Very Good", "Good", 
                                       "Above Average", "Average", 
                                       "Below Average", "Poor"))
  
  df$Roof.Style <- ifelse(df$Roof.Style != 'Gable', 'Other', df$Roof.Style)
  df$Roof.Style <- factor(df$Roof.Style)
  
  lvls <- c("VinylSd", "Wd Sdng", "MetalSd", "HdBoard", "Plywood", "CemntBd", "BrkFace")
  df$Exterior.1st <- ifelse(df$Exterior.1st %in% lvls, df$Exterior.1st, "Other")
  df$Exterior.1st <- factor(df$Exterior.1st)
  
  lvls <- c("BrkFace", "None")
  df$Mas.Vnr.Type <- ifelse(df$Mas.Vnr.Type %in% lvls, df$Mas.Vnr.Type, "Other")
  df$Mas.Vnr.Type <- factor(df$Mas.Vnr.Type)
  
  lvls <- c("Average/Typical", "Fair")
  df$Exter.Qual <- ifelse(df$Exter.Qual %in% lvls, "Average/Typical/Fair", df$Exter.Qual)
  df$Exter.Qual <- factor(df$Exter.Qual)
  
  lvls <- c("Excellent", "Good")
  df$Exter.Cond <- ifelse(df$Exter.Cond %in% lvls, "Excellent/Good", df$Exter.Cond)
  lvls <- c("Average/Typical", "Fair")
  df$Exter.Cond <- ifelse(df$Exter.Cond %in% lvls, "Average/Typical/Fair", df$Exter.Cond)
  df$Exter.Cond <- factor(df$Exter.Cond)
  
  lvls <- c("CBlock", "PConc", "BrkTill")
  df$Foundation <- ifelse(df$Foundation %in% lvls, df$Foundation, "Other")
  df$Foundation <- factor(df$Foundation)
  
  lvls <- c("Fair", "Poor")
  df$Bsmt.Qual <- ifelse(df$Bsmt.Qual %in% lvls, "Fair/Poor", df$Bsmt.Qual)
  df$Bsmt.Qual <- factor(df$Bsmt.Qual)
  
  lvls <- c("Fair", "Poor")
  df$Heating.QC <- ifelse(df$Heating.QC %in% lvls, "Fair/Poor", df$Heating.QC)
  df$Heating.QC <- factor(df$Heating.QC)
  
  lvls <- c("Fair", "Poor")
  df$Kitchen.Qual <- ifelse(df$Kitchen.Qual %in% lvls, "Fair/Poor", df$Kitchen.Qual)
  df$Kitchen.Qual <- factor(df$Kitchen.Qual)
  
  lvls <- c("Attchd", "BuiltIn", "Detchd")
  df$Garage.Type <- ifelse(df$Garage.Type %in% lvls, df$Garage.Type, "Other")
  df$Garage.Type <- factor(df$Garage.Type, labels = c("Attchd", "BuiltIn", "Detchd", "Other"))
  
  
  # Impute missing values
  df$Year.Remod.Add

  # Remove select variables
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
  df$X2nd.Flr.SF <- NULL
  df$Low.Qual.Fin.SF <- NULL
  df$Bsmt.Full.Bath <- NULL
  df$Bsmt.Half.Bath <- NULL
  df$Half.Bath <- NULL
  df$Kitchen.AbvGr <- NULL
  df$Functional <- NULL
  df$Fireplace.Qu <- NULL
  df$Garage.Qual <- NULL
  df$Garage.Cond <- NULL
  df$Paved.Drive <- NULL
  df$Wood.Deck.SF <- NULL
  df$Open.Porch.SF <- NULL
  df$Enclosed.Porch <- NULL
  df$X3Ssn.Porch <- NULL
  df$Screen.Porch <- NULL
  df$Pool.Area <- NULL
  df$Pool.QC <- NULL
  df$Fence <- NULL
  df$Misc.Feature <- NULL
  df$Misc.Val <- NULL
  df$Sale.Type <- NULL
  
  return(df)
}
  
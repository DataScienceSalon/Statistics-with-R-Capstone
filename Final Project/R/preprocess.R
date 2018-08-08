#------------------------------------------------------------------------------#
#                               preprocess                                     #
#------------------------------------------------------------------------------#
#' preprocess
#'
#' \code{preprocess} Preprocesses the data based upon findings from IDA
#'
#' @param data Dataframe containing data for preprocessing
#' @param fileName Character string containing the file name without extension 
#'
#' @return df Dataframe with processed data
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
preprocess <- function(data, fileName) {
  
  df <- data
  
  # Remove identifier
  df$PID <- NULL
  
  # Convert ordinals to numeric
  df <- convertOrdinals(df)

  # Collapse categorical levels
  df$MS.SubClass <- factor(df$MS.SubClass)
  lvls <- c("40", "45", "75", "85", "180", "190")
  levels(df$MS.SubClass)[which(levels(df$MS.SubClass) %in% lvls)]  <- "Other"
  
  levels(df$MS.Zoning)[which(levels(df$MS.Zoning) != 'RL')]  <- "Other"
  
  levels(df$Lot.Config)[which(levels(df$Lot.Config) %in% c("FR2", "FR3"))]  <- "FR"
  
  levels(df$Condition.1)[which(levels(df$Condition.1) !=  "Norm")]  <- "Other"
  
  levels(df$Land.Contour)[which(levels(df$Land.Contour) !=  "Lvl")]  <- "Other"
  
  lvls <- c("Blueste", "Greens", "GrnHill", "Landmrk", "NPkVill")
  levels(df$Neighborhood)[which(levels(df$Neighborhood) %in% lvls)]  <- "Other"
  
  levels(df$Bldg.Type)[which(levels(df$Bldg.Type) != "1Fam")]  <- "Other"
  
  lvls <- c("1.5Unf", "2.5Fin", "2.5Unf")
  levels(df$House.Style)[which(levels(df$House.Style) %in% lvls)]  <- "Other"
  
  levels(df$Roof.Style)[which(levels(df$Roof.Style) != "Gable")]  <- "Other"
  
  lvls <- c("VinylSd", "Wd Sdng", "MetalSd", "HdBoard", "Plywood", "CemntBd", "BrkFace")
  levels(df$Exterior.1st)[which(!levels(df$Exterior.1st) %in% lvls)]  <- "Other"
  
  lvls <- c("VinylSd", "Wd Sdng", "MetalSd", "HdBoard", "Plywood", "CemntBd", "BrkFace")
  levels(df$Exterior.2nd)[which(!levels(df$Exterior.2nd) %in% lvls)]  <- "Other"
  
  lvls <- c("BrkFace", "None")
  levels(df$Mas.Vnr.Type)[which(!levels(df$Mas.Vnr.Type) %in% lvls)]  <- "Other"
  
  lvls <- c("CBlock", "PConc", "BrkTill")
  levels(df$Foundation)[which(!levels(df$Foundation) %in% lvls)]  <- "Other"
  
  levels(df$Central.Air)[which(is.na(df$Central.Air))]  <- "N"
  
  lvls <- c("Attchd", "BuiltIn", "Detchd")
  levels(df$Garage.Type)[which(!levels(df$Garage.Type) %in% lvls)]  <- "Other"
  lvls <- levels(df$Garage.Type)
  lvls[length(lvls) + 1] <- "None"
  df$Garage.Type <- factor(df$Garage.Type, levels = lvls)
  df$Garage.Type[is.na(df$Garage.Type)]  <- "None"
  
  # Subset only properties sold under normal conditions
  df <- df %>% filter(Sale.Condition == "Normal")
  
  # Impute missing values
  df$Garage.Yr.Blt <- ifelse(is.na(df$Garage.Yr.Blt), df$Year.Built, df$Garage.Yr.Blt)
  df$Garage.Cars <- ifelse(is.na(df$Garage.Cars), 0, df$Garage.Cars)
  df$Garage.Area <- ifelse(is.na(df$Garage.Area), 0, df$Garage.Area)
  df$BsmtFin.SF.1 <- ifelse(is.na(df$BsmtFin.SF.1), 0, df$BsmtFin.SF.1)
  df$Bsmt.Unf.SF <- ifelse(is.na(df$Bsmt.Unf.SF), 0, df$Bsmt.Unf.SF)
  df$Total.Bsmt.SF <- ifelse(is.na(df$Total.Bsmt.SF), 0, df$Total.Bsmt.SF)
  df$Bsmt.Full.Bath <- ifelse(is.na(df$Bsmt.Full.Bath), 0, df$Bsmt.Full.Bath)
  df$Bsmt.Half.Bath <- ifelse(is.na(df$Bsmt.Half.Bath), 0, df$Bsmt.Half.Bath)
  
  # Create new variables
  df$age <- 2018 - df$Year.Built 
  df$age.remod <- 2018 - df$Year.Remod.Add
  df$age.garage <- 2018 - df$Garage.Yr.Blt
  
  # Convert NAs in numeric's to zero where appropriate
  df$Mas.Vnr.Area[is.na(df$Mas.Vnr.Area)] <- 0
  
  # Create log transformations
  df$age.log <- log(df$age+1)
  df$age.garage.log <- log(df$age.garage+1)
  df$age.remod.log <- df$age.remod
  df$area.log <- df$area
  df$BsmtFin.SF.2.log <- log(df$BsmtFin.SF.2+1)
  df$Enclosed.Porch.log <- log(df$Enclosed.Porch+1)
  df$Garage.Area.log <- log(df$Garage.Area+1)
  df$Lot.Area.log <- log(df$Lot.Area+1)
  df$Low.Qual.Fin.SF.log <- log(df$Low.Qual.Fin.SF+1)
  df$Mas.Vnr.Area.log <- log(df$Mas.Vnr.Area+1)
  df$Open.Porch.SF.log <- log(df$Open.Porch.SF+1)
  df$price.log <- log(df$price+1)
  df$Screen.Porch.log <- log(df$Screen.Porch+1)
  df$Total.Bsmt.SF.log <- log(df$Total.Bsmt.SF+1)
  df$TotRms.AbvGrd.log <- log(df$TotRms.AbvGrd+1)
  df$Wood.Deck.SF.log <- log(df$Wood.Deck.SF+1)
  df$X1st.Flr.SF.log <- log(df$X1st.Flr.SF+1)
  df$X2nd.Flr.SF.log <- log(df$X2nd.Flr.SF+1)
  df$X3Ssn.Porch.log <- log(df$X3Ssn.Porch+1)
  df$Bsmt.Unf.SF.sqrt <- sqrt(df$Bsmt.Unf.SF+1)
  
  # Remove variables
  df$age <- NULL
  df$age.remod <- NULL
  df$age.garage <- NULL
  df$area <- NULL
  df$BsmtFin.SF.2 <- NULL
  df$Enclosed.Porch <- NULL
  df$Garage.Area <- NULL
  df$Low.Qual.Fin.SF <- NULL
  df$Mas.Vnr.Area <- NULL
  df$Open.Porch.SF <- NULL
  df$price <- NULL
  df$Screen.Porch <- NULL
  df$Total.Bsmt.SF <- NULL
  df$TotRms.AbvGrd <- NULL
  df$Wood.Deck.SF <- NULL
  df$X1st.Flr.SF <- NULL
  df$X2nd.Flr.SF <- NULL
  df$Alley <- NULL
  df$Condition.2 <- NULL
  df$Heating <- NULL
  df$Misc.Feature <- NULL
  df$Roof.Matl <- NULL
  df$Sale.Type <- NULL
  df$Street <- NULL
  df$Bsmt.Cond <- NULL
  df$Bsmt.Half.Bath <- NULL
  df$BsmtFin.SF.1 <- NULL
  df$BsmtFin.Type.2 <- NULL
  df$Electrical <- NULL
  df$Exter.Cond <- NULL
  df$Fence <- NULL
  df$Functional <- NULL
  df$Garage.Cond <- NULL
  df$Garage.Qual <- NULL
  df$Garage.Yr.Blt <- NULL
  df$Kitchen.AbvGr <- NULL
  df$Land.Slope <- NULL
  df$Lot.Frontage <- NULL
  df$Misc.Val <- NULL
  df$Paved.Drive <- NULL
  df$PID <- NULL
  df$Pool.Area <- NULL
  df$Pool.QC <- NULL
  df$Utilities <- NULL
  df$Year.Built <- NULL
  df$Year.Remod.Add <- NULL
  df$Bsmt.Unf.SF <- NULL
  
  
  
  
  
  
  
  
  datastr <- capture.output(str(df))
  save(df, file = file.path("../data/preprocessed",paste0(fileName, ".Rdata")))
  write.csv(file = file.path("../data/preprocessed",paste0(fileName, ".csv")), df)
  write.csv(file = file.path("../data/preprocessed",paste0("structure.csv")), datastr)
  
  return(df)
}
  
#------------------------------------------------------------------------------#
#                                 reformat                                     #
#------------------------------------------------------------------------------#
#' reformat
#'
#' \code{reformat} Converts numeric categorical variables to factors
#'
#' @param data Dataframe containing data for preprocessing
#'
#' @return df Dataframe with processed data
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
reformat <- function(data) {
  
  data$MS.SubClass <- factor(data$MS.SubClass)
  
  data$Lot.Shape <- factor(data$Lot.Shape, 
                           levels = c("Reg", "IR1", "IR2", "IR3"))
  
  data$Utilities <- factor(data$Utilities, 
                           levels = c("AllPub", "NoSewr", "NoSeWa", "ELO"))
  
  data$Land.Slope <- factor(data$Land.Slope, 
                            levels = c("Gtl", "Mod", "Sev"))

  data$Exter.Qual <- factor(data$Exter.Qual, 
                            levels = c("Ex", "Gd", "TA", "Fa", "Po"),
                            labels = c("Excellent", "Good", "Average/Typical",
                                       "Fair", "Poor"))
  
  data$Exter.Cond <- factor(data$Exter.Cond, 
                            levels = c("Ex", "Gd", "TA", "Fa", "Po"),
                            labels = c("Excellent", "Good", "Average/Typical",
                                       "Fair", "Poor"))
  
  data$Bsmt.Qual <- factor(data$Bsmt.Qual,  
                           levels = c("Ex", "Gd", "TA", "Fa", "Po", "NA"),
                           labels = c("Excellent", "Good", "Typical", "Fair", 
                                      "Poor", "No Basement"))
  
  data$Bsmt.Cond <- factor(data$Bsmt.Cond,  
                           levels = c("Ex", "Gd", "TA", "Fa", "Po", "NA"),
                           labels = c("Excellent", "Good", "Typical", "Fair", 
                                      "Poor", "No Basement"))
  
  data$Bsmt.Exposure <- factor(data$Bsmt.Exposure,  
                               levels = c("Gd", "Av", "Mn", "No", "NA"),
                               labels = c("Good Exposure", "Average Exposure", 
                                      "Minimum Exposure", "No Exposure", 
                                      "No Basement"))
  
  data$BsmtFin.Type.1 <- factor(data$BsmtFin.Type.1,  
                                levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", 
                                           "Unf", "NA"),
                                labels = c("Good Living Quarters",
                                           "Average Living Quarters", 
                                           "Average Rec Room",
                                           "Below Average Living Quarters",
                                           "Low Quality", "Unfinished", 
                                           "No Basement"))
  data$BsmtFin.Type.2 <- factor(data$BsmtFin.Type.2,  
                                levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", 
                                           "Unf", "NA"),
                                labels = c("Good Living Quarters",
                                           "Average Living Quarters", 
                                           "Average Rec Room",
                                           "Below Average Living Quarters",
                                           "Low Quality", "Unfinished", 
                                           "No Basement"))
  
  data$Heating.QC <- factor(data$Heating.QC,  
                           levels = c("Ex", "Gd", "TA", "Fa", "Po"),
                           labels = c("Excellent", "Good", "Average/Typical", "Fair", 
                                      "Poor"))
  
  data$Electrical <- factor(data$Electrical,  
                            levels = c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix"),
                            labels = c("Standard", "Average", "Fair", "Poor", 
                                       "Mixed"))
  
  data$Kitchen.Qual <- factor(data$Kitchen.Qual,  
                            levels = c("Ex", "Gd", "TA", "Fa", "Po"),
                            labels = c("Excellent", "Good", "Average/Typical", "Fair", 
                                       "Poor"))
  
  data$Functional <- factor(data$Functional,  
                            levels = c("Typ", "Min1", "Min2", "Mod", "Maj1",
                                       "Maj2", "Sev", "Sal"),
                            labels = c("Typical", "Minor Deductions 1", "Minor Deductions 1",
                                       "Moderage Deductions",  "Major Deductions 1",
                                       "Major Deductions 1", "Severely Damaged",
                                       "Salvage Only"))
  
  data$Fireplace.Qu <- factor(data$Fireplace.Qu,  
                              levels = c("Ex", "Gd", "TA", "Fa", "Po", "NA"),
                              labels = c("Excellent", "Good", "Typical", "Fair", 
                                      "Poor", "No Fireplace"))
  
  data$Garage.Finish <- factor(data$Garage.Finish,  
                           levels = c("Fin", "RFn", "Unf", "NA"),
                           labels = c("Finished", "Rough Finished", "Unfinished",
                                      "No Garage"))
  
  data$Garage.Qual <- factor(data$Garage.Qual,  
                              levels = c("Ex", "Gd", "TA", "Fa", "Po", "NA"),
                              labels = c("Excellent", "Good", "Typical", "Fair", 
                                         "Poor", "No Garage"))
  
  data$Garage.Cond <- factor(data$Garage.Cond,  
                             levels = c("Ex", "Gd", "TA", "Fa", "Po", "NA"),
                             labels = c("Excellent", "Good", "Typical", "Fair", 
                                        "Poor", "No Garage"))
  
  data$Paved.Drive <- factor(data$Paved.Drive,  
                             levels = c("Y", "P", "N"),
                             labels = c("Paved", "Partial Pavement", "Dirt/Gravel"))
  
  data$Pool.QC <- factor(data$Pool.QC,  
                             levels = c("Ex", "Gd", "TA", "Fa", "NA"),
                             labels = c("Excellent", "Good", "Typical", "Fair", 
                                        "No Pool"))
  
  data$Fence <- factor(data$Fence,  
                         levels = c("GdPrv", "MnPrv", "GdWo", "MnWw", "NA"),
                         labels = c("Good Privacy", "Minimum Privacy", 
                                    "Good Wood", "Minimum Wood/Wire", 
                                    "No Fence"))
  
  data$Mo.Sold <- factor(data$Mo.Sold,
                         levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(data)
}

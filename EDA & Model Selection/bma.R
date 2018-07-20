#==============================================================================#
#                     Bayes Model Averaging Fit                                #
#==============================================================================#
#' bma
#'
#' \code{bma} Performs BMA using several default priors on data set 
#'
#' @param ames_train Data frame containing the vector y and matrix X of parameters.
#' @return list of BMA models
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family BMA functions
#' @export
bma <- function(ames_train) {
  
  #---------------------------------------------------------------------------#
  #                           Model Averaging                                 #
  #---------------------------------------------------------------------------#
  n = nrow(ames_train)
  
  models <- list()
  
  models[["BIC"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                   Year.Remod.Add + Bedroom.AbvGr,
                                 data = ames_train, prior = "BIC",
                                 modelprior = uniform(), method = "BAS")
  models[["BIC"]]$priorDesc <- 'Bayesian Information Criteria (BIC)'
  
  
  models[["AIC"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                   Year.Remod.Add + Bedroom.AbvGr,
                                 data = ames_train, prior = "AIC",
                                 modelprior = uniform(), method = "BAS")
  models[["AIC"]]$priorDesc <- 'Akaike Information Criterion (AIC)'
  
  
  
  models[["EB-global"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                         Year.Remod.Add + Bedroom.AbvGr,
                                       data = ames_train, prior = "EB-global", initprobs = "eplogp",
                                       modelprior = uniform(), method = "BAS")
  models[["EB-global"]]$priorDesc <- 'Empirical Bayes (Global)'
  
  
  models[["EB-local"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                        Year.Remod.Add + Bedroom.AbvGr,
                                      data = ames_train, prior = "EB-local", initprobs = "eplogp",
                                      modelprior = uniform(), method = "BAS")
  models[["EB-local"]]$priorDesc <- 'Empirical Bayes (Local)'
  
  models[["g-prior"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                       Year.Remod.Add + Bedroom.AbvGr,,
                                     data = ames_train, prior = "g-prior", alpha = nrow(ames_train),
                                     modelprior = uniform(), method = "BAS")
  models[["g-prior"]]$priorDesc <- "Zellner's g-prior"
  
  
  models[["hyper-g"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                       Year.Remod.Add + Bedroom.AbvGr,
                                     data = ames_train, prior = "hyper-g",alpha = 3,
                                     modelprior = uniform(), method = "BAS")
  models[["hyper-g"]]$priorDesc <- 'Hyper-g'
  
  
  models[["hyper-g-laplace"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                               Year.Remod.Add + Bedroom.AbvGr,
                                             data = ames_train, prior = "hyper-g-laplace",
                                             modelprior = uniform(), method = "BAS")
  models[["hyper-g-laplace"]]$priorDesc <- 'Hyper-g Laplace'
  
  
  models[["hyper-g-n"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                         Year.Remod.Add + Bedroom.AbvGr,
                                       data = ames_train, prior = "hyper-g-n",
                                       modelprior = uniform(), method = "BAS")
  
  models[["hyper-g-n"]]$priorDesc <- 'Hyper-g-n'
  
  
  models[["ZS-null"]] <- BAS::bas.lm(log(price) ~ Lot.Area + Land.Slope + Year.Built + 
                                       Year.Remod.Add + Bedroom.AbvGr,
                                     data = ames_train, alpha = n, prior = "ZS-null", 
                                     modelprior = uniform(), method = "BAS")
  models[["ZS-null"]]$priorDesc <- 'Zellner-Siow (NULL)'
  
  return(models)
}

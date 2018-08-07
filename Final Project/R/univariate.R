#------------------------------------------------------------------------------#
#                              quantPlots                                      #
#------------------------------------------------------------------------------#
#' quantPlots
#'
#' \code{quantPlots} Returns six univariate analysis plots, including boxplots
#' and histograms, with and without outliers
#' 
#' @param x Dataframe with data to be analyzed and plotted
#' @param cname Column to be analyzed and plotted
#' @param outliers The indices of the outliers
#'
#' @return Vector of outlier indices
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
quantPlots <- function(x, cname, outliers) {
  
  plots <- list()
  if (length(outliers) > 0) {
    
    plots$boxWithOutliers <- plotBox(x[cname], yLab = cname, rotate = TRUE,
                                     pal = "Blues", showMean = FALSE)
    plots$boxNoOutliers <- plotBox(x[-outliers,][cname], yLab = cname, rotate = TRUE,
                                   pal = "Blues", showMean = FALSE)
    plots$histWithOutliers <- plotHist(x[cname], xLab = cname, 
                                       plotTitle = paste("Histogram:", cname, 
                                                         "(w/Outliers)"))
    plots$histNoOutliers <- plotHist(x[-outliers,][cname], xLab = cname, 
                                       plotTitle = paste("Histogram:", cname, 
                                                         "(w/o Outliers)"))
  } else {
    plots$boxWithOutliers <- plotBox(x[cname], yLab = cname, rotate = TRUE,
                                     pal = "Blues")
    plots$histWithOutliers <- plotHist(x[cname], xLab = cname, 
                                       plotTitle = paste("Histogram:", cname))
  }
  plots$histSqrt <- plotHist(sqrt(x[cname]), xLab = cname, 
                             plotTitle = paste("Histogram:", cname, 
                                               "(Sqrt Transformation)"))
  
  plots$histLog <- plotHist(log(x[cname]), xLab = cname, 
                            plotTitle = paste("Histogram:", cname))
  
  
  
  return(plots)
  
}
#------------------------------------------------------------------------------#
#                              getOutliers                                     #
#------------------------------------------------------------------------------#
#' getOutliers
#'
#' \code{getOutliers} Returns indices of outliers for a variable.
#' 
#' @param x Dataframe containing the data to be analyzed
#' @param cname Character string indicating column to be analyzed
#' @param extreme Logical, if true, returns outliers at 3*IQR, 
#' otherwise 1.5*IQR
#'
#' @return Vector of outlier indices
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
getOutliers <- function(x, cname, extreme = FALSE) {
  
  range <- 1.5
  if (extreme) range <- 3
  
  lowerq <- quantile(x[[cname]], na.rm = TRUE)[2]
  upperq <- quantile(x[[cname]], na.rm = TRUE)[4]
  
  iqr <- upperq - lowerq 
  lowerThreshold = lowerq - (iqr * range)
  upperThreshold = upperq + (iqr * range)
  
  outlierIdx <- which(x[[cname]] > upperThreshold | x[[cname]] < lowerThreshold)
  df <- data.frame(term = cname,
                   outliers = length(outlierIdx),
                   pct = length(outlierIdx) / length(x[[cname]]) * 100,
                   mean1 = mean(x[[cname]]),
                   mean2 = mean(x[[cname]][-c(outlierIdx)]))
  names(df) <- c("Term", "Outliers", "Outlier Pct", "Mean with Outliers", "Mean w/o Outliers")
  cases <- x[outlierIdx,]
  
  outliers <- list()
  outliers$idx <- outlierIdx
  outliers$stats <- df
  outliers$cases <- cases
  
  return(outliers)
  
}

#------------------------------------------------------------------------------#
#                              qualStats                                       #
#------------------------------------------------------------------------------#
#' qualStats
#'
#' \code{qualStats} Creates a dataframe containing the contingency matrix
#' for a categorical variable.
#' 
#' @param x Categorical variable with no more than 20 distinct levels 
#'
#' @return A dataframe of descriptive statistics
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
qualStats <- function(x) {
  
  # Obtain the contingency data from Hmisc describe class
  desc <- Hmisc::describe(x)
  
  # Extract the values and frequencies and compute the total frequencies
  value <- as.data.frame(desc[[1]]$values$value, stringsAsFactors = FALSE)
  freq <- as.data.frame(desc[[1]]$values$frequency)
  freq <- rbind(freq, sum(desc[[1]]$values$frequency))
  
  # Compute the proportions and convert the frequencies to characters
  # This conversion is performed in order to avoid converting the 
  # precision of the frequencies to that of the proportions.
  prop <- freq / nrow(x)
  freq <- apply(freq, 1, as.character)
  
  # Combine the frequencies and proportions, and provide row and column names
  tbl <- cbind(freq, prop)
  ttl <- c(sum(desc[[1]]$values$frequency), sum(prop))
  colnames(tbl) <- c('Frequency', 'Proportion')
  rownames(tbl)  <- c(desc[[1]]$values$value, "Total")
  
  # Transpose, and name the  and voila
  tbl <- t(tbl)
  return(tbl)
}
  
#------------------------------------------------------------------------------#
#                              quantStats                                      #
#------------------------------------------------------------------------------#
#' quantStats
#'
#' \code{quantStats} Creates a dataframe containing the descriptive 
#' statistics for a quantitative variable.
#' 
#' @param x Quantitative variable
#'
#' @return A dataframe of descriptive statistics
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
quantStats <- function(x) {
  
  options(scipen=100)
  options(digits=3)
  
  desc <- Hmisc::describe(x)
  desc <- t(as.data.frame(desc$counts))
  rownames(desc) <- NULL
  desc <- as.data.frame(desc)
  desc$missing <- sum(is.na(x))
  desc$pctMissing <- sum(is.na(x)) / length(x) * 100
  desc <- desc[,c(1,2,14,3,4,5,7,9,10,11,13)]
  
  return(desc)
}

#------------------------------------------------------------------------------#
#                               univariate                                     #
#------------------------------------------------------------------------------#
#' univariate
#'
#' \code{univariate} Creates tabular and graphical summaries of dataframes
#' 
#' @param x Dataframe to be summarized
#'
#' @return A list containing summary dataframes and plots
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Summarize Functions
#' @export
univariate <- function(x) {
  
  # Obtain column names
  cnames <- names(x)
  
  # Obtain summaries for each column in the dataframe
  analysis <- lapply(cnames, function(cname) {
    
    a <- list()
    
    if (is.numeric(x[[cname]]) | is.integer(x[[cname]])) {
      a$tbl <- quantStats(x[[cname]])
      a$outliers <- getOutliers(x, cname)
      a$plots <- quantPlots(x, cname, a$outliers$idx)
    } else {
      a$tbl <- qualStats(x[cname])
      a$plot <- plotBar(x[cname], yLab = 'Frequency', xLab = cname, legend = FALSE)
    }
    a 
  })
  names(analysis) <- cnames
  return(analysis)
}

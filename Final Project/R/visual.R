#==============================================================================#
#                             Visualization Functions                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                               Plot Histogram                                 #
#------------------------------------------------------------------------------#
#' plotHist
#'
#' \code{plotHist} Renders a histogram with a normal curve as well as a table
#' of summary statistics.
#'
#' @param data Data frame containing a single numeric variable
#' @param xLab Capital case character string the group or subset of data being printed (optional)
#' @param yLab Capital case character string describing y is being printed
#' @param plotTitle Capital case character string for the plotTitle of the plot
#'
#' @return List containing a summary statistics and a histogram
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotHist <- function(data, xLab = NULL, yLab, plotTitle = NULL) {
  
  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- yLab
    } else {
      plotTitle <- paste("Histogram: ",xLab)
    }
  }
  
  colorCount <- 30
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  hist <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[[1]])) +
    ggplot2::geom_histogram(fill = 'steelblue4') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) 
  
  
  return(hist)
}

#------------------------------------------------------------------------------#
#                                   plotBar                                    #
#------------------------------------------------------------------------------#
#' plotBar
#'
#' \code{plotBar} Renders a bar plot with bars sequenced by value left to right.
#'
#' @param data Data frame or vector containing a single categorical factor variable
#' @param yLab Capital case character string describing the y variable
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param plotTitle Capital case character string for the title of the plot
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBar <- function(data, yLab, xLab, plotTitle = NULL, values = TRUE,
                    horizontal = FALSE, legend = TRUE) {
  
  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab, "Categorical Levels")
  }
  
  # If raw data sent, prepare plot data as categorical level bar plot
  if (ncol(data) == 1) {
    data <- as.data.frame(table(data[[1]])) %>% filter(Freq > 0)
  }
  
  # If more than 5 categorical levels, make plot horizontal
  if (nrow(data) > 5) horizontal <- TRUE
  
  # Render plot
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  if (horizontal) {
    barPlot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = data[[1]],
                                            y = data[[2]],
                                            fill = data[[1]])) +
      scale_x_discrete(limits = rev(levels(data[[1]])))
  } else {
    barPlot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = data[[1]],
                                            y = data[[2]],
                                            fill = data[[1]]))
  }
  barPlot <- barPlot +
    ggplot2::geom_bar(stat='identity', fill = 'steelblue4') +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::ylab(yLab) +
    ggplot2::xlab(xLab) +
    ggplot2::scale_y_continuous(labels = scales::comma)
  
  if (values == TRUE) {
    barPlot <- barPlot + ggplot2::geom_text(ggplot2::aes(label=round(data[[2]], 3)),
                                            family="Open Sans",
                                            vjust=.2, color="black",
                                            size=5)
  }
  
  if (legend) {
    barPlot <- barPlot + 
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.position = "bottom")
    
  } else {
    barPlot <- barPlot + 
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none")
  }
  
  if (horizontal) {
    barPlot <- barPlot + ggplot2::coord_flip() 
  }
  
  
  return(barPlot)
}

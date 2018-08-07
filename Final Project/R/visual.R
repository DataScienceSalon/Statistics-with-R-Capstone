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
plotHist <- function(data, xLab, plotTitle = NULL) {
  
  if (is.null(plotTitle)) {
    plotTitle <- paste(xLab, "Histogram")
  }
  
  bw <- diff(range(data[[1]])) / (2 * IQR(data[[1]], na.rm = TRUE) / length(data[[1]])^(1/3))
  
  hist <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[[1]], binwidth = bw)) +
    ggplot2::geom_histogram(fill = 'steelblue4') +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::xlab(xLab) 
  
  
  return(hist)
}

#------------------------------------------------------------------------------#
#                           Plot Group Histogram                               #
#------------------------------------------------------------------------------#
#' plotGroupHist
#'
#' \code{plotGroupHist} Renders histogram for multiple levels of a categorical 
#' variable
#'
#' @param data Data frame containing x and a grouping variable
#' @param xLab Capital case character string the group or subset of data being printed (optional)
#' @param yLab Capital case character string describing y is being printed
#' @param plotTitle Capital case character string for the plotTitle of the plot
#'
#' @return List containing a summary statistics and a histogram
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotGroupHist <- function(data, xLab, groupVar, plotTitle = NULL) {
  
  if (is.null(plotTitle)) {
    plotTitle <- paste(xLab, "by", groupVar)
  }
  
  colorCount <- length(unique(data[2]))
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  hist <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[[1]],  
                                                    fill = data[[2]])) +
    ggplot2::geom_histogram(alpha = 0.5, position = "identity") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_fill_discrete(name=groupVar) +
    ggplot2::xlab(xLab) 
  
  
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
    ggplot2::theme_minimal(base_size = 12) +
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


#------------------------------------------------------------------------------#
#                                 Boxplot                                      #
#------------------------------------------------------------------------------#
#' plotBox
#'
#' \code{plotBox} Renders a single or grouped box plot.
#'
#' @param data Data frame or vector containing two columns:
#'          1: Numeric response variable (y)
#'          2: Categorical independent variable (x)
#' @param xLab Capital case character string containing the name of the x variable (optional)
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Character case character string containing the title for the plot
#' @param rotate Logical indicating whether to rotate the plot by 90 degrees
#' @param showMean Logical indicating whether the mean value should be displayed.
#'
#' @return List containing a contingency table and a stacked bar plot.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBox <- function(data, xLab = NULL, yLab, plotTitle = NULL, rotate = FALSE, 
                    showMean = TRUE, pal = "Blues") {
  
  if (length(data) > 2) stop(paste("Error in plotBox: Dimension of data frame must be 1 or 2, not", length(data)))
  if (length(data) == 1) {
    data <- data.frame(x = rep("x", nrow(data)),
                       y = data[[1]], row.names = NULL)
  } else {
    data <- data.frame(y = data[[1]],
                       x = data[[2]],
                       row.names = NULL)
  }
  
  # Format title
  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- yLab
    } else {
      plotTitle <- paste(yLab, "by", xLab)
    }
  }
  
  # Calculate means
  means <- function(x){
    return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 1)))
  }
  
  # Select palette
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(length(unique(data$x)), pal))
  
  # Designate fill value
  if (length(unique(data$x)) == 1) {
    fill <- "steelblue4"
  } else {
    fill <- data$x
  }
  
  # Render plot
  bp <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = reorder(x, -y, mean),
                                     y = data$y,
                                     fill=fill))  +
    ggplot2::geom_boxplot(outlier.colour = "black") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "none") +
    ggplot2::ggtitle(plotTitle)  +
    ggplot2::scale_fill_manual(values = myPal(length(unique(data$x))))
  
  if (showMean == TRUE) {
    bp <- bp + ggplot2::stat_summary(fun.y = mean, colour = "black", 
                                     geom = "point", shape = 12, 
                                     size = 3, show.legend = FALSE) + 
      ggplot2::stat_summary(fun.data = means, geom = "text", 
                            colour = "black", vjust = -0.0,
                            hjust = -1, size = 4)
  }
  
  if (is.null(xLab)) {
    bp <-  bp + ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  }
  
  if (rotate == TRUE) {
    bp <- bp + ggplot2::coord_flip() +
      ggplot2::labs(y = yLab, x = xLab, fill = xLab)
  } else {
    bp <- bp + ggplot2::labs(y = yLab, x = xLab, fill = xLab)
  }

  
  
  return(bp)
}


#------------------------------------------------------------------------------#
#                               Plot Scatterplot                               #
#------------------------------------------------------------------------------#
#' plotScatter
#'
#' \code{plotScatter} Renders a scatterplot for two numerical variablesa
#'
#' @param data Data frame containing the quantitative variables
#' @param xLab Capital case character string containing the name of the grouping or subset variable
#' @param yLab Capital case character string containing the name of the y variable
#' @param plotTitle Capital case character string for title of plot
#'
#' @return Scatterplot object
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotScatter <- function(data, xLab, yLab, plotTitle = NULL,
                        smooth = TRUE) {
  
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }
  
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  scatter <- ggplot2::ggplot(data = data,
                             ggplot2::aes(y = as.numeric(unlist(data[,1])),
                                          x = as.numeric(unlist(data[,2])))) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   legend.position = "right") +
    ggplot2::labs(x = xLab, y = yLab) +
    ggplot2::ggtitle(plotTitle)
  
  if (smooth) {
    scatter <- scatter + ggplot2::geom_smooth(method = lm, se = FALSE) 
  }
  
  return(scatter)
  
}

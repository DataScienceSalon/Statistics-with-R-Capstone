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
#' plotFactorHist
#'
#' \code{plotFactorHist} Renders histogram for multiple levels of a categorical 
#' variable
#'
#' @param x Data frame containing the data 
#' @param cVar Character string containing the name of the column of the continuous variable 
#' @param factorVar Character string containing the name of the factor column
#' @param xLab Capital case character string the group or subset of data being printed (optional)
#' @param plotTitle Capital case character string for the plotTitle of the plot
#'
#' @return List containing a summary statistics and a histogram
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotFactorHist <- function(x, cVar, factorVar, xLab = NULL, plotTitle = NULL, 
                           facets = FALSE) {
  
  if (is.null(plotTitle)) {
    if (is.null(xLab)) {
      plotTitle <- paste(cVar, "by", factorVar)
    } else {
      plotTitle <- paste(xLab, "by", factorVar)
    }
  }
  
  colorCount <- length(unique(x[factorVar]))
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  hist <- ggplot2::ggplot(data = x, ggplot2::aes(x = x[[cVar]],  
                                                 fill = x[[factorVar]])) +
    ggplot2::geom_histogram(alpha = 0.5, position = "identity") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                   axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::scale_fill_discrete(name=factorVar) +
    ggplot2::xlab(xLab) 
  
  if (facets) {
    fmla <- as.formula(paste(factorVar, ".", sep = "~"))
    hist <- hist + facet_grid(fmla, scales = 'free')
  }
  return(hist)
}

#------------------------------------------------------------------------------#
#                                   plotBar                                    #
#------------------------------------------------------------------------------#
#' plotBar
#'
#' \code{plotBar} Renders a bar plot with bars sequenced by value left to right.
#'
#' @param x Data frame or vector containing a single categorical factor variable
#' @param xVar Character string containing the variable to place on the x-axis
#' @param yVar Character string containing the variable to place on the y-axis
#' @param xLab Capital case character string containing the name of the variable x variable
#' @param yLab Capital case character string describing the y variable
#' @param horizontal Logical. If true, bars will be plotted horizontally
#' @param legend Logical, if true the legend will be plotted.
#' @param plotTitle Capital case character string for the title of the plot
#' @param values. Logical, if true the y-values will be printed.
#'
#' @return Bar plot
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotBar <- function(x, xVar, yVar, xLab = NULL, yLab = NULL, plotTitle = NULL, 
                    values = FALSE, horizontal = FALSE, legend = TRUE) {
  
  # Convert x to dataframe
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  # Format xLab and yLab
  if (is.null(xLab)) xLab <- xVar
  if (is.null(yLab)) yLab <- yVar
  
  # Format title
  if (is.null(plotTitle)) {
    plotTitle <- paste(yLab, "by", xLab)
  }
  
  # If more than 5 categorical levels, make plot horizontal
  if (nrow(x[xVar]) > 10) horizontal <- TRUE
  
  # Format data
  data <- data.frame(x = lapply(x[xVar], as.character), 
                     y = x[yVar], stringsAsFactors = FALSE)
  
  # Render plot
  if (horizontal) {
    barPlot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = reorder(data[[1]], data[[2]]),
                                            y = data[[2]],
                                            fill = data[[1]])) 
  } else {
    barPlot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = reorder(data[[1]], -data[[2]]),
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
                                            position = position_stack(vjust = .5),
                                            color="white", size=3)
  }
  
  if (legend) {
    barPlot <- barPlot + 
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     legend.position = "bottom")
    
  } else {
    barPlot <- barPlot + 
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
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
#' @param x Data frame containing the quantitative variables
#' @param xVar Character string containing the name of the x variable column 
#' @param yVar Character string containing the name of the y variable column 
#' @param groupVar Optional character string containing the name of the gruping variable 
#' @param xLab Capital case character string containing the label for xVar
#' @param yLab Capital case character string containing the label for yVar
#' @param groupName Capital case character string containing the label for groupVar
#' @param plotTitle Capital case character string for title of plot
#'
#' @return Scatterplot object
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotScatter <- function(x, xVar, yVar, groupVar = NULL, xLab = NULL, 
                        yLab = NULL, groupName = NULL, plotTitle = NULL,
                        smooth = FALSE) {
  
  if (is.null(xLab)) xLab <- xVar
  if (is.null(yLab)) yLab <- yVar
  if (is.null(groupName)) groupName <- groupVar
  if (is.null(plotTitle))  plotTitle <- paste(yLab, "by", xLab)
  
  data <- data.frame(x = x[[xVar]], y = x[[yVar]], group = x[[groupVar]])
  
  
  myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, "PiYG"))
  
  if (is.null(groupVar)) {
    scatter <- ggplot2::ggplot(data = data,
                               ggplot2::aes(y = as.numeric(unlist(data['y'])),
                                            x = as.numeric(unlist(data['x'])))) 
    
  } else {
    scatter <- ggplot2::ggplot(data = data,
                               ggplot2::aes(y = as.numeric(unlist(data['y'])),
                                            x = as.numeric(unlist(data['x'])),
                                            colour = data[['group']])) +
      ggplot2::theme(legend.position = "right")  +
      ggplot2::scale_color_discrete(name = groupName)
  }
  
  scatter <- scatter + 
    ggplot2::geom_point() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(x = xLab, y = yLab) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::ggtitle(plotTitle)
  
  if (smooth) {
    scatter <- scatter + ggplot2::geom_smooth(method = lm, se = FALSE) 
  }
  
  return(scatter)
  
}

#------------------------------------------------------------------------------#
#                                  Plot Coef                                   #
#------------------------------------------------------------------------------#
#' plotCoef
#'
#' \code{plotCoef} Renders a coefficient plot with error bars for an LM model
#'
#' @param x an LM model object
#' @param plotTitle Capital case character string for title of plot
#' @param intercept if FALSE, the intercept value will not be plotted
#'
#' @return Coefficient Plot 
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family visualization functions
#' @export
plotCoef <- function(x, plotTitle = NULL, intercept = FALSE) {
  
  library(broom)
  
  # Initialize parameters
  if (is.null(plotTitle)) plotTitle <- "Coefficient Estimates"
  
  # Format data
  data <- tidy(x)
  data$lowerCI <- data$estimate - (1.95 * data$std.error)
  data$upperCI <- data$estimate + (1.95 * data$std.error)
  
  # Remove intercept terms if requested
  if (!intercept)  data <- data %>% filter(!grepl("*intercept*", term, ignore.case = TRUE))
  
  # Create factors for term (so that I can reorder them of coordinates are flipped)
  data$term <- factor(data$term)
  # Plot elements
  points <- ggplot2::ggplot(data = data, ggplot2::aes(x = term, y = estimate))

  
  points <- points +
    ggplot2::geom_linerange(aes(ymin = lowerCI, ymax = upperCI)) + 
    ggplot2::geom_pointrange(aes(ymin = lowerCI, ymax = upperCI)) + 
    ggplot2::geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.5) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(text = ggplot2::element_text(family="Open Sans")) +
    ggplot2::labs(x = "Term", y = "Estimate") +
    ggplot2::ggtitle(plotTitle)
  
  if (!intercept) points <- points + ggplot2::labs(x = "Term", y = "Estimate (Intercept Omitted)") 
  
  if (nrow(data) > 5) {
    points <- points + ggplot2::coord_flip() + 
      ggplot2::scale_x_discrete(limits = rev(levels(data$term)))
    
  }
  return(points)
}
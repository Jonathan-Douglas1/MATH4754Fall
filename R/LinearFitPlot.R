#' LinearFitPlot
#'
#' This function creates
#'
#' @param xData - The x axis data
#' @param yData  - The y axis data
#' @param title  - The title name (String)
#' @param xaxislab - x axis label (String)
#' @param yaxislab - y axis label (String)
#'
#' @return data.lm - the linear model of the dataset
#' @export
#'
LinearFitPlot <- function(xData, yData, title, xaxislab, yaxislab){
  #Create linear model out of data and desired axis
  data.lm = lm(yData~xData)

  #Plot the scatterplot with the linear fit and custom labels
  plot(yData ~ xData, main = title, xlab = xaxislab, ylab = yaxislab,
          pch = 21, bg = "blue", cex = 1.2,
       xlim = c(0, 1.1 * max(xData)), ylim =
         c(0, 1.1 * max(yData)))
  abline(data.lm)

  #Return the model object
  return(data.lm)
}

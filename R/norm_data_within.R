norm_data_within <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                             na.rm=FALSE, .drop=TRUE) {
  #' @name norm_data_within
  #' @title norms data within units of observation
  #' @description normalize data within units of observation
  #' @param data main data frame
  #' @param idvar column name of unit of observation as string
  #' @param measurevar column name of measured variable as string
  #' @param betweenvars column names of between-subject variables
  #' @param na.rm should NAs be removed?
  #' @param .drop should unobserved variable combinations be dropped?
  #' @return the normalized data
  #'
  #' @importFrom plyr ddply
  #'
  #' @export
  #'
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(
    data, c(idvar, betweenvars), .drop=.drop, .fun = function(xx, col, na.rm) {
      c(subjMean = mean(xx[,col], na.rm=na.rm))
      },
    measurevar, na.rm
  )

  # Put the subject means with original data
  data <- merge(data, data.subjMean)

  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)

  # Remove this subject mean column
  data$subjMean <- NULL

  return(data)
}

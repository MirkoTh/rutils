summary_se <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                       conf.interval=.95, .drop=TRUE) {
  #' @name summary_se
  #' @title norms data within units of observation
  #' @description normalize data within units of observation
  #' @param data main data frame
  #' @param measurevar column name of measured variable as string
  #' @param groupvars column names of between-subject variables
  #' @param na.rm should NAs be removed?
  #' @param conf.interval width of confidence interval
  #' @param .drop should unobserved variable combinations be dropped?
  #' @return the summarized data frame
  #'
  #' @importFrom plyr ddply rename
  #'
  #'
  #' @export
  #'
  options(warn = 0)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm=na.rm),
      mean = mean   (xx[[col]], na.rm=na.rm),
      sd   = sd     (xx[[col]], na.rm=na.rm)
      )},
    measurevar
  )

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

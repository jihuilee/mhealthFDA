#' Structure the observed data into an equally-spaced data frame
#'
#' @param data Data in a matrix form. Data should contain a variable named 'timestamp' in a 'yyyy-mm-dd hh:mm:ss' format.
#' @param variable Name of variable in the data set you would like to re-structure
#' @param interval The interval of time in second. Default is 3 (i.e. The measurement is evaluated every three second.)
#' @param na.missing If TRUE, missing is considered as NA. If FALSE, the missing is considered as 0. Defulat is FALSE.
#' @param smoothing If TRUE, the curve is smooted first and evaluated at every interval minutes. Defulat is TRUE. If na.missing is TRUE, smoothing is automatically FALSE.
#'
#' @importFrom hms hms
#' @importFrom hms as.hms
#'
#' @export

data_grid = function(data, variable, interval = 3, na.missing = FALSE, smoothing = TRUE)
{
  if(na.missing) {smoothing = FALSE}
  if(na.missing & smoothing) {warning("If na.missing = TRUE, smoothing should be FALSE. Respecifying..."); smoothing = FALSE}

  data$date = as.Date(substr(data$timestamp, 1, 10))
  data$time = as.hms(substr(data$timestamp, 12, 19))
  data = data[order(data$date),]

  # Create a sequence of time by a minute

  # Hour, minute
  tseq0 = as.hms(seq(hms(0, 0, 0), hms(00, 00, 24), length = as.numeric(difftime(hms(0, 0, 24), hms(0, 0, 0), units = "min")) + 1))
  tseq0 = tseq0[-length(tseq0)]
  tlength0 = length(tseq0)

  # Every (interval) minutes
  if(interval == 1) {tseq = tseq0; tlength = tlength0} else{
    tseq = tseq0[interval * (1:round((tlength0 + 1)/interval)-1) + 1]
    tlength = length(tseq)
  }

  datetable = unique(data$date)
  ndays = length(datetable)
  out00 = matrix(NA, nrow = ndays, ncol = tlength0) # Missing = NA
  out11 = matrix(0, nrow = ndays, ncol = tlength0) # Missing = 0
  out22 = matrix(NA, nrow = ndays, ncol = tlength) # More sparse matrix for FPCA

  for(d in 1:ndays)
  {
    date.d = datetable[d]
    data.d = data[data$date == date.d,]
    data.d = data.d[order(data.d$time),]

    data.d$time2 = as.hms(paste0(substr(data.d$time, 1, 6), "00"))

    # Delete duplicate
    data.d = data.d[!duplicated(data.d$time2),]

    out00[d, (1:tlength0)[tseq0 %in% data.d$time2]] = out11[d, (1:tlength0)[tseq0 %in% data.d$time2]] = data.d[,names(data.d) == variable]

    fn.d = splinefun(tseq0, out11[d,])
    out22[d,] = fn.d(tseq)
  }

  out00_2 = out00[,tseq0 %in% tseq]
  out11_2 = out11[,tseq0 %in% tseq]

  Date = unique(data$date)
  DayofWeek = factor(weekdays(Date), level = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  Day = Date - Date[1] + as.numeric(DayofWeek[1]) # Calculate days
  Week = as.numeric(ceiling(Day/7))

  # Information about the curves
  info = data.frame(Date = Date, DayofWeek = DayofWeek, Week = Week) #Day = Day

  # Create a data frame
  if(na.missing & !smoothing)  {out = list(Date, DayofWeek, Week, out00_2)}
  if(!na.missing & !smoothing) {out = list(Date, DayofWeek, Week, out11_2)}
  if(!na.missing & smoothing)  {out = list(Date, DayofWeek, Week, out22)}

  class(out) = "data.frame"
  names(out) = c("Date", "DayofWeek", "Week", "Out")
  rownames(out) = 1:length(Date)

  return(list(tseq = tseq, out= out))
}

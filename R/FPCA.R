#'Run FPCA (daily / all combined)
#'
#' @param data 'data_grid' object
#' @param nbasis Number of basis. Defult is 12.
#'
#' @importFrom refund fpca.sc
#'
#' @export

FPCA = function(data, nbasis = 12)
{
  dayofweek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  dayofweek = factor(dayofweek,
                     level = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  # Each day of week separately
  FPCA_day = vector("list", 7)

  # There may be no curve for certain days of a week
  day.seq = (1:7)[dayofweek %in% data$DayofWeek]

  for(d in day.seq)
  {
    cat("FPCA: Curves on", as.character(dayofweek[d]), " \n")

    data.d = data$Out[data$DayofWeek == dayofweek[d],]
    FPCA_day[[d]] = fpca.sc(data.d, pve = 0.999, nbasis = nbasis, var = TRUE, simul = TRUE)
  }

  # All curves combined
  cat("FPCA: All curves \n")

  FPCA = fpca.sc(data$Out, pve = 0.999, nbasis = nbasis, var = TRUE, simul = TRUE)

  return(list(FPCA_day = FPCA_day, FPCA = FPCA))
}

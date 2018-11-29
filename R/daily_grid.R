#' Stretch derived (daily summary) measurements for multiple subjects (evaluate curves on a common grid)
#'
#' @param data Long data frame of daily measure. Output of function 'daily'. Data include columns named Subj, Date, and Measure.
#' @param ngrid Number of grid. Default is 200.
#'
#' @export
#'

daily_grid = function(data, ngrid = 200)
{
  Subj = unique(data$Subj)

  out = matrix(NA, nrow = length(Subj), ncol = ngrid)
  tseq = seq(0, 1, length = ngrid)
  for(s in 1:length(Subj))
  {
    subj.s = Subj[s]
    data.s = data[data$Subj == subj.s, ]

    tseq.s = seq(0, 1, length = nrow(data.s))
    fn.s = splinefun(tseq.s, data.s$Measure)
    out[s, ] = fn.s(tseq)
    out[s, ] = sapply(out[s, ], function(x){max(x, 0)}) # No minimum
  }
  rownames(out) = Subj
  return(out)
}

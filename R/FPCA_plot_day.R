#' Plot FPCA (Daily)
#'
#' @param fpca FPCA object: Specifically, output 'FPCA_day' of FPCA()
#' @param npc Number of FPCs to plot. Default is 2. Option for FPC plot
#' @param obj Objects for plotting. Default is FPCs. Possible options include FPCs and estimated curve.
#'
#' @export

FPCA_plot_day = function(fpca, npc = 2, obj = c("FPC", "EST"))
{
  dayofweek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  dayofweek = factor(dayofweek,
                     level = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  FPC = EST = vector("list", 7)

  # Plot PCA results: Daily
  for(d in 1:7)
  {
    if(!is.null(fpca[[d]])){

      FPCA.d = FPCA_plot(fpca[[d]], npc = npc, obj = obj)

      FPC[[d]] = FPCA.d$FPC
      EST[[d]] = FPCA.d$EST
    }
  }

#  plot.obj = list(FPC, EST)

  return(list(FPC = FPC, EST = EST))
}

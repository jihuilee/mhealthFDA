#' Plot FPCA (Overall: (0, 1) on x-axis)
#'
#' @param fpca FPCA object: Specifically, output 'FPCA' of FPCA()
#' @param npc Number of FPCs to plot. Default is 2.
#' @param obj Objects for plotting. Default is FPCs. Possible options include FPCs and estimated curve.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#'
#' @export

FPCA_plot = function(fpca, npc = 2, obj = c("FPC", "EST"))
{

  if (fpca$npc == 1){npc = 1} else {npc = npc}

  # tseq
  tseq = seq(0, 1, length = nrow(fpca$efunctions))

  plot.dat = data.frame(Time = as.hms(rep(tseq, npc)),
                        FPC = c(fpca$efunctions[,1:npc]),
                        nPC = as.factor(rep(1:npc, each = length(tseq))))

  plot.fpc = plot.est = NULL

  if("FPC" %in% obj)
  {
    # Proportion of variance
    prop = round(fpca$evalues * 100 / sum(fpca$evalues), 2)[1:npc]

    plot.fpc = ggplot() + geom_line(data = plot.dat, aes(x = Time, y = FPC, group = nPC, color = nPC)) +
      xlab("") + ylab("FPC") + theme_bw() +
      scale_color_manual(labels = c(paste("1st FPC: ", prop[1], "%", sep = ""), paste("2nd FPC: ", prop[2], "%", sep = "")),
                         values = c("red", "blue"), guide = guide_legend(title = NULL)) +
      theme(legend.text = element_text(size = 10), axis.text = element_text(size = 13), legend.position = c(0.1, 0.9), # legend.position = c(0.85, 0.9) / c(0.85, 0.9)
            axis.title = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 16))
  }

  if("EST" %in% obj)
  {
    ex = ceiling((1 + nrow(fpca$Yhat)) / 2) # Middle obs
    ex.mm = data.frame(fitted = fpca$Yhat[ex,],
                       ptwise.UB = fpca$Yhat[ex,] + 1.96 * sqrt(fpca$diag.var[ex,]),
                       ptwise.LB = fpca$Yhat[ex,] - 1.96 * sqrt(fpca$diag.var[ex,]),
                       #                     simul.UB = fpca$Yhat[ex,] + fpca$crit.val[ex] * sqrt(fpca$diag.var[ex,]),
                       #                     simul.LB = fpca$Yhat[ex,] - fpca$crit.val[ex] * sqrt(fpca$diag.var[ex,]),
                       time = tseq)

    plot.dat = melt(ex.mm, id = 'time')

    plot.est = ggplot(plot.dat, aes(x = time, y = value, group = variable, color = variable, linetype = variable)) + geom_path() +
      xlab("") + ylab("Estimated") + theme_bw() +
      scale_linetype_manual(values = c(fitted = 1, ptwise.UB = 2, ptwise.LB = 2)) +  # simul.UB = 3, simul.LB = 3)) +
      scale_color_manual(values = c(fitted = 1, ptwise.UB = 2, ptwise.LB = 2)) + # simul.UB = 3, simul.LB = 3))
      theme(legend.text = element_text(size = 10), axis.text = element_text(size = 13), legend.position = "NONE", # legend.position = c(0.05, 0.9)
            axis.title = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 16))
  }

  plot.obj = list(plot.fpc, plot.est)
  names(plot.obj) = c("FPC", "EST")
  plot.obj = plot.obj[names(plot.obj) %in% obj]

  return(plot.obj)
}

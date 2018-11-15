#' Create derived (daily summary) measurements for multiple subjects
#'
#' @param data List of data_grid outcomes
#' @param subj Subject name
#' @param plotting If TRUE, a ggplot object is produced.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 facet_wrap
#'
#' @export

daily = function(data, subj = NULL, plotting = TRUE)
{
  out = NULL

  if(is.null(subj) && is.null(names(data))){subj = 1:length(data)}
  if(is.null(subj) && !is.null(names(data))){subj = names(data)}

  grid = 10
  nummax = (floor(max(unlist(lapply(data, nrow))) / grid) + 1) * grid
  xgrid = seq(0, nummax, by = grid)
  xgrid2 = seq(0, nummax, by = grid/2)

  for(s in 1:length(data))
  {
    data.s = data[[s]]
    subj.s = subj[[s]]

    out.s = data.frame(Subj = subj.s, CurveNum = 1:nrow(data.s),
                       Date = data.s$Date, DayofWeek = data.s$DayofWeek, Week = data.s$Week,
                       Measure = apply(data.s$Out, 1, function(x){sum(x, na.rm = T)}))

    out = rbind(out, out.s)
  }

  if(plotting) {
    plot = ggplot() + geom_line(data = out, aes(x = CurveNum, y = Measure, color = as.factor(Subj), group = Subj)) +
                        theme_bw() + facet_wrap(.~Subj, ncol = 5, scales = "free_y") +
      scale_x_continuous(breaks = xgrid, labels = xgrid, minor_breaks =  xgrid2) +
    theme(legend.position = "none", strip.background = element_blank(), strip.text.y = element_blank()) + xlab("") + ylab("")
    return(list(out = out, plot = plot))
  } else{
    return(out)
  }
}

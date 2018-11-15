#'Plot the daily raw trajectory per individual
#'
#' @param data Data in a data.frame format; output of function 'data_grid'
#'
#' @importFrom hms hms
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 geom_text
#'
#' @export

raw_plot = function(data)
{
  out = data$Out
  time = as.hms(seq(hms(00, 00, 00), hms(00, 00, 24), length = ncol(out)+1))[-(ncol(out)+1)]
  plot.data = data.frame(time = rep(time, nrow(out)),
                         date = rep(data$Date, each = ncol(out)),
                         dayofweek = rep(data$DayofWeek, each = ncol(out)),
                         week = rep(data$Week, each = ncol(out)),
                         var = c(t(out)))

  dayofweek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  dayofweek = factor(dayofweek,
                     level = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  label.data1 = data[,1:3]
  label.data1$label = interaction(label.data1$DayofWeek, label.data1$Week)

  label.data2 = expand.grid(dayofweek = dayofweek, week = 1:max(plot.data$week))
  label.data2$label = interaction(label.data2$dayofweek, label.data2$week)

  label.data = merge(label.data1, label.data2, by = "label", all.y = TRUE)

  print(ggplot(data = plot.data) + geom_line(aes(x = time, y = var, color = dayofweek, group = date)) +
          theme_bw() + facet_grid(week ~ dayofweek, scales = "free_y") + xlab("") + ylab("") +
          scale_x_continuous(breaks = seq(hms(0, 0, 0), hms(00, 00, 24), length = 9),
                             labels = substr(as.hms(seq(hms(0, 0, 0), hms(00, 00, 24), length = 9)), 1, 5),
                             minor_breaks =  seq(hms(0, 0, 0), hms(00, 00, 24), length = 25)) +
          geom_text(size = 3, data = label.data, mapping = aes(x = hms(00, 00, 9), y = Inf, label = Date),
                    hjust = 1.05, vjust = 1.5) +
          theme(legend.position = "none", axis.text.x = element_text(angle = 30),
                strip.background = element_blank(), strip.text.y = element_blank()))
}

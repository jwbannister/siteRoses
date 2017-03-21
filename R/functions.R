#' Create directional rose ggplot object
#' 
#' @import ggplot2
#' @import RColorBrewer
#' @param data Data frame.  
#' @param value Text string. Column name of value variable.
#' @param dir Text string. Column name of direction variable.
#' @param valuemin Numeric. Lower value limit. All rows with value < valuemin 
#' are removed from data.
#' @param valueseq Numberic. Bin cutoffs for values.
#' @param palette Text string. Name of ColorBrewer palette to use for value 
#' fill.
#' @param legend.title Text string.
#' @param plot.title Text string.
#' @param revers.bars Boolean. Should display order of bar factors be reversed?
#' @return Returns the plot as a ggplot object. Paddles lengths (total and fill)
#' are proportional to percentage of total.
#' @examples
#' plot_rose(datafile, value='sand_flux', dir='resultant_wd_10m',
#'           valuemin=low.cutoff, legend.title="Sand Flux",
#'           plot.title="Sand Roses", reverse.bars=F)
plot_rose <- function(data,
                      value,
                      dir,
                      dirres = 22.5,
                      valuemin = 0,
                      valueseq,
                      palette = "YlOrRd",
                      legend.title = "",
                      plot.title = "",
                      ylabel.loc="NW", 
                      reverse.bars=F){

  data <- data[data[[value]]>valuemin, ] 
  ifelse(missing(valueseq),
    valueseq <- round(seq(valuemin, max(data[[value]]), 
                    (max(data[[value]]) - valuemin) / 5), 0)[1:5],
    valueseq <- c(valuemin, valueseq))
  # get some information about the number of bins, etc.
  n.value.seq <- length(valueseq)
  n.colors.in.range <- n.value.seq 
  # create the color map
  value.colors <- 
    colorRampPalette(RColorBrewer::brewer.pal(min(max(3, n.colors.in.range),
                                                  min(9, n.colors.in.range)), 
                                              palette))(n.colors.in.range)
  value.breaks <- c(valueseq, max(data[[value]]) + 1)
  value.labels <- c(paste0(c(valueseq[1:n.value.seq-1]), '-',
                          c(valueseq[2:n.value.seq]), "  "), 
                    paste0(valueseq[n.value.seq], "+"))
  data$value.binned <- cut(x = data[[value]],
                           breaks = value.breaks,
                           labels = value.labels,
                           ordered_result = TRUE)
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
 # summarize data
  data_sum <- data %>% dplyr::group_by(dir.binned, value.binned) %>%
    summarize(value.prcnt = 100 * length(value.binned) / nrow(data))
  prcnt_sums <- data_sum %>% group_by(dir.binned) %>%
    summarize(total.prcnt = sum(value.prcnt))
 # reverse order of bar factors if desired
  if (reverse.bars){
      value.colors <- rev(value.colors)
      data_sum$value.binned <- factor(data_sum$value.binned, 
                                      levels=rev(levels(data_sum$value.binned)),
                                      ordered=T)
  }
  # y-axis labels data frame
  b <- data.frame(label = c('5%', "10%", "15%", "20%", "25%", "30%", "35%", 
                            "40%", "45%", "50%", "55%", "60%", "65%", "70%",
                            "75%", "80%", "85%", "90%", "95%", "100%"), 
                  value.prcnt = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55,
                                  60, 65, 70, 75, 80, 85, 90, 95, 100),
                  dir.binned = rep(ylabel.loc, 20))
  ind <- which(b$value.prcnt==mround(max(prcnt_sums$total.prcnt), 5))
  b <- b[1:ind, ]
  # create the plot ----
  p.rose <- ggplot(data = data_sum,
                   aes(x = dir.binned, y = value.prcnt)) +
geom_bar(aes(fill=value.binned), stat='identity', color="black") + 
scale_x_discrete(drop = FALSE,
                 labels = waiver()) +
scale_y_continuous(breaks=b$value.prcnt, labels = waiver()) +
coord_polar(start = -((dirres/2)/360) * 2*pi) +
scale_fill_manual(name = legend.title, 
                  values = value.colors,
                  drop = FALSE) +
theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
ggtitle(plot.title) +
geom_text(data=b, aes(label=label), size=3)

return(p.rose)
}

# query of AWS owenslake database
query_owens <- function(query){
  usr <- readLines("./secure_data/airsci_db_cred.txt")[3]
  psswrd <- readLines("./secure_data/airsci_db_cred.txt")[4]
  hst <- "airdbo1.cwxikzzkese5.us-west-2.rds.amazonaws.com"
  prt <- "5432"
  con <- RPostgreSQL::dbConnect("PostgreSQL", host=hst, port=prt, 
                                dbname="owenslake", user=usr, password=psswrd)
  dat <- RPostgreSQL::dbGetQuery(con, query)
  RPostgreSQL::dbDisconnect(con)
  dat
}

mround <- function(x, base){
  base * round(x/base)
}

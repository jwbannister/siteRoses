load_all()
library(lubridate)
library(tidyverse)

cl_args <- commandArgs(trailingOnly=TRUE)
site <- cl_args[1]
day <- mdy(cl_args[2])

query1 <- paste0("SELECT s.datetime, i.deployment AS csc, s.sand_flux, s.ws_10m AS ws,  ", 
                 "COALESCE(wd_10m, resultant_wd_10m) AS wd ", 
                 "FROM sandcatch.sandflux_5min s JOIN instruments.deployments i ", 
                 "ON s.csc_deployment_id=i.deployment_id ",
                 "WHERE i.deployment='", site, "' ",
                 "AND (s.datetime-'1 second'::interval)::date", 
                 "='", day, "'::date ") 
sand_df <- query_owens(query1)
if (nrow(sand_df)==0) {
    stop(paste0("No observed sand motion at site ", site, " on ", 
                format(day, "%m/%d/%Y")))
}

dirres <- 22.5
# figure out the wind direction bins
dir.breaks <- c(-dirres/2,
              seq(dirres/2, 360-dirres/2, by = dirres),
              360+dirres/2)  
dir.labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
              "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
# assign each wind direction to a bin
dir.binned <- cut(sand_df$wd, 
                breaks = dir.breaks,
                ordered_result = TRUE)
levels(dir.binned) <- dir.labels
sand_df$dir.binned <- dir.binned

sand_summary <- sand_df %>% group_by(dir.binned) %>%
    summarize(flux=round(sum(sand_flux), 3)) %>% filter(!is.na(dir.binned))
y_max <- max(sand_summary$flux) * 1.1
y_breaks <- round(seq(0, y_max, y_max/4), 1)[-1]
y_labels <- data.frame(dir.binned=rep("NW", length(y_breaks)), flux=y_breaks, 
                       label=as.character(y_breaks))

p.rose <- ggplot(data = sand_summary, aes(x = dir.binned, y = flux,)) +
coord_polar(start = -((dirres/2)/360) * 2*pi) +
geom_bar(stat='identity', color="black", fill="lightblue") + 
scale_x_discrete(drop = FALSE,
                 labels = waiver()) +
scale_y_continuous(breaks=y_breaks) +
geom_text(data=y_labels, mapping=aes(label=label)) + 
theme(axis.title.x = element_blank(), 
      plot.title=element_text(hjust=0.5), 
      axis.text.y = element_blank()) + 
ylab(expression(paste("Total Sand Flux (g/c", m^2, ")"))) + 
ggtitle(paste0("Sand Flux at Site ", site, " on ", format(day, "%m/%d/%Y"))) 

file_name <- paste0(site, "_sandrose_", format(day, "%m%d%y"), ".jpg")
jpeg(paste0(tempdir(), "/", file_name), width=6, height=6, units="in", res=300)
p.rose
dev.off()
system(paste0("~/sh/dropbox_uploader.sh upload ", 
              paste0(tempdir(), "/", file_name), " ", file_name))


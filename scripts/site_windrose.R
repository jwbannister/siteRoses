load_all()
library(dplyr)
library(ggplot2)
library(lubridate)

cl_args <- commandArgs(trailingOnly=TRUE)
day <- mdy(cl_args[1])

query1 <- paste0("SELECT i.deployment, t.datetime, t.ws_wvc AS ws, ",
                 "t.wd_wvc AS wd ",
                 "FROM teom.teom_analog_1hour t ",
                 "JOIN instruments.deployments i ",
                 "ON t.deployment_id=i.deployment_id ",
                 "WHERE (t.datetime-'1 second'::interval)::date", 
                 "='", day, "'::date ") 
a <- query_owens(query1)

query3 <- paste0("SELECT DISTINCT i.deployment, ", 
                 "m.datetime, m.ws_10m, m.wd_10m ",
                 "FROM instruments.deployments i ",
                 "JOIN met.met_1hour m ON i.deployment_id=m.deployment_id ",
                 "WHERE (m.datetime-'1 second'::interval)::date", 
                 "='", day, "'::date ") 
b <- query_owens(query3) %>% rename(ws=ws_10m, wd=wd_10m)
df1 <- rbind(a, b)  %>% filter(ws>-9999 & ws<100) %>% filter(wd<999)
cat(paste0("Enter Station (", 
                 paste(unique(df1$deployment), collapse=", "), "):"))
station <- readLines(con='stdin', 1)
df2 <- filter(df1, deployment==station)

dirres <- 22.5
# figure out the wind direction bins
dir.breaks <- c(-dirres/2,
              seq(dirres/2, 360-dirres/2, by = dirres),
              360+dirres/2)  
dir.labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
              "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
# assign each wind direction to a bin
dir.binned <- cut(df2$wd,
                breaks = dir.breaks,
                ordered_result = TRUE)
levels(dir.binned) <- dir.labels
df2$dir.binned <- dir.binned

title <- paste0(station, " ", format(day, "%m-%d-%Y"))
legend.title <- "Wind Speed (m/s)"
p.rose <- plot_rose(df2, value='ws', dir='wd', plot.title=title, 
                    legend.title=legend.title, reverse.bars=T)
file_name <- paste0(station, "_windrose_", format(day, "%m%d%y"), ".jpg")
jpeg(paste0(tempdir(), "/", file_name), width=6, height=6, units="in", res=300)
p.rose
dev.off()
system(paste0("~/sh/dropbox_uploader.sh upload ", 
              paste0(tempdir(), "/", file_name), " ", file_name))

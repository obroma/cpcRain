# author: Gopi Goteti
# analyze precipitation data from the Climate Prediction Center (CPC)

source("cpc_lib.R")

library(ggplot2)
library(reshape2)
library(gridExtra) # for plotting multiple panels within a page

#-------------------------------------------------------------------------------
# hurricane Sandy, 2012/10/25, entire world
yr  <- 2012
mo  <- 10
day <- 22
# get raw data from CPC
Fn_Download_CPC_Data_OneDay(yr, mo, day) 
# convert raw data to binary
sandy <- Fn_Read_CPC_RawData(yr, mo, day) 
# re-orient data for plotting
sandy <- Fn_Rotate_Matrix(Fn_Flip_Matrix_Rows(sandy))
# plot data
plot1 <- ggplot(data = melt(sandy)) + 
  geom_raster(aes(Var1, Var2, fill = value)) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
  labs(x = NULL, y = NULL, fill = "Rain/Snow (mm/day)") + 
  ggtitle(paste0("Hurricane Sandy: ", as.Date(paste(yr, mo, day, sep = "-"))))
# save plot
ggsave(plot1, file="sandy1.png", width = 10, height = 8)

#-------------------------------------------------------------------------------
# hurricane Sandy, 2012, entire duration 2012/10/22-31, entire world
# get raw data from CPC
Fn_Download_CPC_Data_ManyDays(yr, mo, 22, yr, mo, 31)
# process and plot rain on 4 select days
days  <- c(28, 29, 30, 31)
plot4 <- list() # store ggplot output
for (eachDay in 1:length(days)) {
  # convert raw data to binary
  sandy <- Fn_Read_CPC_RawData(yr, mo, days[eachDay])
  # re-orient data for plotting
  sandy <- Fn_Rotate_Matrix(Fn_Flip_Matrix_Rows(sandy))
  # plot data
  plot4[[eachDay]] <- ggplot(data = melt(sandy)) + 
    geom_raster(aes(Var1, Var2, fill = value)) + 
    theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(x = NULL, y = NULL, fill = "Rain/Snow (mm/day)") + 
    ggtitle(as.Date(paste(yr, mo, days[eachDay], sep = "-")))
}
# save plot
png("sandy2.png", width=10, height = 8, units = "in", res = 72)
grid.arrange(arrangeGrob(plot4[[1]], plot4[[2]], plot4[[3]], plot4[[4]], 
                         ncol = 2, 
                         main = textGrob("Hurricane Sandy", 
                                         vjust = 1, 
                                         gp = gpar(fontface = "bold", cex = 1.5))))
garbage <- dev.off()


#-------------------------------------------------------------------------------
# hurricane Sandy, 2012, entire duration 2012/10/22-31, eastern USA
# lat-lon bounds of eastern US
maxLat <- 48.0
minLat <- 23.0
maxLon <- -70.0
minLon <- -90.0

# US state boundaries used by ggplot
boundaryState <- map_data("state")

# process and plot rain on 4 select days
days  <- c(28, 29, 30, 31)
plot4 <- list() # store ggplot output
for (eachDay in 1:length(days)) {
  # convert raw data to binary
  sandy <- Fn_Read_CPC_RawData(yr, mo, days[eachDay]) 
  # extract regional data
  sandy <- Fn_Get_Region_Data_For_Plot(sandy, maxLat, minLat, maxLon, minLon)
  #plot data
  plot4[[eachDay]] <- ggplot(data = sandy) + 
    geom_raster(aes(lon, lat, fill = colorScale)) +
    scale_fill_brewer(palette = "BuPu", drop = FALSE) +
    xlim(c(minLon, maxLon)) + 
    ylim(c(minLat, maxLat)) + 
    geom_path(data = boundaryState, aes(x = long, y = lat, group = group), na.rm = TRUE) +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(x = NULL, y = NULL, fill = "Rain/Snow (mm/day)") + 
    ggtitle(as.Date(paste(yr, mo, days[eachDay], sep="-")))
  
}
# save plot
png("sandy3.png", width=10, height = 8, units = "in", res = 72)
grid.arrange(arrangeGrob(plot4[[1]], plot4[[2]], plot4[[3]], plot4[[4]], 
                         ncol = 2, 
                         main = textGrob("Hurricane Sandy", 
                                         vjust = 1, 
                                         gp = gpar(fontface = "bold", cex = 1.5))))
garbage <- dev.off()

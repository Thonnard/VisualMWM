## load some data
load("track_data.RData")

## groupCOURD
groupCOORD(data=track_data, by="Group")
groupCOORD(data=track_data, by=c("Group", "Day"))
groupCOORD(data=track_data, by=c("Group", "Day", "Trial"))
groupCOORD(data=track_data, by=c("Day", "Trial"))

## updatecOORD
data <- updateCOORD(data=track_data, centerx=19.4, centery=-1.4, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5, removeNA=TRUE)

## mergeCOORD
setwd("tests/CSV_merge")
mergeCOORD(startData=39,rowID=34, rowDay=35, rowTrial=32)
mergeCOORD(startData=38,rowID=33, rowDay=32, rowTrial=34, rowGroup=22)

##
areaQuadrantTime(data=track_data, id="1w", day=1, trial=1,
                 centerx=19.4, centery=-1.4, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5,
                 device="tiff", width=18, height=12, dpi=300, units="cm", viridis_color_palette = "D")

areaQuadrantTime(data=track_data, id="1w", day=1, trial=4,
                 centerx=19.4, centery=-1.4, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5,
                 width=18, height=12, dpi=300, units="cm", viridis_color_palette = "E",
                 theme_settings =list(axis.text = element_text(face="bold", color="#993333", size=12),
                                      axis.title = element_text(face="bold", color="black",size=14),
                                      legend.position="none"))
## barGIF
barGIF(data=track_data, id="1w", day=1, trial=1,
       centerx=19.4, centery=-1.4, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5,
       removeSwimspeedOutliers = TRUE, SwimspeedLimit = 50,
       loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50)

barGIF(data=track_data, id="1w", day=1, trial=1,
       centerx=19.4, centery=-1.4, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5,
       removeSwimspeedOutliers = TRUE, SwimspeedLimit = 50,
       loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50,
       theme_settings =list(axis.text.x = element_text(face="bold", color="#993333", size=16)))

## simple circle
circle(x=0, y=0, radius=75, nrow_data=100, from=0, to=2, add_center=FALSE)

## trackGIF
trackGIF(data=track_data, id="1w", day=1, trial=1,
         centerx=19.4, centery=-1.49, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5, ndata_circle=100,
         quadrant_colours=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), platform_colour="black" , alpha_quadrants=0.3, alpha_platform=1,
         track_colour="orange", alpha_track=0.35,
         loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50)

trackGIF(data=track_data, id="1w", day=1, trial=1,
         centerx=19.4, centery=-1.49, radius=75, platformx=50.60, platformy=-33.34, platformradius=7.5, ndata_circle=100,
         quadrant_colours=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), platform_colour="black" , alpha_quadrants=0.3, alpha_platform=1,
         track_colour="orange", alpha_track=0.35,
         loop = FALSE, width = 480, height = 480, fps = 10, duration = 10, frames = 100, time_bins = 50,
         theme_settings =list(axis.text.x = element_text(face="bold", color="#993333", size=16),
                              axis.text.y = element_text(face="bold", color="blue", size=16)))

## play around with circles and quadrants
# parameter
radius=75
# circle data
maze <- circle(x=0, y=0, radius=radius, nrow_data=1000, from=0, to=2, add_center=FALSE)
# quadrants data
top_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=100, from=0, to=0.5, add_center=TRUE)
bottom_right_quadrant <- circle(x=0, y=0, radius=radius, nrow_data=100, from=0, to=-0.5, add_center=TRUE)
top_left_quadrant <- -bottom_right_quadrant
bottom_left_quadrant <- -top_right_quadrant
# quadrants outside data
outside_TR <- subset(top_right_quadrant, x!=0 & y!=0)
outside_TL <- subset(top_left_quadrant, x!=0 & y!=0)
outside_BR <- subset(bottom_right_quadrant, x!=0 & y!=0)
outside_BL <- subset(bottom_left_quadrant, x!=0 & y!=0)
outside_TR <- rbind(outside_TR,c(0,radius), c(0,radius+radius/10), c(radius+radius/10,radius+radius/10),c(radius+radius/10,0), c(radius,0))
outside_TL <- rbind(outside_TL,c(0,radius), c(0,radius+radius/10), c(-radius-radius/10,radius+radius/10),c(-radius-radius/10,0), c(-radius,0))
outside_BR <- rbind(outside_BR, c(0,-radius), c(0,-radius-radius/10), c(radius+radius/10,-radius-radius/10), c(radius+radius/10,0), c(radius,0))
outside_BL <- rbind(outside_BL, c(0,-radius), c(0,-radius-radius/10,0), c(-radius-radius/10,-radius-radius/10), c(-radius-radius/10,0), c(-radius,0))
# outside circle data
df <- data.frame(x=c(-radius,-radius-radius/10,-radius-radius/10,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,-radius),
                 y=c(0,0,radius+radius/10,radius+radius/10,-radius-radius/10,-radius-radius/10,0,0))
outside_all <- rbind(maze,df)
# plot
ggplot() +
  # plot quadrants
  # geom_polygon(data=top_right_quadrant, aes(x,y), color="black", fill="blue", alpha=1) +
  # geom_polygon(data=bottom_right_quadrant, aes(x,y), color="black", fill="yellow", alpha=1) +
  # geom_polygon(data=top_left_quadrant, aes(x,y), color="black", fill="green", alpha=1) +
  # geom_polygon(data=bottom_left_quadrant, aes(x,y), color="black", fill="red", alpha=1) +
  # plot quadrants outside
  # geom_polygon(data=outside_TR, aes(x,y), color="black", fill="orange", alpha=1) +
  # geom_polygon(data=outside_TL, aes(x,y), color="black", fill="grey", alpha=1) +
  # geom_polygon(data=outside_BR, aes(x,y), color="black", fill="grey", alpha=1) +
  # geom_polygon(data=outside_BL, aes(x,y), color="black", fill="orange", alpha=1) +
  # plot outside all, cave: set color=NA or otherwise you see the closing line through the circle
  geom_polygon(data=outside_all, aes(x,y), color=NA, fill="pink", alpha=0.4) +
  # plot empty circle
  geom_polygon(data=maze, aes(x,y), color="black", fill=NA, alpha=1) +
  # plot quadrant division
  geom_segment(aes(x=-radius,xend=radius,y=0,yend=0),linetype=2) +
  geom_segment(aes(x=0,xend=0,y=-radius,yend=radius),linetype=2) +
  # theme + fixed coord
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.title.y=element_blank()) +
  coord_fixed()

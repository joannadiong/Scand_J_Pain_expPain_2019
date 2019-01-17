# Remove comments and run following lines if packages are not installed
#install.packages("reshape2")
#install.packages("ggplot2") 

# Load libraries
library(reshape2)
library(ggplot2)
library(plyr)

# Set current working directory
setwd('/home/joanna/Dropbox/Projects/shoulderPain/doc/ms/S1 File')

# Read in CSV file, assign values to dataframe `df`
df <- read.csv('160412_nrs.csv', header=TRUE, sep=',')

# Reshape dataframe with melt() so data are in long format
df <- melt(df, id.vars=c('subject', 'site'))

# Rename long format identifier as `time`
names(df)[names(df)=="variable"] <- "time"

# Rename levels of factors of `time` variable
levels(df$time)[levels(df$time) == c("X1", "X2", "X3", "X4", "X5", "X6", 
                                     "X7", "X8", "X9", "X10", "X11", "X12"
                                     )] <- c("1", "2", "3", "4", "5", "6", 
                                             "7", "8", "9", "10", "11", "12")

# Change factor variables (time) to continuous
as.numeric(levels(df$time))[df$time]

# Get mean, sd of pain for each site at each time point
ds <- ddply(df, na.rm=T, .(time, site), summarise, mean=mean(value), sd=sd(value), 
            median=median(value,na.rm=T), qt25=quantile(value,0.25,type=6,na.rm=T), qt75=quantile(value,0.75,type=6,na.rm=T))

# Plot raw pain scores on time for each subject, add jitter to x-axis values
ggplot(data=df, aes(y=df$value, x=df$time)) + 
  # set x- and y-axis labels
  labs(x="Time (minute)", y="Pain score") +
  # set colours for `subacromial`, `supraspinatus` factor levels
  scale_colour_manual(name="", values=c("cyan", "grey55", "blue", "black")) +
  # define geometry: plot each values as a point, set plot aesthetics and jitter
  geom_point(aes(x=df$time, colour=df$site), size=3, position=position_jitter(width=0.2,height=0)) +
  # set y-axis ticks
  scale_y_discrete(breaks=c("0", "1", "2", "3", "4", "5", "6",  "7", "8", "9")) + 
  # set theme as black-white
  theme_bw() + 
  # set parameters of theme
  theme(
    axis.text=element_text(size=14),
    legend.text=element_text(size=14),
    # position legend at 90% of x- and y-axis length
    legend.position=c(0.9, 0.9),
    legend.key=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
  ) 

rep = replicate(12, c("cyan", "grey55", "blue", "black"))

# Plot raw and summary data of pain scores
set.seed(1)
legend_point_fill <- c("white", "gray55", "white", "black")
geom_point_fill <- c("gray55", "gray55", "black", "black")
fig <- ggplot() + 
  # raw data
  labs(x="Time (minute)", y="Pain score") +
  geom_point(data=df, aes(y=value, x=time, colour=df$site, fill=df$site, shape=df$site), size=2, position=position_jitter(width=0.4,height=0)) +
  scale_shape_manual(values=c(22, 22, 21, 21), guide=FALSE) + 
  scale_colour_manual(name="", values=geom_point_fill) +
  scale_fill_manual(values=legend_point_fill) + 
  guides(fill=FALSE) +
  coord_cartesian(ylim=c(-0.1, 10)) + 
  scale_y_discrete(breaks=c("0", "1", "2", "3", "4", "5", "6",  "7", "8", "9", "10")) + 
  theme_bw() + 
  theme(
    axis.text=element_text(size=14),
    legend.text=element_text(size=14),
    # position legend at 80% of x- and y-axis length
    legend.position=c(0.85, 0.85),
    legend.title=element_blank(),
    legend.key=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(), 
    text=element_text(size=18)) + 
  # summary data
  # geom_errorbar(data=ds, aes(x=time, y=mean, ymin=mean-sd, ymax=mean+sd, color=ds$site), position=position_dodge(width=0.4)) + 
  geom_errorbar(data=ds, aes(x=time, y=median, ymin=qt25, ymax=qt75, color=ds$site), position=position_dodge(width=0.4)) + 
  geom_line(data=ds, aes(x=time, y=mean, group=site, colour=ds$site), position=position_dodge(width=0.4)) +
  geom_point(data=ds, aes(x=time, y=mean, colour=ds$site, fill=ds$site, shape=ds$site), size=4, position=position_dodge(width=0.4)) + 
  scale_color_manual(labels = c("Isotonic subacromial", "Isotonic supraspinatus", "Subacromial", "Supraspinatus"), 
                     values=geom_point_fill) + 
  scale_shape_manual(values=c(22, 22, 21, 21), guide=FALSE) + 
  scale_fill_manual(values=legend_point_fill) +
  guides(col=guide_legend(override.aes=list(shape=c(22, 22, 21, 21), fill=legend_point_fill)))
print(fig)
# save figure
png(filename="Fig 3.png", width=11, height=7, units='in', res=300)
plot(fig)
dev.off()

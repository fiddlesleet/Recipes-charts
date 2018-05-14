library(ggplot2)
library(gridExtra)
set.seed(10005)


## rather than stacking histograms, best to compare frequency polygons
# stacked hist 
ggplot(diamonds, aes(price, fill=cut)) + 
    geom_histogram(binwidth = 500)
# freq_polygons
ggplot(diamonds, aes(price, color=cut)) + 
    geom_freqpoly(binwidth=500)
# to make it easier to compare dists. with very diffferent counts, use density on y axis
ggplot(diamonds, aes(x = price, y = ..density.., color = cut)) + 
    geom_freqpoly(binwidth=500)



### using ggplot2movies
library(ggplot2movies)
m <- ggplot(movies, aes(rating))
m + geom_histogram(binwidth = .1)

# weighted by votes var
m + geom_histogram(aes(weight = votes), binwidth=.1) + 
    ylab('votes')

# The bins have constant width on the transformed scale.
m + geom_histogram() + scale_x_log10()
m + geom_histogram(binwidth = 0.05) + scale_x_log10()




###########
xvar <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
yvar <- c(rnorm(1500, mean = 1), rnorm(1500, mean=1.5))
zvar <- as.factor(c(rep(1,1500), rep(2,1500)))
xyz <- data.frame(xvar, yvar,zvar)

### histograms
# counts on y-axis
g <- ggplot(xyz, aes(x = xvar)) + 
    geom_histogram()
# ugly default
g2 <- ggplot(xyz, aes(x = xvar)) + 
    geom_histogram(binwidth = 1)
# change binwidth, make nicer looking
g3 <- ggplot(xyz, aes(x = xvar)) + 
    geom_histogram(fill=NA, color="black") + 
    theme_bw()
# density on y axis
g4 <- ggplot(xyz, aes(x=xvar)) + 
    geom_histogram(aes(y=..density..), color="black", fill=NA) +
    theme_bw()

## arrange all in grid
grid.arrange(g,g2,g3,g4)


### density plots
# basic density plot
d <- ggplot(xyz, aes(x = xvar)) + 
    geom_density()

# histogram with density line overlaid
d2 <- ggplot(xyz, aes(x = xvar)) + 
    geom_histogram(aes(y = ..density..), color="red", fill=NA) +
    geom_density(color="blue")

# split and color by 3rd var; alpha fades color a bit
## useful for splitting densities by gender
d3 <- ggplot(xyz, aes(x = xvar, fill = zvar)) + 
    geom_density(alpha = .2)

# plot the density charts
grid.arrange(d,d2,d3, nrow=1)

### boxplots
# basic boxplot
b <- ggplot(xyz, aes( zvar, xvar)) + 
    geom_boxplot(aes(fill=zvar)) + 
    theme(legend.position = "none")

# jitter plot
b2 <- ggplot(xyz, aes(zvar, xvar)) + 
    geom_jitter(alpha=I(.25), aes(color=zvar)) + 
    theme(legend.position = "none")

# volcano plot
b3 <- ggplot(xyz, aes(x = xvar)) + 
    stat_density(aes(ymax = ..density.., ymin = -..density.., 
                     fill = zvar,
                     color = zvar),
                 geom = "ribbon",
                 position = "identity") +
    facet_grid(. ~zvar) +
    coord_flip() +
    theme(legend.position = "none")

grid.arrange(b,b2,b3)


### combine multiple plots

# rug plot: shows density on scatter plot
ggplot(xyz, aes(xvar, yvar)) + 
    geom_point() + 
    geom_rug(col='darkred', alpha=.1)

#placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
    )

#scatterplot of x and y variables
scatter <- ggplot(xyz,aes(xvar, yvar)) + 
    geom_point(aes(color=zvar)) + 
    scale_color_manual(values = c("orange", "purple")) + 
    theme(legend.position=c(1,1),legend.justification=c(1,1)) 

#marginal density of x - plot on top
plot_top <- ggplot(xyz, aes(xvar, fill=zvar)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c("orange", "purple")) + 
    theme(legend.position = "none")

#marginal density of y - plot on the right
plot_right <- ggplot(xyz, aes(yvar, fill=zvar)) + 
    geom_density(alpha=.5) + 
    coord_flip() + 
    scale_fill_manual(values = c("orange", "purple")) + 
    theme(legend.position = "none") 

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

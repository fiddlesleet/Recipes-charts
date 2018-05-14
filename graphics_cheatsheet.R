# included: heatmap, correlogram, etc

setwd('/Users/hannahsmythe/Desktop/To_Publish_R/Replications/distribution_viz/')
library(readr)
library(ggplot2)
# read in data
df <- read_csv('Train_UWu5bXk.csv')

# look at data
str(df)

# check var classes
sapply(df, class)


#### SCATTER PLOTS: relationship bt 2 continuous quant vars, or 2 + a categorical var
# scatterplot: relationship bt 2 continuous vars
ggplot(df, aes(x = Item_Visibility, y = Item_MRP)) + 
    geom_point() +
    scale_x_continuous("Item Visibility", breaks = seq(0, 0.35, 0.05)) +
    scale_y_continuous("Item MRP", breaks = seq(0,270, by=30)) +
    theme_bw()

# scatterplot: relationship bt 2 continuous vars, and add in a categorical var
ggplot(df, aes(x = Item_Visibility, y = Item_MRP)) + 
    geom_point(aes(color=Item_Type)) +
    scale_x_continuous("Item Visibility", breaks = seq(0, 0.35, 0.05)) +
    scale_y_continuous("Item MRP", breaks = seq(0,270, by=30)) +
    theme_bw() + 
    labs(title="Scatterplot")

# use facet wrap to create sep. scatter plot by Item_Type
ggplot(df, aes(x = Item_Visibility, y = Item_MRP)) + 
    geom_point(aes(color=Item_Type)) +
    scale_x_continuous("Item Visibility", breaks = seq(0, 0.35, 0.05)) +
    scale_y_continuous("Item MRP", breaks = seq(0,270, by=30)) +
    theme_bw() + 
    labs(title="Scatterplot") + 
    facet_wrap(~Item_Type)

### HISTOGRAMS: 1 continuous, quant var
ggplot(df, aes(Item_MRP)) + 
    geom_histogram(binwidth=2) + 
    scale_x_continuous("Item MRP", breaks = seq(0,270, by=30)) + 
    scale_y_continuous("Count", breaks = seq(0,200, by=20)) + 
    labs(title="Histogram")

###  BAR CHART: CATEGORICAL VARS, or combo of Continuous and cat vars

# bar chart with 1 var
ggplot(df, aes(x = Outlet_Establishment_Year)) + 
    geom_bar(fill="red") +
    theme_bw() + 
    scale_x_continuous("Establishment Year", breaks=seq(1985, 2010)) + # all years
    scale_y_continuous("Count", breaks=seq(0,1500, 150)) + 
    coord_flip() + # remove to get this chart vertically
    labs(title = "Bar Chart") +
    theme_gray()

# remove coord_flip to get this chart vertically
ggplot(df, aes(x = Outlet_Establishment_Year)) + 
    geom_bar(fill="red") +
    theme_bw() + 
    scale_x_continuous("Establishment Year", breaks=seq(1985, 2010)) + # all years
    scale_y_continuous("Count", breaks=seq(0,1500, 150)) + 
    # coord_flip() + # remove to get this chart vertically
    labs(title = "Bar Chart") +
    theme_gray()

# bar chart with 2 variables
ggplot(df, aes(x = Item_Type, y = Item_Weight)) + 
    geom_bar(stat="identity", fill="darkblue") + 
    scale_x_discrete("Outlet Type") + # for categoricald ata
    scale_y_continuous("Item Weight", breaks=seq(0,15000, by=500)) + 
    theme(axis.text.x = element_text(angle=90, vjust = .5)) + 
    labs(title = "Bar Chart ")

# stack bar chart: combo of continuous vars
ggplot(df, aes(Outlet_Location_Type, fill=Outlet_Type)) + 
    geom_bar() + 
    labs(title = "Stacked Bar Chart", x = "Outlet Location Type", 
         y = "Count of Outlets")

### BOXPLOT : categorical and contin vars; to id dist. and detect outliers
ggplot(df, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
    geom_boxplot(fill = "red") + 
    scale_y_continuous("Item OUtlet Sales", breaks=seq(0,15000, by=500)) + 
    labs(title = "Box Plot", x = "Outlet Identifier")

### AREA CHART: like a line chart; often used for time series plots or for
###     continuous vars

ggplot(df, aes(Item_Outlet_Sales)) + 
    geom_area(stat = "bin", bins = 30, fill = "steelblue") +  
    scale_x_continuous(breaks = seq(0,11000, 1000)) + 
    labs(title = "Area Chart", x = "Item Outlet Sales", y = "Count")

### Heat Map 
ggplot(df, aes(x=Outlet_Identifier, y=Item_Type)) + 
    geom_raster(aes(fill = Item_MRP)) + 
    labs(title="Heat Map", x = "Outlet Identifier", y = "Item Type") + 
    scale_fill_continuous(name="Item MRP")

### Correlogram: test corr between vars
# install.packages("corrgram")
library(corrgram) # easier to use than ggplot2 for this

#correlogram: blue is + correlation, red is -
corrgram(df, order=NULL,
         panel = panel.shade, 
         text.panel = panel.txt,
         main="Correlogram")

### geographical map
#install.packages('ggmap')
library(ggmap)
#plot the countries participating in ICC World T20 World Cup 2016 (16 paises)
#List of Countries
ICC_WC_T20 <- c("Australia",
                  "WestIndies",
                  "India",
                  "SriLanka",
                  "Pakistan",
                  "Bangladesh",
                  "NewZealand",
                  "SouthAfrica",
                  "England",
                  "HongKong",
                  "Zimbabwe",
                  "Afghanistan",
                  "Scotland",
                  "Netherlands",
                  "Ireland",
                  "Oman")

#extract geo location of these countries
countries <- geocode(ICC_WC_T20)

# map long, lat in sep. vars
nations.x <- countries$lon
nations.y <- countries$lat

# plot world map with ggplot2
map <- borders('world', color='lightblue', fill='lightgreen')
# add pts to map
ggplot() + 
    map + 
    geom_point(aes(x=nations.x, y=nations.y),
               color = 'red', size=3)

#using ggmaps extract world map
world_map <- qmap("World", zoom = 2)

#see how map looks
world_map

#plot the data on map
world_map + geom_point(aes(x=nation.x, y=nation.y),
                         data = countries, alpha = 0.5,
                         size = 3, color = "red")

# plot entire dataset in one command
# plot data
install.packages("tabplot")
library(tabplot)
tableplot(df)

###Sample gaussian distributions with 1, 2 and 3 modes.
install.packages('ggforce')
library(ggforce)
df <- data.frame(
    "Distribution" = c(rep("Unimodal", 500),
                       rep("Bimodal", 250),
                       rep("Trimodal", 600)),
    "Value" = c(rnorm(500, 6, 1),
                rnorm(200, 3, .7), rnorm(50, 7, 0.4),
                rnorm(200, 2, 0.7), rnorm(300, 5.5, 0.4), rnorm(100, 8, 0.4))
)

# Reorder levels
df$Distribution <- factor(df$Distribution,
                          levels(df$Distribution)[c(3, 1, 2)])

p <- ggplot(df, aes(Distribution, Value))
p + geom_violin(aes(fill = Distribution))

p + geom_sina(aes(color = Distribution), size = 1)


### contextual zoom
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species == "versicolor")

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(y = Species == "versicolor")

# Zoom in on versicolor on both axes
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(xy = Species == "versicolor")

# Use different zoom criteria on each axis
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species != 'setosa', y = Species == 'versicolor')

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
    geom_point() +
    facet_zoom(x = Species != 'setosa', y = Species == 'versicolor', 
               split = TRUE)

#--------------------------
# GET ANOTHER LAYER OF DATA
#--------------------------
library(maps)
# This is the data we're going to plot ...
foo <- c(-122.419416,-121.886329,-71.05888,-74.005941,-118.243685,-117.161084,-0.127758,-77.036871,116.407395,-122.332071,-87.629798,-79.383184,-97.743061,121.473701,72.877656,2.352222,77.594563,-75.165222,-112.074037,37.6173)
bar <- c(37.77493,37.338208,42.360083,40.712784,34.052234,32.715738,51.507351,38.907192,39.904211,47.60621,41.878114,43.653226,30.267153,31.230416,19.075984,48.856614,12.971599,39.952584,33.448377,55.755826)
zaz <- c(6471,4175,3144,2106,1450,1410,842,835,758,727,688,628,626,510,497,449,419,413,325,318)


# CREATE DATA FRAME
df.dummy <- data_frame(foo,bar,zaz)

df.more_data <- map_data("world")


# PLOT
ggplot(data = df.dummy, aes(x = foo, y = bar)) +
    geom_polygon(data = df.more_data, aes(x = long, y = lat, group = group)) +
    geom_point(aes(size = zaz), color = "red") 
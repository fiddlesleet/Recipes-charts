# plotting density distributions with ggplot2
library(dplyr)
library(ggplot2)

set.seed(1234)
d <- data_frame(cond = factor(rep(c("A", "B"), each=200)), 
                rating = c(rnorm(200), rnorm(200, mean=.8)))
d

# basic histogram of vector 'rating'. each bin has width .5
ggplot(d, aes(x = rating)) + 
  geom_histogram(binwidth = .5)

# with bin width 1
ggplot(d, aes(x = rating)) + 
  geom_histogram(binwidth = 1)

# with blue outline, red fill
ggplot(d, aes(x = rating)) + 
  geom_histogram(binwidth = .5, color = "blue", fill = "red")

# with blue outline, red fill, yellow line for the mean
ggplot(d, aes(x = rating)) + 
  geom_histogram(binwidth = .5, color = "blue", fill = "red") +
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)),
             color = "yellow", linetype = "dashed", size = 1)

# density curve
ggplot(d, aes(x = rating)) + 
  geom_density()

# histogram with kernel density curve
ggplot(d, aes(x = rating)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = .5,
                 color = "black",
                 fill = "white") + # histogram with denisty instead of count
  geom_density(alpha = .2, fill = "#FF6666")

# histogram with kernel density curve and mean line
## USE FOR MERYL STREEP ANALYSIS
ggplot(d, aes(x = rating)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = .5,
                 color = "black",
                 fill = "white") + # histogram with denisty instead of count
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1)

# overlaid histograms
## USE FOR MERYL STREEP ANALYSIS
# cond options: A or B
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity")

# interleaved histograms
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, position = "dodge")

# density plots overlaid
ggplot(d, aes(x = rating, color = cond)) +
  geom_density()

# density plots with semi-transparent fill
ggplot(d, aes(x = rating, fill = cond)) +
  geom_density(alpha = .3)

# Add lines for each mean requires first creating a 
#  separate data frame with the means:
means_d <- ddply(d, "cond", summarize, rating.mean = mean(rating))
means_d

# overlaid histograms with means
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  geom_vline(data = means_d, aes(xintercept = rating.mean, color = cond),
             linetype = "dashed", size = 1)

# density plots with means
ggplot(d, aes(x = rating, color = cond)) +
  geom_density() +
  geom_vline(data = means_d, aes(xintercept = rating.mean, color = cond),
             linetype = "dashed", size = 1)

# filled density plots with means
ggplot(d, aes(x = rating, color = cond, fill = cond)) +
  geom_density(alpha = .5) +
  geom_vline(data = means_d, aes(xintercept = rating.mean, color = cond),
             linetype = "dashed", size = 1)

# using facets
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  facet_grid(cond ~.)

# facets with mean lines
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") + 
  facet_grid(cond ~.) +
  geom_vline(data = means_d, aes(xintercept = rating.mean, color = cond),
             linetype = "dashed", size = 1)



ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") + 
  facet_grid(cond ~.) +
  stat_summary(fun.y = mean, geom = "line")
  geom_vline(data = means_d, aes(xintercept = rating.mean, color = cond),
             linetype = "dashed", size = 1)
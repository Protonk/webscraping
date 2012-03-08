library(maps)
library(plyr)
library(ggplot2)

# Assumes you have run the setup script to 
# generate price.df


# Show the growth from the start for the ratings as a whole

growFromSt <- function() {
  fromstart <- ddply(price.df, .(price.df[, "Date"]), "nrow")
  names(fromstart) <- c("Date", "Ratings")
  ggplot(data = fromstart, aes(Date, Ratings)) + geom_line() +
    scale_x_date(name = '') + scale_y_continuous(name = "Ratings per Day")
}

# Ratings by day of the month. Excluding the big jump for Jan 27, 2012 there isn't too 
# much there.

dayofmonth <- function() {
  dayM <- ddply(price.df, .(format(price.df[, "Date"], "%d")), "nrow")
  dayM[, 1] <- sub("^0","",dayM[, 1])
  names(dayM) <- c("DayofM", "Ratings")
  dayM[, 1] <- factor(dayM[, 1], levels = unique(sub("^0","",dayM[, 1])))
  ggplot(data = dayM, aes(DayofM, Ratings)) + geom_bar() +
    scale_x_discrete(name = '') + scale_y_continuous(name = "Ratings per Day")
}

# More options to be added later, but this produces maps by number of
# ratings and reported avg. price.

byState <- function(metric) {
  states_map <- map_data("state")
  rperst <- switch(metric,
                   Ratings = ddply(price.df, .(price.df[, "State"]), "nrow"),
                   Price = ddply(price.df, .(price.df[, "State"]), summarise, mean(`Price Per Oz`)))
  names(rperst) <- c("State", metric)
  rperst[, 1] <- tolower(rperst[, 1])
  # See http://stackoverflow.com/a/6489037/1188479 for why we use
  # aes_string and not aes
  ggplot(data = rperst, aes(map_id = State)) + geom_map(aes_string(fill = metric), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_y_continuous(name = '') + scale_x_continuous(name = '') + 
    opts(axis.text.y = theme_blank(), axis.text.x = theme_blank(), axis.ticks = theme_blank())
}

distbyweight <- function() {
  qplot(data = price.df, `Price Per Oz`,  geom = "density") + facet_wrap(~ Weight)
}

# note changes by date in general

datechange <- ddply(price.df, .(price.df[, "Date"]), summarise,
                    Quality = mean(unclass(Quality)),
                    Price = mean(`Price Per Oz`),
                    # Quality is just an ordered factor but
                    # Weight still has meaning
                    Weight = mean(as.numeric(as.character(Weight)))
                    )
names(datechange) <- c("Date", "Quality", "Price", "Weight")

# Not much shows up for autocorrelation. If you plot the longer series
# It doesn't seem that reported Quality, price or weight
# appreciably changed between 2010 and now.

plotautoCor <- function() {
  par(mfrow = c(3,1))
  for (i in names(datechange)[-1]) {
    pacf(datechange[, i], main = i)
  }
  par(mfrow = c(1,1))
}

# Plot a stacked barplot of proportion of reported prices captured by various sizes.
plotstackedW <- function() {
  library(reshape2)
  library(scales)
  weight.tab <- table(cut(price.df[, "Price Per Oz"], breaks = seq(25, 600, by = 25)), price.df[, "Weight"])
  weight.tab <- weight.tab/rowSums(weight.tab)
  weight.m <- melt(weight.tab)
  # Not quite accurate, but it looks purdy
  levels(weight.m[,1]) <- seq(25, 575, 25)
  names(weight.m) <- c("Price", "Weight", "Percent")
  barstack <- function() {
    ggplot(weight.m,aes(x = Price,y = Percent,fill = Weight)) + 
      geom_bar(position = "fill") + 
      scale_y_continuous(labels = percent_format(), name = "Proportion of Total") +
      scale_x_discrete(name = "Price")  
  }
  pseq <- seq(25, 575, 25)           
  wmr <- data.frame(cbind(pseq, wm))
  rownames(wmr) <- NULL
  names(wmr) <- c("x", "inf" ,"eighth", "fiveg", "quarter", "half", "ounce")
  ribbonstack <- function() {
    ribw <- ggplot(wmr, aes(x = x))
    ribw + geom_ribbon(aes(ymin = half, ymax = ounce), fill = "blue") + geom_ribbon(aes(ymin = quarter, ymax = half), fill = "orange") + geom_ribbon(aes(ymin = fiveg, ymax = quarter), fill = "green") + geom_ribbon(aes(ymin = eighth, ymax = fiveg), fill = "purple") + geom_ribbon(aes(ymin = inf, ymax = eighth), fill = "brown")
  }
  switch(type,
         bar = barstack(),
         ribbon = ribbonstack()
         )

}

#
# Load some variables locally rather than from a remote source
#
#


stateinc <- structure(list(State = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                                     "California", "Colorado", "Connecticut", "Delaware", "Florida", 
                                     "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                                     "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                                     "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                     "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                     "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                     "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                                     "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                                     "West Virginia", "Wisconsin", "Wyoming"), 
                           Income = c(42021L, 62592L, 49239L, 40812L, 58253L, 61251L, 66370L, 56128L, 47983L, 
                                      51406L, 65280L, 50022L, 53047L, 48698L, 50896L, 49315L, 41409L, 
                                      40778L, 49051L, 67813L, 59671L, 51413L, 59930L, 37757L, 47508L, 
                                      44445L, 51044L, 55771L, 67916L, 67499L, 44234L, 50966L, 43676L, 
                                      46250L, 49811L, 43012L, 51033L, 50840L, 56623L, 43942L, 48173L, 
                                      42953L, 46856L, 56745L, 52162L, 60983L, 59150L, 42207L, 54019L, 
                                      50234L)),
                          .Names = c("State", "Income"), class = "data.frame", row.names = c(NA,-50L))

price.df <- merge(price.df, stateinc, by = "State")


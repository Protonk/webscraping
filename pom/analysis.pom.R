library(maps)
library(plyr)
library(ggplot2)

# Assumes you have run the setup script to 
# generate trimmed.price.df


# Show the growth from the start for the ratings as a whole

growFromSt <- function() {
  fromstart <- ddply(trimmed.price.df, .(trimmed.price.df[, "Date"]), "nrow")
  names(fromstart) <- c("Date", "Ratings")
  ggplot(data = fromstart, aes(Date, Ratings)) + geom_line() +
    scale_x_date(name = '') + scale_y_continuous(name = "Ratings per Day")
}

# Ratings by day of the month. Excluding the big jump for Jan 27, 2012 there isn't too 
# much there.

dayofmonth <- function() {
  dayM <- ddply(trimmed.price.df, .(format(trimmed.price.df[, "Date"], "%d")), "nrow")
  dayM[, 1] <- sub("^0","",dayM[, 1])
  names(dayM) <- c("DayofM", "Ratings")
  dayM[, 1] <- factor(dayM[, 1], levels = unique(sub("^0","",dayM[, 1])))
  ggplot(data = dayM, aes(DayofM, Ratings)) + geom_bar() +
    scale_x_discrete(name = '') + scale_y_continuous(name = "Ratings per Day")
}

# More options to be added later, but this produces maps by number of
# ratings and reported avg. price.

byState <- function(metric) {
  rperst <- switch(metric,
                   Ratings = ddply(trimmed.price.df, .(trimmed.price.df[, "State"]), "nrow"),
                   Price = ddply(trimmed.price.df, .(trimmed.price.df[, "State"]), summarise, mean(`Price Per Oz`)))
  names(rperst) <- c("State", metric)
  rperst[, 1] <- tolower(rperst[, 1])
  states_map <- map_data("state")
  # See http://stackoverflow.com/a/6489037/1188479 for why we use
  # aes_string and not aes
  ggplot(data = rperst, aes(map_id = State)) + geom_map(aes_string(fill = metric), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_y_continuous(name = '') + scale_x_continuous(name = '') + 
    opts(axis.text.y = theme_blank(), axis.text.x = theme_blank(), axis.ticks = theme_blank())
}



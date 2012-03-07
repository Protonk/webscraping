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


library(maps)
library(plyr)
library(ggplot2)

# Assumes you have run the setup script to 
# generate price.df


# Show the growth from the start for the ratings as a whole

growFromSt <- function(method = "Raw") {
  # Initial recorded ratings per day
  fromstart <- ddply(price.df, .(price.df[, "Date"]), "nrow")
  # Next few lines fill in the blanks so we get an 
  # equally spaced series including days w/ no ratings
  # If you see sum(diff(fromstart[,1])) - nrow(fromstart)
  # we miss 94 days w/o this
  dateseq <- seq(fromstart[1, 1], fromstart[nrow(fromstart), 1], by = 1)
  fulldate.df <- data.frame(matrix(cbind(0,0), length(dateseq), 2), stringsAsFactors = FALSE)
  fulldate.df[, 1] <- dateseq
  fulldate.df[which(fulldate.df[, 1] %in% fromstart[ , 1]), 2] <- fromstart[ , 2]
  names(fulldate.df) <- c("Date", "Ratings")
  # Convert to time series so we can get some moving averages
  rate.ts <- ts(fulldate.df[, 2], start = fromstart[1, 1], end = fromstart[nrow(fromstart), 1])
  # rollmean() is a little more tractable than the other functions in base
  library(zoo)
  fulldate.df$Filtered <- as.numeric(c(rep(NA, 3), rollmean(rate.ts, k = 4, align = "center")))
  switch(method,
         Raw = ggplot(data = fulldate.df, aes(Date, Ratings)) + geom_line() +
           scale_x_date(name = '') + scale_y_continuous(name = "Ratings per Day"),
         MA = ggplot(data = fulldate.df, aes(Date, Ratings)) + geom_line(alpha = 0.7) +
           scale_x_date(name = '') + scale_y_continuous(name = "Ratings per Day") +
           geom_line(aes(Date, Filtered), colour = "blue", size = 1.3)
         )
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

# Not much shows up for autocorrelation. If you plot the longer series
# It doesn't seem that reported Quality, price or weight
# appreciably changed between 2010 and now.

plotautoCor <- function() {
  datechange <- ddply(price.df, .(price.df[, "Date"]), summarise,
                      Quality = mean(unclass(Quality)),
                      Price = mean(`Price Per Oz`),
                      # Quality is just an ordered factor but
                      # Weight still has meaning
                      Weight = mean(as.numeric(as.character(Weight)))
                      )
  names(datechange) <- c("Date", "Quality", "Price", "Weight")
  par(mfrow = c(3,1))
  for (i in names(datechange)[-1]) {
    pacf(datechange[, i], main = i)
  }
  par(mfrow = c(1,1))
}

# Plot a stacked plot of proportion of reported prices captured by various sizes.
# depending on type selected the result will be:
# bar: A fixed proportion plot of weight by price (discrete)
# ribbon: A fixed proportion plot of weight by price (continuous)
# density: A stacked density estimate of weight by price

plotstackedW <- function(type) {
  library(reshape2)
  library(scales)
  weight.tab <- table(cut(price.df[, "Price Per Oz"], breaks = seq(25, 600, by = 25)), price.df[, "Weight"])
  weight.tab <- weight.tab/rowSums(weight.tab)
  # Stack bar charts by treating price as a factor variable
  barstack <- function() {
    weight.m <- melt(weight.tab)
    # Not quite accurate, but it looks purdy
    levels(weight.m[,1]) <- seq(25, 575, 25)
    names(weight.m) <- c("Price", "Weight", "Percent")
    ggplot(weight.m,aes(x = Price,y = Percent,fill = Weight)) + 
      geom_bar(position = "fill") + 
      scale_y_continuous(labels = percent_format(), name = "Proportion of Total") +
      scale_x_discrete(name = "Price")  
  }
  # stack ribbon charts, treating price as continuous
  ribbonstack <- function() {
    # Cumsum of rows to produce bands for ribbon plot
    # add 0 on the left and x values to stick to 
    # one df w/ ggplot
    wmr <- data.frame(cbind(t(apply(weight.tab/rowSums(weight.tab), 1, cumsum))))
    names(wmr) <- c("eighth", "fiveg", "quarter", "half", "ounce")
    mwmr <- cbind(rep(seq(25,575,25), 5), melt(wmr))
    names(mwmr) <- c("Price", "Weight", "Proportion")
    mwmr[, "Weight"] <- factor(mwmr[, "Weight"], levels = c("ounce", "half", "quarter", "fiveg", "eighth"), ordered = TRUE)
    ggplot(mwmr, aes(x = Price)) + geom_ribbon(aes(ymin = 0, ymax = Proportion, fill = Weight))
  }
  switch(type,
         bar = barstack(),
         ribbon = ribbonstack(),
         # Colors are reversed on density. 
         density = qplot(`Price Per Oz`, data = price.df, fill = Weight, geom = "density", position="stack")
         )
}

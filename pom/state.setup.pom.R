# Main function to download data. 

buildPOWDf <- function() {
  library(XML)
  # The world may be metric but most people choose fractions of 
  # ounces from the dropdown menu
  ounce <- as.numeric("28.3495231")
  weight.assign <- c(1, 0.5, 0.125, 5/ounce, 0.25, 15/ounce, 10/ounce, 25/ounce, 20/ounce)
  
  #URL structure is very standard
  dir.urls <- paste("http://www.priceofweed.com/prices/United-States/", state.name, ".html", sep = "")
  
  # grabs the data for each state url
  
  perpageData <- function(input.url) {
    # Wrapper for XML package, see http://stackoverflow.com/questions/7269006/r-xml-package-how-to-set-the-user-agent
    UAhtmlParse <- function(url, ...) {
      temp <- tempfile()
      download.file(url, temp, quiet = TRUE)
      html.out <- htmlParse(temp, ...)
      unlink(temp)
      return(html.out)
    }
    options(HTTPUserAgent="Protonk at gmail. Contact if there is a problem")
    # Setup the while condition
    init.table <- data.frame(matrix(0, 0, 5))
    i <- 1
    p.next <- "Next"
    # While the pagination indicates there are more pages
    # continue to grab the next page, updating 
    # counter and pagination test
    while (grepl("Next", p.next, fixed = TRUE)) {
      single.parse <- UAhtmlParse(url = paste(input.url, "?pg=", i, sep = ""))
      if (length(readHTMLTable(single.parse, stringsAsFactors = FALSE)) < 2) return(init.table)
      init.table <- rbind(init.table, readHTMLTable(single.parse, stringsAsFactors = FALSE)[[2]])
      p.next <- xpathApply(single.parse, "//div[@id='pagination']", xmlValue)[[1]]
      i <- i + 1
      Sys.sleep(3)
    }
    init.table
  }
  
  #
  # Builds a dataframe for all observations by calling perpageData()
  #
  
  state.list <- lapply(dir.urls, perpageData)
  df.comb <- do.call(rbind, state.list)
  
  # reclass as character, remove rownames
  row.names(df.comb) <- NULL
  df.comb <- as.data.frame(matrix(as.character(unlist(df.comb, recursive = FALSE, use.names = FALSE)), dim(df.comb)), stringsAsFactors = FALSE)
  
  # Add state vector from "City, State" column, strip that 
  # column to just city
  df.comb[, "State"] <- factor(unlist(lapply(strsplit(df.comb[, 1], ", "), `[`, 2)))
  df.comb[, 1] <- unlist(lapply(strsplit(df.comb[, 1], ", "), `[`, 1))
  
  df.comb <- df.comb[, c(1,6,2:5)]
  names(df.comb) <- c("City", "State", "Price", "Weight", "Quality", "Date")
  
  df.comb[, "Price"] <- as.numeric(sub("\\$", "", df.comb[, "Price"]))
  
  # convert reported weights to numeric
  weight.num <- numeric(length = nrow(df.comb))
  for (i in seq_along(weight.assign)) {
    weight.num[which(df.comb[, "Weight"] %in% unique(df.comb[, "Weight"])[i])] <- weight.assign[i]
  }
  df.comb[, "Weight"] <- weight.num
  # only 77 report purchases between 0.5 oz and 1oz in grams. 
  df.comb <- df.comb[which(df.comb[, "Weight"] %in% weight.assign[1:5]),]
  
  # Strip outliers and add per oz price
  df.comb <- df.comb[which(df.comb[, "Price"]/df.comb[, "Weight"] >= 50 & df.comb[, "Price"]/df.comb[, "Weight"] <= 640), ]
  df.comb$`Price Per Oz` <- df.comb[, "Price"]/df.comb[, "Weight"]
  
  # Convert to factor now that most of the math is done
  df.comb[, "Weight"] <- signif(df.comb[, "Weight"], 3)
  df.comb[, "Weight"] <- factor(df.comb[, "Weight"], levels = sort(unique(df.comb[, "Weight"])))
  
  # Quality and Date to factor and date classes
  df.comb[, "Quality"] <- factor(df.comb[, "Quality"], levels = rev(unique(df.comb[, "Quality"])), ordered = TRUE)
  df.comb[, "Date"] <- as.Date(df.comb[, "Date"], format = "%B %d, %Y")
  
  df.comb <- df.comb[, c(1:4,7,5,6)]
  return(df.comb)
}

price.df <- buildPOWDf()

library(XML)


buildWeedDf <- function() {
  # The world may be metric but most people choose fractions of 
  # ounces from the dropdown menu
  ounce <- as.numeric("28.3495231")
  weight.assign <- c(1, 0.5, 0.125, 5/ounce, 0.25, 15/ounce, 10/ounce, 25/ounce, 20/ounce)
  
  #URL structure is very standard
  dir.urls <- paste("http://www.priceofweed.com/prices/United-States/", state.name, ".html", sep = "")
  
  # grabs the data for each state url
  
  perpageData <- function(input.url) {
    base.parse <- htmlParse(input.url)
    # Detect the number of pages for each state
    pag.test <- xpathApply(base.parse, "//div[@id='pagination']", xmlValue)[[1]]
    max.pages <- as.numeric(tail(unlist(strsplit(gsub("\\D", "", pag.test), "")), 1))
    # Character vector of URLs for each following page
    appended <- paste(input.url, "?pg=", 2:max.pages, sep = "")
    init.table <- readHTMLTable(base.parse)[[2]]
    for (i in seq_along(appended)) {
      init.table <- rbind(init.table, readHTMLTable(htmlParse(appended[i]))[[2]])
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
  df.comb[, "State"] <- unlist(lapply(strsplit(df.comb[, 1], ", "), `[`, 2))
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
  
  # Quality and Date to factor and date classes
  df.comb[, "Quality"] <- factor(df.comb[, "Quality"], levels = rev(unique(df.comb[, "Quality"])), ordered = TRUE)
  df.comb[, "Date"] <- as.Date(df.comb[, "Date"], format = "%B %d, %Y")
  
  # Strip outliers and add per oz price
  df.comb <- df.comb[which(df.comb[, "Price"]/df.comb[, "Weight"] >= 50 & df.comb[, "Price"]/df.comb[, "Weight"] <= 950), ]
  df.comb$`Price Per Oz` <- df.comb[, "Price"]/df.comb[, "Weight"]
  df.comb <- df.comb[, c(1:4,7,5,6)]
  return(df.comb)
}

trimmed.price.df <- buildWeedDf()



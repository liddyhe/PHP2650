############################################################################################################
## 2 S & P 100 Stock Prices
############################################################################################################

##import required packages
library(XML)
library(RCurl)


## Q1


## define url 
URL1 <- "https://en.wikipedia.org/wiki/S%26P_100"

## convert target url content into HTML
XML1 <- htmlParse(getURL(URL1),encoding="UTF-8")

## read HTML table into a list
table1 <- readHTMLTable(XML1)

## extract the corresponding table containing symbols and company names
stock_table <- table1[[2]]


## Q2


## create a character vector containing all symbols
stock_symbols <- as.character(stock_table$Symbol)

## check whether the pattern's occurrence is unique
length(grep("\\.B",stock_symbols))

## occurrence of pattern is 1, then use such pattern to change the symbol BRK.B to BRK-B
stock_symbols <- gsub("\\.B","\\-B",stock_symbols)


## Q3


## write a loop to download stock data
for (s in stock_symbols) {
  
  ##specify the url and the time period we want
  URL2 <- paste0("http://chart.finance.yahoo.com/table.csv?s=", s, "&a=11&b=31&c=2016&d=0&e=31&f=2017&g=d&ignore=.csv")
  
  ## download files
  print(download.file(URL2, paste0(s, ".csv"))) 
  
  ##set visit interval to avoid permission denied
  Sys.sleep(10)
}


## Q4


## loop for all symbols
for (s in stock_symbols){
  
  ## read csv files into R
  stock_raw <- read.csv(paste0(s, ".csv"))
  
  ## create a new dataset including corresponding symbols in the first column
  stock_sym <- cbind(rep(s,dim(stock_raw)[1]),stock_raw)
  
  ## define the column name
  names(stock_sym)[1] <- "Symbols"
  
  ## export new dataset into csv, without row names
  write.csv(stock_sym,paste0(s, "1.csv"), row.names = F)
}


## Q5


## create an empty vector in order to comobine datasets by rows
stock_final <- rep(NA,dim(stock_sym)[2])

## loop for all symbols
for (s in 1:length(stock_symbols)){
  
  ## read csv files into R, define columns which have character to "character" class 
  ## so we prevent R from seeing "F" and "T" as "FALSE" and "TRUE"
  stock_tem <- read.csv(paste0(stock_symbols[s], "1.csv"), colClasses = "character")
  
  ## combine datasets by rows
  stock_final <- rbind(stock_final,stock_tem)
}

## export final dataset into csv without row names
write.csv(na.omit(stock_final),"Stock_final.csv", row.names = F)



############################################################################################################
## 3 Funding and Publicationa
############################################################################################################


## Q1

## read csv into R
raw_tables <- read.csv("NIHHarvard.csv", colClasses = "character")

## delete value of "Activity" beginning with "T" or "F" 
raw_tables <- raw_tables[-grep("^[TF]",raw_tables$Activity),]

## define a function to delete whitespace at the end of each string
trim.trailing <- function (x) sub("\\s+$", "", x)

## create a character variables containing all unique names
## Note we must deal with trailing blank at first otherwise there will be replicates
PI_names <- unique(trim.trailing(raw_tables$Contact.PI...Project.Leader))


## Q2

## use a pattern to delete all middle names and initials
PI_names[grep("\\,\\s[A-Z]+[-']*[A-Z]*\\s[A-Z]+[-']*[A-Z]*\\.*$", PI_names)] <- 
  sub("\\s[A-Z]+[-']*[A-Z]*\\.*$","",PI_names[grep("\\,\\s[A-Z]+[-']*[A-Z]*\\s[A-Z]+[-']*[A-Z]*\\.*$", PI_names)])


## Q3

## change names format to match searching format
name_tem <- sub("(\\,\\s)", "%2C+", PI_names)
name_final <- sub("\\s", "+", name_tem)

## create an empty vector to store number of publication
num_publication <- rep(NA,length(name_final))

## loop
for (i in 1:length(name_final)){
  
  ## create URL
  URL3 <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", name_final[i], "%5BAuthor%5D+AND+Harvard%5BAffiliation%5D")
  
  ## read URL into R
  html <- htmlParse(getURL(URL3),encoding="UTF-8")
  
  ## extract the target information
  publications <- xpathSApply(html,"//h3[@class='result_count left']",xmlValue)
  
  ## if there is only one publication, variable publications will become "list()"
  if(length(publications)==0){
    num_publication[i] = 1
  }
  
  ## if there is no publication or more than one publications, it will return "Items: ..."
  else{
    
    ## specify the index of all whitespace
    publications1 <- gregexpr(pattern =' ', publications)
    
    ## the number of the last whitespace is our target
    num_publication[i] <- as.numeric(substr(publications,start=max(publications1[[1]])+1,stop=nchar(publications)))
  
  }

}


## Q4


## combine PI names and corresponding number of publications togehther
final <- cbind(PI_names, num_publication)

## export to csv file
write.csv(final,"PI_publications.csv", row.names = F)






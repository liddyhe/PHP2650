################################################################
## PHP2650 HW1 Fangyu Wang
################################################################

###Problem 2.

## Download page using RCurl
library(RCurl)
theurl <- "https://en.wikipedia.org/wiki/S%26P_100"
a <- getURL(theurl)

#1.
table=readHTMLTable(a)[2]
write.table(table,"table.text")
table=read.csv("~/table.text", sep="")
names(table)=c("Symbol","Name")
table

#2.
newsymbol=gsub("BRK.B","BRK-B",as.matrix(table));newsymbol

#3.
for (i in newsymbol[,1][-which(newsymbol[,1]=="MS")]) { #NOTE: THERE IS NO WEBPAGE FOR MS
download.file(paste("http://chart.finance.yahoo.com/table.csv?s=",i,"&a=11&b=31&c=2016&d=0&e=31&f=2017&g=d&ignore=.csv",sep=""), paste(i,".csv",sep=""))
}

#4.
for (i in newsymbol[,1][-which(newsymbol[,1]=="MS")]) {
stock=read.csv(paste("~/",i,".csv",sep=""))
stock=cbind(rep(paste(i),dim(stock)[1]),stock)
write.csv(stock,paste(i,".csv",sep=""))
}

#5.
stock=rep(NA,8)
for (i in newsymbol[,1][-which(newsymbol[,1]=="MS")]) {
  filename=read.csv(paste("~/",i,".csv",sep=""))
  stock=rbind(stock,filename)
}
stock=stock[-1,-1]
names(stock)[1]="Stock.Symbol";stock
write.csv(stock,"stock.csv")


###Problem3.

#1.
hv=read.csv("~/Downloads/NIHHarvard.csv")
hv=hv[-grep("T",hv$Activity),]
hv=hv[-grep("F",hv$Activity),]

hv$Contact.PI...Project.Leader=gsub("^\\s+|\\s+$", "", hv$Contact.PI...Project.Leader)
uniquePI=unique(sort(hv$Contact.PI...Project.Leader));uniquePI

#2.
for (i in 1:length(uniquePI)) {
  if (length(gregexpr(" ", uniquePI[[i]])[[1]])==2) {
    uniquePI[[i]]=sub("\\s+[A-Z]+\\.*$", "", uniquePI[[i]])
  }
}

uniquePI

#3.
NumberPublish=function(name){
  url=paste("https://www.ncbi.nlm.nih.gov/pubmed/?term=",name,"+Harvard",sep="")
  a=getURL(url)
  a=readLines(tc <- textConnection(a)); close(tc)
  if(length(grep("The following term was not found in PubMed:",a))==1) {
    numpub=0
  } else if (length(grep("No documents match your search terms",a))==1) {
    numpub=0 
  } else if (length(grep("Showing results for",a))==1) {
    numpub=0
  } else if (length(grep("            <div><h2>Search results</h2><h3 class=\"result_count left\">Items:",a))==1) {
    number=a[grep("            <div><h2>Search results</h2><h3 class=\"result_count left\">Items:",a)]
    number=regmatches(number,gregexpr('[0-9]+</h3>',number))
    numpub=gsub("</h3>","",number)
  } else {
    numpub=1
  }
  numpub
}

namesearch=gsub(", ","+",uniquePI)
numpub=0
for (i in namesearch) {
numpub=c(numpub,NumberPublish(i))
}
numpub=numpub[-1]
cbind(uniquePI,numpub)

#4.
write.csv(cbind(uniquePI,numpub),"Publish.csv")










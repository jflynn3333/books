
library(data.table)
bookStatsDir <- "C:/Users/fly/Documents/personal/inventory/book.stats/"
downloadDate <- 20171105
downloadDate <- 20180128
downloadDate <- 20180131
downloadDate <- 20180216
downloadDate <- 20180217
verbose <- 1



readDt        <- fread(paste0(bookStatsDir,"read.",downloadDate,".csv"))
toreadDt      <- fread(paste0(bookStatsDir,"toread.",downloadDate,".csv"))
collectionsDt <- fread(paste0(bookStatsDir,"collections.",downloadDate,".csv"))
awardssdt     <- fread(paste0(bookStatsDir,"awards.",downloadDate,".csv"))
awardsnDt     <- fread(paste0(bookStatsDir,"nawards.",downloadDate,".csv"))
purchasesDt   <- fread(paste0(bookStatsDir,"purchases.",downloadDate,".csv"))

"Author", "Title", "Published", "Source", "PurchaseDate", "Key", "Read", "Read"


books.save[["awardsnDt.20180128"]]$Key[1:10]
awardssdt$Key[1:10]

setdiff(awardsDT$key, awardsnDt$key)


setdiff(books.save[["awardsnDt.20180128"]]$Key, awardssdt$Key)

setdiff(books.save[["awardsnDt.20180131"]]$Key, awardssdt$Key)


books.save[[paste0("purchasesDt.",downloadDate)]] <- purchasesDt
books.save[[paste0("read.",downloadDate)]] <- readDt
books.save[[paste0("toread.",downloadDate)]] <- toreadDt
books.save[[paste0("awardsnDt.",downloadDate)]] <- awardsnDt
books.save[[paste0("awardssDt.",downloadDate)]] <- awardsnDt

books.save[[paste0("collections.",downloadDate)]] <- collectionsDt
books.save
names(books.save)

# find differences
setdiff(books.save[["awardsnDt.20180128"]]$Key ,books.save[["awardsnDt.20180216"]]$Key )
setdiff(books.save[["awardsnDt.20180216"]]$Key ,books.save[["awardsnDt.20180128"]]$Key )

setdiff(books.save[["read.20180216"]]$Key ,books.save[["read.20180131"]]$Key )
setdiff(books.save[["read.20180131"]]$Key ,books.save[["read.20180216"]]$Key )

dim(books.save[["read.20180131"]])



collections_by_author <- function(author) {
  return(collectionsDt[grep(author,Author) ,.(Author,Collection=substring(Collection,1,50),Title=substring(Title,1,25),Year,Type,ReadYear,Nominations,Wins,ReadYear,ReadWords,Owned)])
}
if (verbose == 1) { collections_by_author("Butler") }


collections_by_title <- function(collections_by_title) {
  return(collectionsDt[grep(collections_by_title,Collection) ,.(Author,Collection=substring(Collection,1,25),Title=substring(Title,1,25),Year,Type,ReadYear,Nominations,Wins,ReadYear,ReadWords,Owned)])
}
if (verbose == 1) { collections_by_title("Christmas") }


purchases_by_author <- function(author) {
  return(purchasesDt[grep(author,Author) ,.(Author, Title, Published, Source, PurchaseDate, Key, Read, Read)])
}
if (verbose == 1) { purchases_by_author("Butler") }

purchases_by_title <- function(title) {
  return(purchasesDt[grep(title,Title) ,.(Author, Title, Published, Source, PurchaseDate, Key, Read, Read)])
}
if (verbose == 1) { purchases_by_title("Wild") }


collections_by_title <- function(title) {
  return(collectionsDt[grep(title,Title) ,.(Author,Collection=substring(Collection,1,50),Title=substring(Title,1,25),Year,Type,ReadYear,Nominations,Wins,ReadYear,ReadWords,Owned)])
} 
if (verbose == 1) { collections_by_title("deck") }


awardss_by_author <- function(author) {
  return(awardssDt[grep(author,Author),.(Author,Title=substring(Title,1,25),ReadYear,HugoSS,NebulaSS,LocusSS,WFSS,SJSS,Sturgeon,BSFA,HugoNV,NebulaNV,LocusNV,HugoNA,NebulaNA,LocusNA,BSSS,BSNA,SJNV,SJNA,WFNA,BFASS,BFANA,SS,All,Winners,ReadWords,ToReadWords,ReadDate,ToReadSource,Weight)])
}
if (verbose == 1) { awardss_by_author("Butler") }

awardss_by_title <- function(title) {
  return(awardssDt[grep(title,Title),.(Author,Title=substring(Title,1,25),ReadYear,HugoSS,NebulaSS,LocusSS,WFSS,SJSS,Sturgeon,BSFA,HugoNV,NebulaNV,LocusNV,HugoNA,NebulaNA,LocusNA,BSSS,BSNA,SJNV,SJNA,WFNA,BFASS,BFANA,SS,All,Winners,ReadWords,ToReadWords,ReadDate,ToReadSource,Weight)])
}
if (verbose == 1) { awardss_by_title("deck") }


awardsn_by_author <- function(author) {
  return(awardsnDt[grep(author,Author),.(Author,Title,Read,Hugo,Nebula,LocusSF,BSFA,Campbell,
Clarke,RetroHugo,LocusFantasy,WF,SF,Nom,Win,Read)])
}
if (verbose == 1) { awardsn_by_author("Butler") }


awardsn_by_title <- function(title) {
  return(awardsnDt[grep(title,Title),.(Author,Title,Read,Hugo,Nebula,LocusSF,BSFA,Campbell,
Clarke,RetroHugo,LocusFantasy,WF,SF,Nom,Win,Read)])
}
if (verbose == 1) { awardsn_by_title("Butler") }


read_by_author <- function(author) {   
  return(readDt[grep(author,Author),.(Author,Title,Written,ReadYear,ReadDate,Words,Pages,GR,SSNom,SSWin,NovNom,NovWin)])
}  
if (verbose == 1) { read_by_author("Butler") }


read_by_title <- function(title) {   
  return(readDt[grep(title,Title),.(Author,Title,Written,ReadYear,ReadDate,Words,Pages,GR,SSNom,SSWin,NovNom,NovWin)])
}  
if (verbose == 1) { read_by_author("Smoke") }

toread_by_author <- function(author) {   
  return(toreadDt[grep(author,Author),.(Author,Story,Read,Written,Key,Source,Words,SSNom,SSWin,NovNom,NovWin,AlreadyRead)])
}  
if (verbose == 1) { toread_by_author("Butler") }


asum <- function(author) {
  rl = list()
  rl$read_by_author <- read_by_author(author)
  rl$toread_by_author <- toread_by_author(author)
  rl$awardsn_by_author <- awardsn_by_author(author)
  rl$awardss_by_author <- awardss_by_author(author)
  rl$collections_by_author <- collections_by_author(author)
  rl$purchases_by_author <- purchases_by_author(author)
  return(rl)
}
if (verbose == 1) { asum("Butler") }



csum <- function(title) {
  rl = list()
  rl$collections_by_title <- collections_by_title(title)
  rl$purchases_by_title <- purchases_by_title(title)
  
  return(rl)

}
if (verbose == 1) { csum("Christmas") }

tsum <- function(title) {
  rl = list()
  rl$collections_by_title <- collections_by_title(title)
  rl$awardss_by_title <- awardss_by_title(title)
  rl$awardsn_by_title <- awardsn_by_title(title)
  rl$read_by_title <- read_by_title(title)
  rl$purchases_by_title <- purchases_by_title(title)
  return(rl)
}
if (verbose == 1) { tsum("deck") }



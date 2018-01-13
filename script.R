library(RCurl)
library(jsonlite)
library(RSelenium)


yc_email <- "Your email here"
yc_password <- "Your Y-Charts Password here"
machine_ip <- "your_selenium_machine_ip"

#Get all Nasdaq-listed companies
nasdaqUrl <- "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download"
myfile <- getURL(nasdaqUrl)
nasdaqList <- read.csv(textConnection(myfile), header=T, stringsAsFactors=F)
nasdaqCompanies <- nasdaqList[,1]

#Get all NYSE-listed companies
nyseUrl <- "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download"
myfile <- getURL(nyseUrl)
nyseList <- read.csv(textConnection(myfile), header=T, stringsAsFactors=F)
nyseCompanies <- nyseList[,1]

#NYSE and Nasdaq Companies Together- we end up with 6,423 companies
allCompanies <- c(nasdaqCompanies, nyseCompanies)
allCompanies <- unique(allCompanies)

#for Docker
#docker pull selenium/standalone-firefox:2.53.0
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0

#to stop an image 
#docker stop $(docker ps -q)

#for testing purposes, I want to run the program on a virtual VNC machine, 
# so I run the following on Docker:
# full tutorial on how to do this is here: http://rpubs.com/johndharrison/RSelenium-Docker
# docker pull selenium/standalone-firefox:2.53.0
# docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.0

#then open VNC, Remote Host  machine-ip:5901
#password: secret

remDr <- remoteDriver(remoteServerAddr = machine_ip,port = 4445L)
remDr$setImplicitWaitTimeout(10)
remDr$open(silent = TRUE)
remDr$setWindowSize(width=1400, height=1000)
remDr$navigate("https://ycharts.com/login")

email <- remDr$findElement(using="css selector", "#id_username")
email$sendKeysToElement(list(yc_email))

password <- remDr$findElement(using="css selector", "#id_password")
password$sendKeysToElement(list(yc_password))

signIn <- remDr$findElement(using="css selector", "#mainContent > div > div.enterPage > div > div > form > div > div.formBottom > div > button")
signIn$clickElement()

browser_check <- function () {
  tryCatch({remDr$getCurrentUrl()},
           error= function (e) {
             print("RSelenium browser died")
             remDr$open(silent = TRUE)
             remDr$setWindowSize(width=1400, height=1000)
           }
  )
}

companiesThatRanDF <- marketData[which(!is.na(marketData$V2)),]
companiesThatRan <- companiesThatRanDF$V1

companiesThatFailed <- allCompanies[!allCompanies %in% companiesThatRan]

companiesToRun <- read.csv("largeCompaniesToRun.csv")
companiesToRun <- companiesToRun$x

marketCapDataFrame <- data.frame()
for(i in 1:nrow(companiesToRun)) {
  if(i%%100==0) {0
    write.csv(marketCapDataFrame, "largeMarketData.csv")
  }
  tryCatch({
    browser_check()
    symbol <- companiesToRun[i]
    marketCapURL <- paste0("https://ycharts.com/companies/", symbol, "/market_cap")
    remDr$navigate(marketCapURL)
    Sys.sleep(3)
    
    possibleError <- tryCatch({ currentMarketCap <- remDr$findElement(using="css selector", "#pgNameVal")}, error = function(e) {
      marketCapDataFrame[i,1] <- symbol
      marketCapDataFrame[i,2] <- NA
      marketCapDataFrame[i,3] <- NA
      print(paste0("skipped ", symbol))
    })
    
    if(is.environment(possibleError)){
      
      currentMarketCapText <- currentMarketCap$getElementText()[[1]]
      currentMarketCapData <- gsub(" for Dec. 1\\d{1}, 2017", "", currentMarketCapText)
      if(grepl("B", currentMarketCapData)) {
        currentMarketCapData <- gsub("B", " * 1000000000", currentMarketCapData)
      } else if(grepl("M", currentMarketCapData)) {
        currentMarketCapData <- gsub("M", " * 1000000", currentMarketCapData)
      }
      currentMarketCapData <- eval(parse(text=currentMarketCapData))
      
      startDate <- remDr$findElement(using="css selector", ".dateRangeControl > input:nth-child(1)")
      startDate$clearElement()
      startDate$sendKeysToElement(list("01/03/2017"))
      
      endDate <- remDr$findElement(using="css selector", ".dateRangeControl > input:nth-child(3)")
      endDate$clearElement()
      endDate$sendKeysToElement(list("01/03/2017"))
      
      getData <- remDr$findElement(using="css selector", ".dateRngSbmt")
      getData$clickElement()
      
      possibleNoData <- tryCatch({historicMarketCap <- remDr$findElement(using="css selector", "div.ng-binding > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(2)")
      }, error = function(e) {
        #if no data for 01/03/2017, expand the date-range
        startDate <- remDr$findElement(using="css selector", ".dateRangeControl > input:nth-child(1)")
        startDate$clearElement()
        startDate$sendKeysToElement(list("12/20/2016"))
        
        endDate <- remDr$findElement(using="css selector", ".dateRangeControl > input:nth-child(3)")
        endDate$clearElement()
        endDate$sendKeysToElement(list("01/10/2017"))
        
        getData <- remDr$findElement(using="css selector", ".dateRngSbmt")
        getData$clickElement()
        
        noExpandedData <- tryCatch({historicMarketCap <- remDr$findElement(using="css selector", "div.ng-binding > div:nth-child(1) > div:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(2)")}, error=function(e){
          marketCapDataFrame[i,1] <- symbol
          marketCapDataFrame[i,3] <- currentMarketCapData
          
          print(paste0("No historic data for ", symbol))
        })
        
        if(is.environment(noExpandedData)) {
          historicMarketCapData <- historicMarketCap$getElementText()[[1]]
          if(grepl("B", historicMarketCapData)) {
            historicMarketCapData <- gsub("B", " * 1000000000", historicMarketCapData)
          } else if(grepl("M", historicMarketCapData)) {
            historicMarketCapData <- gsub("M", " * 1000000", historicMarketCapData)
          }
          historicMarketCapData <- eval(parse(text=historicMarketCapData))
          
          marketCapDataFrame[i,1] <- symbol
          marketCapDataFrame[i,2] <- historicMarketCapData
          marketCapDataFrame[i,3] <- currentMarketCapData
          
          print(marketCapDataFrame[i,])
        }
      })
      
      if(is.environment(possibleNoData)){
        historicMarketCapData <- historicMarketCap$getElementText()[[1]]
        if(grepl("B", historicMarketCapData)) {
          historicMarketCapData <- gsub("B", " * 1000000000", historicMarketCapData)
        } else if(grepl("M", historicMarketCapData)) {
          historicMarketCapData <- gsub("M", " * 1000000", historicMarketCapData)
        }
        historicMarketCapData <- eval(parse(text=historicMarketCapData))
        
        marketCapDataFrame[i,1] <- symbol
        marketCapDataFrame[i,2] <- historicMarketCapData
        marketCapDataFrame[i,3] <- currentMarketCapData
        
        print(marketCapDataFrame[i,])
      }
    } 
    }, error=function(e) {})
}

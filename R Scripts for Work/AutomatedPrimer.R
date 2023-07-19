install.packages("RSelenium")
install.packages("devtools")
devtools::install_github("ropensci/RSelenium")


'''
Download phantomjs from http://phantomjs.org/download.html and extract the exe file to the path folder

From comments:
Click start 
Select Control Panel > System 
Select Advance system settings
Click Environment Variables...

Under System Variables
Scroll to Path and double click
At the end of Variable value: add ;C:\path\to\directory that holds the chromedriver.exe file. Note the ; that separates the paths

Restart your R session and you should now be able to run:
'''

CreatePrimerBatchPrimer <- function(CSVName, LineStart = 1, LineEnd = length(file[,1]), separetedBy=","){
  # Start Selenium Server
  RSeleniumStart <- function(x){
    library(RSelenium)
    # Start Selenium Server --------------------------------------------------------
    # checkForServer()
    # When promot allow network access ---------------------------------------------
    # startServer()
    
    rD <- rsDriver(verbose = FALSE,port=4442L, browser = "phantomjs")
    remDrv <- rD[["client"]]
    
    # remDrv <- remoteDriver(browserName = "phantomjs")
    # remDrv$open()
    return(remDrv)
  }
  # Start Selenium Server
  remDrv <- RSeleniumStart()
  
  # Start twice becouse for some reason it crashes the first time
  # remDrv <- RSeleniumStart()

  
  # website function
  UCSCIn_SilicoPCR <- function(PF,PR){
    # Simulate browser session and fill out form -----------------------------------
    remDrv$navigate('https://genome.ucsc.edu/cgi-bin/hgPcr')
    # remDrv$screenshot(display = TRUE) # to see the screenshot
    # Sys.sleep(3) 
    remDrv$findElement(using = "xpath", "//*/select[@name = 'db']/option[@value = 'hg19']")$clickElement()
    # Sys.sleep(1) 
    remDrv$findElement(using = "xpath", "//*/input[@name = 'wp_f']")$sendKeysToElement(list(PF))
    remDrv$findElement(using = "xpath", "//*/input[@name = 'wp_r']")$sendKeysToElement(list(PR))
    remDrv$findElement(using = "xpath", "//*/input[@name = 'Submit']")$clickElement()
    # Sys.sleep(1) 
    webElem <- remDrv$findElement("xpath", "//body")
    Text <- webElem$getElementText()
    
    # Send Selenium Server result --------------------------------------------------------
    return(Text[[1]])
  }
  
  # Run the file lines
  file <- read.csv(CSVName, sep = separetedBy)
  
  ExtractedUCSC <- apply(file[LineStart:LineEnd,], 1, function(x){
    UCSCIn_SilicoPCR(as.vector(x[2]),as.vector(x[3]))
  })
  
  # # End Selenium Server
  # RSeleniumEnd <- function(x){
  #   library(RSelenium)
  #   # Emd Selenium Server --------------------------------------------------------
  #   remDrv$quit()
  #   remDrv$close()
  #   remDrv$closeServer()
  #   rD$server$stop()
  #   rm(rD)
  # }
  # RSeleniumEnd()

  # Return the result
  return(ExtractedUCSC)
  
}

result <- CreatePrimerBatchPrimer("Primers_new_test.csv")

# result <- CreatePrimerBatchPrimer("adapterprimers.csv")

CreatAmbienceForSaving <- function(CSVName, BatchPrimerResult, separetedBy=",", NCol=3, OutPutName_DidntWork = "PrimersThatDidntWork5.csv", 
                                   OutPutName_WorkWithOneSeq = "ThatWorkOnce5.csv", OutPutName_WorkWithMoreThanOneSeq = "ThatWorkMoreThanOnce5.csv",
                                   OutPutName_FinalDataMinning = "WorkedPrimers5.csv"){
  
  file <- read.csv(CSVName, sep = separetedBy)
  ResultAsText <- gsub("\n", " ", BatchPrimerResult)
  BindedResult <- cbind( file, ResultAsText)
  PrimersThatDidntWork <- BindedResult[grep("No matches to ", BindedResult[,NCol+1]),]
  write.csv(PrimersThatDidntWork, OutPutName_DidntWork, row.names = F,  quote = F)
  ThatWork <- BindedResult[grep(">", BindedResult[,NCol+1]),]
  ThatWork2 <- ThatWork
  ThatWork2[,NCol+2] <- sapply(ThatWork[,NCol+1], function(x) {
    length(grep(">", strsplit(as.character(x), split= " ")[[1]]))
  })
  ThatWorkMoreThanOnce <- ThatWork2[ThatWork2[,NCol+2]>1,]
  ThatWorkOnce <- ThatWork2[ThatWork2[,NCol+2]==1,]
  write.csv(ThatWorkOnce, OutPutName_WorkWithOneSeq, row.names = F,  quote = F)
  write.csv(ThatWorkMoreThanOnce, OutPutName_WorkWithMoreThanOneSeq, row.names = F,  quote = F)
  
  WorkedPrimers <- ThatWorkOnce[,1:NCol]
  
  WorkedPrimers2 <- cbind(WorkedPrimers, matrix(unlist(lapply(ThatWorkOnce[,NCol+1], function(x) {
    striped1 <- strsplit(as.character(x), split= " ")[[1]] 
    striped1 <- striped1[grep("Forward:", striped1)+1]
    striped2 <- strsplit(as.character(x), split= " ")[[1]] 
    striped2 <- striped2[grep("Reverse:", striped2)+1]
    striped3 <- strsplit(as.character(x), split= " ")[[1]] 
    striped3 <- striped3[grep(">",striped3)]
    striped3 <- strsplit(as.character(striped3), split= "[>|:|+|-]")[[1]]
    return(c(striped1,striped2,striped3[3],striped3[4],striped3[1],striped3[1],striped3[1],striped3[1],striped3[2]))
  })),ncol = 9, byrow = TRUE))
  
  write.csv(WorkedPrimers2, OutPutName_FinalDataMinning, row.names = F,  quote = F)
  
  
}

CreatAmbienceForSaving("Primers_new_test.csv", result, NCol=4)

FetchSequence <- function(CSVName, ColChr=12, ColChrStart=6, ColChrEnd=7, LineStart = 1, 
                          LineEnd = length(file[,1]), separetedBy=","){
  
  # Start Selenium Server
  RSeleniumStart <- function(x){
    library(RSelenium)
    # Start Selenium Server --------------------------------------------------------
    checkForServer()
    # When promot allow network access ---------------------------------------------
    startServer()
    remDrv <- remoteDriver(browserName = "phantomjs")
    remDrv$open()
    return(remDrv)
  }
  
  # Start Selenium Server
  remDrv <- RSeleniumStart()
  
  # Start twice becouse for some reason it crashes the first time
  remDrv <- RSeleniumStart()
  
  # website function
  togows_Seq <- function(Chr, ChrStart, ChrEnd){
    # Simulate browser session and fill out form -----------------------------------
    Link <- gsub("[[:space:]]","",paste('http://togows.org/api/ucsc/hg19/', Chr,':' ,ChrStart,'-' ,ChrEnd, sep=""))
    print(ChrStart)
    remDrv$navigate(Link)

    # remDrv$screenshot(display = TRUE) # to see the screenshot
    # Sys.sleep(3)
    Sys.sleep(0.1)
    webElem <- remDrv$findElement("xpath", "//body")
    Text <- webElem$getElementText()
    # Send Selenium Server result --------------------------------------------------------
    return(Text[[1]])
  }
  
  # Run the file lines
  file <- read.csv(CSVName, sep = separetedBy)
  ExtractedSeq <- apply(file[LineStart:LineEnd,], 1, function(x){
    togows_Seq(as.vector(x[ColChr]), as.vector(x[ColChrStart]), as.vector(x[ColChrEnd]))
  })
  
  # End Selenium Server
  RSeleniumEnd <- function(x){
    library(RSelenium)
    # Emd Selenium Server --------------------------------------------------------
    remDrv$quit()
    remDrv$closeServer()
  }
  RSeleniumEnd()
  
  # Return the result
  return(ExtractedSeq)
  
}


result <- FetchSequence("fetchingSequence.csv")



PrimerTable <- read.csv("fetchingSequence.csv")
ncol(PrimerTable)

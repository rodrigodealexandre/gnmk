CreateFasta <- function(Ensembl){
  Ensembl <- gsub("[[:space:]]|[[:blank:]]|[[:cntrl:]]|[[:digit:]]|", "", Ensembl, perl=TRUE)
  Ensembl <- gsub("\r", "", Ensembl, perl=TRUE)
  Ensembl <- gsub("\\,|\\-", "", Ensembl, perl=TRUE)
  Ensembl <- gsub("\\.", "-", Ensembl, perl=T)
  Ensembl <- paste(Ensembl, sep="", collapse ="")
  name <- paste(readline(prompt="Enter gene name: "), sep="_", collapse ="")
  fasta<- c(paste(">", name, "_complete", sep = ""), Ensembl)
  write.table(fasta, paste("F:/Genomika/Referencias/", paste(name, "_complete.fasta", sep = ""), sep = ""), row.names=FALSE,col.names=FALSE, quote = F)
}

CreateFasta(readClipboard())

# install.packages("knitr")
# install.packages("seqinr")
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# biocLite(c("sangerseqR", "seqTools")) #install sangerseqR and seqTools with bioconductor
library(sangerseqR)
library(seqTools)
library(seqinr)
library(GenomicRanges)


# ## ref cordinates for "1het.ab1"
# chr <- 13
# chrStart <- 32913813
# chrEnd <- 32914379
# 
# ## ref cordinates for "1frame.ab1"
# chr <- 13
# chrStart <- 32910837
# chrEnd <- 32911482
# 
### ref cordinates for "1wt.ab1"
# chr <- 6
# chrStart <- 26091342
# chrEnd <- 26091078
# 
# ## ref cordinates for "1som.ab1"
# chr <- 17
# chrStart <- 7577608
# chrEnd <- "577498
# strand <- "-"

ReferenceFastaFile <- function(chr, chrStart, chrEnd, strand = "+"){
  if(chrStart > chrEnd | strand == "-"){
    temp <- chrStart
    chrStart <- chrEnd
    chrEnd <- temp
  }
  if(strand == "-"){
    temp <- chrStart
    chrStart <- chrEnd
    chrEnd <- temp
  }
  fasta <- read.fasta(paste("http://togows.org/api/ucsc/hg19/chr",chr,":",chrStart,"-",chrEnd,".fasta", sep = ""), as.string = TRUE)
  write.fasta(fasta[[1]][1],paste(names(fasta)),gsub(":","-", paste(names(fasta),".fasta", sep="")))
  
}

Abi2FastQ <- function(AbiFile, FastaRef="null", RatioRange="normal"){
  
  #'This script works and was tested for a 3500 Series Genetic Analyzer.
  #'AbiFile is the name of the file to be converted
  #'FastaRef is the reference sequence in fasta format with the same exact direction as the sanger and strand of DNA. Highly recomended for frameshifts.
  #'Ratio range can be selected as "normal" for germinative and low sensitivity. The option "lower" reduces the ratio range and create higher sensitivity for somatic and background noises.
  
  #Ignore the normal loading error.
  options(warn= -1)
  Abi <- tryCatch(read.abif(AbiFile), error=function(e) NULL)
  options(warn= 0)
  
  #Quality Check. If sequence does not have enough quality it will no proceed.
  if(is.null(Abi[3][[1]]$CRLn.2)){
    warning("Sequence without enough quality")
    return()
  }
  
  #Ignore the normal loading error.
  options(warn= -1)
  reads <- tryCatch(readsangerseq(AbiFile), error=function(e) NULL )
  options(warn= 0)
  
  #Set the range ratio
  if(RatioRange=="normal"){
    range <- seq(0.25, 0.45, by = 0.01)
  } else if(RatioRange=="lower"){
    range <- seq(0.1, 0.4, by = 0.01)
  } else {
    warning("Choose 'RatioRange' between the strings 'normal' or 'lower'")
    return()
  }
  
  #Check if there is a reference file
  if(FastaRef!="null"){
    ref <- read.fasta(FastaRef, as.string = TRUE)[[1]][1]
    if(is.character(ref)!=TRUE){
      warning("check if the fasta file is correct")
      return()
    }
  }
  
  ### phred quality run quality
  qual1 <- as.numeric(unlist(PhredQuality(Abi[3][[1]]$PCON.1)))
  qual2 <- as.numeric(unlist(PhredQuality(Abi[3][[1]]$PCON.2)))
  
  ###quality bases selection lower ration lowerhigher signal id and more missmatches
  calls <- makeBaseCalls(reads, ratio = 0.2)
  
  ### due to unknown ratio treaming, there is no way to identify which bases to apply to the Phred score treamming
  ### therefor, the quality for each base is substitute for the mean qual
  ### converting phred quality scores to ASCII
  meanqual1 <- round(mean(qual1))
  meanqual2 <- round(mean(qual2))
  QualASCII1 <- paste(rep(unlist(as.character(phredTable()[phredTable()[,2]==meanqual1,3])), length(primarySeq(calls))), collapse = "")
  QualASCII2 <- paste(rep(unlist(as.character(phredTable()[phredTable()[,2]==meanqual2,3])), length(secondarySeq(calls))), collapse = "")
  
  ### old qual, don't work because of the unknown trimming of makeBase() it is impossible to predict which base has which phred
  # QualASCII1 <- paste(unlist(lapply(qual1[lengthdiff:length(qual1)-1], function(x){as.character(phredTable()[phredTable()[,2]==x,3])})), collapse = "")
  # QualASCII2 <- paste(unlist(lapply(qual2[lengthdiff:length(qual2)-1], function(x){as.character(phredTable()[phredTable()[,2]==x,3])})), collapse = "")
  
  ### creating a range of different interpretations of each read. Variation of different ratios that 
  ### represent median low quality variant calling to median and representing the best matches
  ### contains fastq name / sequence / plus sign / and qual ascii
  
  #Code if there is a reference file, else if there isn't. setAllelePhase is highly recomended for frameshifts.
  if(exists("range")==TRUE){
    Read1 <- lapply(range, function(x){
      c("@F", as.character(primarySeq(setAllelePhase(makeBaseCalls(reads, ratio= x),ref))), "+", QualASCII1)})
    Read2 <- lapply(range, function(x){
      c("@F", as.character(secondarySeq(setAllelePhase(makeBaseCalls(reads, ratio= x),ref))), "+", QualASCII2)})
  } else {
    Read1 <- lapply(range, function(x){
      c("@F", as.character(primarySeq(makeBaseCalls(reads, ratio= x))), "+", QualASCII1)})
    Read2 <- lapply(range, function(x){
      c("@F", as.character(secondarySeq(makeBaseCalls(reads, ratio= x))), "+", QualASCII2)})
  }
  
  
  ### writing the fastq
  write.table(c(paste(unlist(Read1),collapse = "\n"),paste(unlist(Read2),collapse = "\n")),
              file=paste(AbiFile,".fastq", sep=""),quote= F, sep = "\n", col.names = F, row.names = F)
}




SequenceQualityControl <- function(AbiFile){
  
  options(warn= -1)
  Abi <- read.abif(AbiFile)
  options(warn= 0)
  
  #Quality Check. If sequence does not have enough quality it will no proceed.
  if(is.null(Abi[3][[1]]$CRLn.2)){
    warning("Sequence without enough quality")
    return()
  }
  
  print(c(paste("Data de cria��o do arquivo: ",Abi[3][[1]]$RUND.4[3],"/",Abi[3][[1]]$RUND.4[2],"/",Abi[3][[1]]$RUND.4[1],
                "(",Abi[3][[1]]$RUNT.4[1],":", Abi[3][[1]]$RUNT.4[2],":",Abi[3][[1]]$RUNT.4[3],")", sep = ""),
          paste("Lote do Polimero:",Abi[3][[1]]$SMLt.1), paste("Lote do Anodo:",Abi[3][[1]]$ABLt.1),
          paste("Lote do Catodo:",Abi[3][[1]]$CBLt.1), paste("SN do Capilar:",Abi[3][[1]]$CASN.1)))
  
  ### phred quality run quality
  qual1 <- as.numeric(unlist(PhredQuality(Abi[3][[1]]$PCON.1)))
  qual2 <- as.numeric(unlist(PhredQuality(Abi[3][[1]]$PCON.2)))
  
  print(c(paste("Nome do arquivo:",Abi[3][[1]]$SMPL.1), paste("Nome do arquivo:",Abi[3][[1]]$SMPL.1), paste("Checagem de qualidade QV20:",Abi[3][[1]]$CRLn.2),
          paste("Qualidade m�dia da primeira leitura:", round(mean(qual1),2)),
          paste("Qualidade m�dia da segunda leitura:",round(mean(qual2),2)),
          paste("QV>=20%: ",round((sum(c(qual2,qual1)>=20)/length(c(qual2,qual1)))*100, 2),"%",sep = "")))
}    




apply(test, 1, function(x) { return(paste("{", names(test)[1],":",x[1],",", names(test)[2],":",x[2],",","}"))})



BiocStyle::latex()


library(knitr, quietly=TRUE)
library(sangerseqR, quietly=TRUE)
library(Biostrings, quietly=TRUE)
opts_chunk$set(tidy=TRUE)

hook_output = knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  n <- 90
  x <- knitr:::split_lines(x)
  # any lines wider than n should be wrapped
  if (any(nchar(x) > n)) {
    x <- gsub(sprintf('(.{%d})', n), "\\1\n## ", x)
  }
  
  hook_output(x, options)
})

hetab1 <- read.abif(system.file("extdata", 
                                AbiFile, 
                                package="sangerseqR"))






hetsangerseq <- read.abif("2124963 CARLA PTCH1 1F_B11.ab1")
hetsangerseq <- readsangerseq(system.file("extdata", "heterozygous.ab1", package = "sangerseqR"))


str(hetsangerseq)
hetcalls <- makeBaseCalls(hetsangerseq, ratio = 0.33)

chromatogram(hetcalls, width = 100, height = 2, trim5 = 50, trim3 = 100, showcalls = "both",
             filename = "chromatogram2.pdf")





## ref cordinates for "1wt.ab1"
chr <- 1
chrStart <- 57832634
chrEnd <- 57832823

ReferenceFastaFile <- function(chr, chrStart, chrEnd, strand = "+"){
  if(chrStart > chrEnd | strand == "-"){
    temp <- chrStart
    chrStart <- chrEnd
    chrEnd <- temp
  }
  if(strand == "-"){
    temp <- chrStart
    chrStart <- chrEnd
    chrEnd <- temp
  }
  fasta <- read.fasta(paste("http://togows.org/api/ucsc/hg19/chr",chr,":",chrStart,"-",chrEnd,".fasta", sep = ""), as.string = TRUE)
  write.fasta(fasta[[1]][1],paste(names(fasta)),gsub(":","-", paste(names(fasta),".fasta", sep="")))
  
}

ReferenceFastaFile(chr,chrStart,chrEnd)


reads <- tryCatch(readsangerseq("B08_FF_05.ab1"), error=function(e) NULL )
hetcalls <- makeBaseCalls(reads, ratio = 0.33)

ref <- read.fasta("hg19-chr1-57832634-57832823.fasta", as.string = TRUE)[[1]][1]
ref2<- reverseComplement(DNAString(ref))

size1 <- nchar(primarySeq(hetcalls, string = TRUE))
size2 <- nchar(secondarySeq(hetcalls, string = TRUE))

hetseqalleles <- setAllelePhase(hetcalls, ref2)

pa <- pairwiseAlignment(ref2, secondarySeq(hetseqalleles)[1:size2],type = "global-local")
pa2 <- pairwiseAlignment(ref2, primarySeq(hetseqalleles)[1:size1], type = "global-local")
pa3 <- pairwiseAlignment(primarySeq(hetseqalleles)[1:size1], secondarySeq(hetseqalleles)[1:size2],type = "global-local")

writePairwiseAlignments(pa,block.width=150)
writePairwiseAlignments(pa2,block.width=150)

chromatogram(hetcalls, width = 100, height = 2, showcalls = "both", filename = "chromatogram2.pdf")

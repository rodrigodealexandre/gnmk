
CreatePrimerToGenSoft <- function(x){
    options(error=NULL)
    copieddata<-readClipboard()
    if(length(grep(">",copieddata))!=0){
        y<-strsplit(copieddata[grep(">",copieddata)], NULL)
    }else{
        stop("Texto copiado invalido")
    }
    if(length(grep("Forward",copieddata))!=0){
        z<-strsplit(copieddata[grep("Forward",copieddata)], NULL)
    }else{
        stop("Texto copiado invalido")
    }
    if(length(grep("Reverse",copieddata))!=0){
        z2<-strsplit(copieddata[grep("Reverse",copieddata)], NULL)
    }else{
        stop("Texto copiado invalido")
    }
    
    result<- paste(c(paste(y[[1]][(grep(" ", y[[1]])+1)[2]:(grep(" ", y[[1]])-1)[3]], collapse = ""), 
                     paste(y[[1]][(grep(" ", y[[1]])+1)[3]:length(y[[1]])], collapse = ""), 
                     paste(z[[1]][(grep(" ", z[[1]])+1)[2]:(grep(" ", z[[1]])-1)[3]], collapse = ""), 
                     paste(z2[[1]][(grep(" ", z2[[1]])+1)[1]:(grep(" ", z2[[1]])-1)[2]], collapse = ""), 
                     paste(y[[1]][(grep(":",y[[1]])+1):(grep("\\+|\\-",y[[1]])-1)], collapse = ""),
                     paste(y[[1]][(grep("\\+|\\-",y[[1]])+1):(grep(" ",y[[1]])-1)[1]], collapse = ""),
                     "","","","", paste(y[[1]][2:(grep(":",y[[1]])-1)], collapse = ""),"",""), sep = "")
    return(t(result))
}

write.table(CreatePrimerToGenSoft(), "clipboard", sep="\t", row.names=FALSE,col.names=FALSE)
    


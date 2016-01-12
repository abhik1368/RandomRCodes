source("http://bioconductor.org/biocLite.R")
biocLite("seqinr")
library("seqinr")
library(Biostrings)
data(BLOSUM50)
fas<-read.fasta(file = "Sequence_Approved.fasta",seqtype = "AA",as.string = TRUE)
fas[[1]][1]
seqnames<-c()

for ( n in 1:length(fas)){
  q<-attr(fas[[n]],"name")
  seqnames<-rbind(seqnames,q)
}
  
errind<-c()  
M.seq<-matrix(0,length(fas),length(fas))
for ( i in 1:length(fas)){
  for ( j in 1:length(fas)){
    tryCatch({
    s<-pairwiseAlignment(fas[[i]][1], fas[[j]][1],substitutionMatrix = "BLOSUM62",gapOpening = 0, 
                         gapExtension = -8,scoreOnly = TRUE,type="local")
    M.seq[i,j]<-s
      },
    error = function(ex) {
      print (ex ,j);
      errind<-rbind(errind,j) ;
      },
    finally = {
      cat("done\n");
      })
  }
}  

r<-read.csv("seqsim.csv",header=TRUE,row.names=1)

dt<-read.csv("test_convert.csv",header=TRUE,row.names=1)

d<-matrix(0,dim(x)[1],dim(x)[1])

for (i in 1:nrow(x)){
    for (j in 1:nrow(x)){

        d[i,j] = x[i,j]/(sqrt(x[i,i])%*%sqrt(x[j,j]))
   }
}
dim<-read.csv("drugsim.csv",header=TRUE,row.names=1)

dt<-read.csv("test_convert.csv",header=FALSE)
nondup<-data.frame()
dupd<-data.frame()
for (d in 1:dim(dt)[1]){
  n1<-as.character(dt[d,1])
  n2<-as.character(dt[d+1,1])
  if (n1==n2) {
    print (d)
    dr<-data.frame()
    r1<-dt[d,1:dim(dt)[2]]
    r2<-dt[d+1,1:dim(dt)[2]]
    dr<-rbind(r1,r2)
    c<-colSums(dr[,2:1772],na.rm=TRUE)
    dfin<-cbind(as.character(dr[1,1]),as.data.frame(t(c)))
    dupd<-rbind(dupd,dfin)
  }
  else{ 
    nondup<-rbind(nondup,dt[d,])
    next;}

}

## Adjacency list to pairs fast methods

dt <- read.csv("drugtarget.csv",header=TRUE)
d <- dt$Drugs
s <- strsplit(as.character(d), ';')
dft <- data.frame(drugs=unlist(s), proteins=rep(dt$UniProt.ID, sapply(s, FUN=length)))

write.csv(dtf,"drugtargetbinary.csv")
write.csv(d,"sequencesim.csv")

d<-as.data.frame(nondup)
names<-as.character(dupd[,1])

a<-d[!rownames(d) %in% names,]
du<-as.data.frame(dupd)
rownames(du)<-as.character(dupd[,1])
xx<-rbind(du,a)
write.csv(xx,"drugtargetbin.csv")

dt-dim[idx,]
names <- rownames(xx)
sub<-dim[rownames(dim) %in% names,rownames(dim) %in% names] # write chemical sim csv
subdt<-xx[rownames(sub),]   

write.csv(subdt,"subdt.csv")

ss<-read.csv("sequencesim.csv",header=TRUE,row.names=1)
subdt<-read.csv("subdt.csv",header=TRUE,row.names=1)
subdt<-subdt[,colnames(ss)] # drug target data 
write.csv(subdt,"drugtargetbin.csv")
write.csv(sub,"chemicalsimilarity.csv")

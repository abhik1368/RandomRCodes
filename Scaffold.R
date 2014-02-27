library(rcdk)
library(RColorBrewer)
library(gplots)
library(pheatmap)

#read in the test dataset of compounds and targets
data<-read.csv("testKinase.csv",header=T)

#parse the smiles from the compounds
Smiles<-lapply(as.character(data[,2]),parse.smiles)

#Generate murcko scaffolds
frags<-lapply(Smiles,get.murcko.fragments,single.framework = TRUE)

#unlisting
fworks<-lapply(frags,function(x) x[[1]]$frameworks)
frag.freq<-data.frame(table(unlist(fworks)))
frag.freq$index<-1:nrow(frag.freq)
#build a look up table
dat<-data.frame()
for ( i in 1:length(fworks))
{
  if(length(fworks[i])>=1)
  {
    set<-cbind(fworks[[i]][1],data$PUBCHEM_SID[i])
    dat<-rbind(dat,set)
  }
}

colnames(dat)<-c("scaffold","mid")
#Query scaffold
query<-'c1nc(nc(c1)c3c[nH]c2ncccc23)NC4CCCCC4'

#Subset molecules from the main set with a given scaffold
ids<-subset(dat,scaffold == query)
depvs<-subset(data,PUBCHEM_SID %in% ids$mid)[,3:174]
depvs<-cbind(pubchemid=ids$mid,depvs)

#For pedagogical purposes converting blank activity values to 0
depvs[is.na(depvs)]<-0

#Plotting the heat maps of compound id's and Targets
heatmap.2(as.matrix(depvs[,2:50]), dendrogram="col",col=redgreen(75),cexRow=0.9,scale="none",density.info="none", trace="none",labRow=depvs[,1])

rownames(depvs)<-depvs[,1]
pheatmap(as.matrix(depvs[,2:50]),show_rownames=T,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))


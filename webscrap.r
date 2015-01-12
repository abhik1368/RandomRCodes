install.packages("rvest")
library(rvest)


#minx <- html("http://www.drugs.com/sfx/minoxidil-side-effects.html")
#url <-html("http://www.drugs.com/sfx/L-Arginine-side-effects.html")

#for (j in 5:7){
#st<-html_nodes(acet,'ul') %>%
#       html_text()
#d<-strsplit(st,"[\n]")
#s<-html_nodes(minx,'i')

readUrl <- function(url) {
  out <- tryCatch(
{
  message("This is the 'try' part")  
  html(url) 
},
error=function(cond) {
  message(paste("URL does not seem to exist:", url))
  message(cond)
  # Choose a return value in case of error
  return(NA)
},
warning=function(cond) {
  message(paste("URL caused a warning:", url))
  message(cond)
  # Choose a return value in case of warning
  return(NULL)
},
finally={
  message(paste("Processed URL:", url))
}
  )
return(out)
}

sdname<-function(durl,drugname){
    # get the right url
    #durl<-drugurl
    seff<-data.frame()
    
    d<- durl%>% html_nodes(xpath="//ul") 
    seff<-data.frame()
    for ( i in 1:length(d)){
      s<-toString.XMLNode(d[[i]])
      l=length(grep("href",s))
      if (l==1) {next ;}
      else {
        st<-html_nodes(durl,'ul') %>% .[[i]] %>%
          html_text()
        if (length(st) > 0 ){
          se<-strsplit(st,"[\n]")
          #print (data.frame(se[[1]]))
          seff<-rbind(seff,data.frame(se[[1]]))
          colnames(seff)[1]<-"Adverse effects"
          seff$drug<-drugname
        }
        else {
        seff<-rbind(seff,"None")
        colnames(seff)[1]<-"Adverse effects"
        seff$drug<-drugname
        return(seff)
      }   
    
    }
    return(seff)
  }
}

# side effect dataframe
sideeffect<-data.frame()

# Read the drugname  list 
drug.Name<-read.csv("drug_links.csv",header=TRUE,row.names=2,stringsAsFactors=FALSE)

# Check the names
drugnames<-rownames(drug.Name)

for (dn in 1:length(drugnames)){

  # Create the URL 
  url<-paste("http://www.drugs.com/sfx/",toString(tolower(drugnames[dn])),"-side-effects.html",sep = "")
  #url<-"http://www.drugs.com/sfx/cyanocobalamin-side-effects.html"
  durl<-readUrl(url)
  if (class(durl)[1] == "HTMLInternalDocument"){
     sd <- sdname(durl,drugnames[dn])
     sideeffect <- rbind(sideeffect,sd)
  }  
   else {next ;}
}


install.packages('weatherData')
library(weatherData)
d<-getWeatherForDate("SEA", "2014-05-05")
getWeatherForDate("SEA", "2014-01-011", opt_detailed=TRUE)
getWeatherForYear("KOLKATA", 2014)


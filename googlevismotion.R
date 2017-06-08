## Provide URLs
data03.09 <- paste("http://www.drugs.com/top200_", 2003:2009, ".html", sep = "")
data10<- "http://www.drugs.com/top200.html"
data11.13 <- paste("http://www.drugs.com/stats/top100/", 2011:2013, "/sales", sep = "")


## Load XML package
library(XML)

## Import 2003-2010 data
drugs03.10 <-
  lapply(c(data03.09, data10),
         function(url) {
           tab.imported <- readHTMLTable(url, header = FALSE, skip.rows = 1)[[2]][-201,1:4]
           
           names(tab.imported) <- c("rank","name","company","sales")
           
           tab.imported <- within(tab.imported, {
             rank <- seq_along(rank)
             sales <- as.numeric(gsub(",", "", sales)) * 1000
           })
           tab.imported
         })


## Import 2011-2013 data (different format)
drugs11.13 <-
  lapply(data11.13,
         function(url) {
           tab.imported <- readHTMLTable(url)[[2]][,1:3]
           
           names(tab.imported) <- c("rank", "name", "sales")
           
           tab.imported <- within(tab.imported, {
             rank <- seq_along(rank)
             sales <- as.numeric(gsub(",", "", sales)) * 1000
           })
         })


## Extract company names from drug.2003.2010 dataset
companies <- unlist(sapply(drugs03.10, "[", "company"))
names(companies) <- NULL
companies <- sort(unique(companies))
companies <- companies[!companies == ""]

## Other company names found in drug.2011.2013 dataset not in 03_10 set
other.companies <- c("Otsuka Pharmaceutical Co.","Wyeth","Novo Nordisk, Inc.","Daiichi Sankyo","AbbVie, Inc.","Generic Drug", "Vertex Pharmaceuticals", "MedImmune, Inc", "Sunovion Pharmaceuticals Inc.")

## Removal of company names from drugs2011.2013 dataset (drug name and company name mixed)
replace.pattern <- paste(companies, other.companies, collapse = "|", sep = "|")

drugs11.13 <-
  lapply(drugs11.13,
         function(y) {
           y$name <- gsub(replace.pattern, "", y$name)
           y
         })

## Save data for backup
save(list = c("drugs03.10","drugs11.13"), file = "~/drugs.com.03.2013.RData")




## Load data
load(file = "~/drugs.com.03.2013.RData")

## Create data frames
df.drugs2003.2010 <- do.call(rbind, drugs03.10)
df.drugs2011.2013 <- do.call(rbind, drugs11.13)

## Add year variable
df.drugs03.10$year <- rep(2003:2010, rep(200, 8))
df.drugs11.13$year <- rep(2011:2013, rep(100, 3))

## Create drug-company table
drug.company.table <- unique(df.drugs03.10[,c("name","company")])

## Merge company name to 2011-2013 dataset
df.drugs11.13 <-
  merge(x = df.drugs11.13,
        y = drug.company.table,
        all.x = TRUE)

## All datasets combined as
df.drugs03.13 <- rbind(df.drugs03.10[,c("year","rank","name","company","sales")],
                           df.drugs11.13
)

## Order by year and ranking
library(doBy)
df.drugs03.13 <- orderBy(data = df.drugs03.13, ~ +year +rank)


## Extract drugs that entered top 10 at some point
ever.in.top.group <- subset(df.drugs03.13, rank %in% 1:10)$name
data.ever.in.top.group <- subset(df.drugs03.13, name %in% ever.in.top.group)

## Give 1 if sales is NA
data.ever.in.top.group[is.na(data.ever.in.top.group$sales), "sales"] <- 1

## Extract data in 2013 for labeling
last.year.top.group <- subset(data.ever.in.top.group, year == 2013)

library(reshape2)
drug.table <- dcast(data = df.drugs03.13, rank ~ year, value.var = "name")[1:10,]

#Check out the top 10 drugs by year by sales
drug.table

data.ever.in.top.group$year <- as.Date(paste(sep = "", data.ever.in.top.group$year, "-01-01"))

library(googleVis)
res <- gvisMotionChart(data.ever.in.top.group, idvar = "name", timevar = "year",options=list(width = 800 , height = 500))
plot(res)





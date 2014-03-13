#Taken from http://flo.nigsch.com/
# RFScript.R - Random forest classification in R
#
# Originally written by Florian Nigsch <fn211@cam.ac.uk>
# If you redistribute this script please leave that little reference
# to my humble self in here. Thank you!
#
# Thanks go to Max Kuhn <Max.Kuhn .at. pfizer.com> and
# Jim Holtman <jholtman .at. gmail.com>who provided some hints
# on how to get randomForest() to work with large datasets.
#
# Reads in a file and performs a certain number of runs to classify
# the contents. Training and test set are randomly drawn from the total
# data set provided.
# Parameters to specify:
#   RUNS -- Numer of runs that should be performed
#   percentage_train -- Percentage of how much of the data should be used
#            for training. The rest is used as a test set.
# Parameter DATAREADIN can be used if the data is already read in.
# Format of the data file: all fields are separated by semicolons, the first
# column contains the label, all others constitute the variables. In this script
# the number of variables is hardcoded (each and every occurence of 184/185) and
# will have to be changed if the dataset contains more variables than that. This can
# now be done with the variable NUMBEROFVARIABLES (which includes the label!)
# The same goes for the number of classes, limited to 12 here, see results matrix.

###########
# IMPORTANT: NUMBEROFLINES has to be specified for an efficient reading in of the
# datafile!
###########

fn_data <- "~/models/binvec"
basedir_results <- "~/models/Robj/"
outfile_comment <- "mycomment"

RUNS <- 10
percentage_train <- 0.8
NUMBEROFVARIABLES <- 166
BUFFER <- 5000
NUMBEROFLINES <- 10000
#NUMBEROFLINES <- 100000

DATAREADIN <- FALSE

# To make the reading in of the data more memory efficient, read in 5 lines,
# process them, and append these lines to a dataframe that will be used in
# the end.
# For this we need to know the number of lines in the input file first.

SKIP <- 0

df <- data.frame()

if (!DATAREADIN) {
	print("Reading data...")
	types <- list("character")
		for (i in 1:(NUMBEROFVARIABLES-1)) {
			types[[i+1]] <- "integer"
	}
	while (SKIP < NUMBEROFLINES) {
		# Read in BUFFER lines
		if (!(SKIP%%BUFFER)) {
			print(paste("Fraction processed:",SKIP/NUMBEROFLINES), digits=4)
		}
		thedata <- scan(fn_data, sep=';', what=types, skip=SKIP, nlines=BUFFER, fill=TRUE)
		SKIP <- SKIP + BUFFER
		tmpdf <- data.frame(factor(thedata[[1]]))
		for (i in 1:(NUMBEROFVARIABLES-1)) {
			tmpdf <- cbind(tmpdf, as.double(thedata[[i+1]]))
		}
		df <- rbind(df, tmpdf)
	}
	rm(tmpdf)
	rm(thedata)
	
} else {
	print("Data is already read in.")
}

print("Naming dataframe...")
names(df) <- paste("V",1:NUMBEROFVARIABLES, sep="")
print("Done. Beginning RandomForesting...")

#####################
##### FUNCTIONS #####
#####################
# Functions to calculate the MCC
CalcMCC <- function(tp, tn, fp, fn) {
	if (tp == 0) {
		tp <- 0.1
	}
	if (fp == 0) {
		fp <- 0.1
	}
	nom <- (tp*tn)-(fp*fn)
	denom <- sqrt((tp+fn))*sqrt((tp+fp))*sqrt((tn+fp))*sqrt((tn+fn))
	return(nom/denom)
}

###########################################

classes <- unique(df[,1])
tablist <- list()
cputimelist_tr <- list()
cputimelist_te <- list()

# Make sure that the randomForest library is loaded.
library(randomForest)

# The following part can be repeated as many times
# as required, just surround with a for loop.

for (RUN in 1:RUNS) {

	trainindices <- c()
	testindices <- c()

	# Now get the 80% for the training set and 20% test set
	for (class in classes) {
		print(paste("Run",RUN,": Training set for class", classes[class]))
		classindices <- which(df[,1]==classes[class])
		num <- length(classindices)
		print(paste(RUN,"Total instances", num))
		indices <- sample(num, num)
		last_tr <- floor(num*percentage_train)
		first_te <- last_tr+1
		trainindices <- c(trainindices,classindices[1:last_tr])
		testindices <- c(testindices, classindices[first_te:num])
	}
	
	# Run the actual random forest stuff
	cputimelist_tr[[RUN]] <- system.time(rf <- randomForest(x=df[trainindices,-1],y=df[trainindices,1],xtest=df[testindices,-1],ytest=df[testindices,1], do.trace=5, ntree=500))
	print("Random forest learned. Starting predictions.")
	cputimelist_te[[RUN]] <- system.time(rf.pred <- predict(rf, df[testindices,-1]))
	print("All predictions done.")
	
	# Get confusion matrix and write to disk
	tab <- table(df[testindices,1], rf.pred)
	tablist[[RUN]] <- tab
	fn_predfile_tab <- paste(basedir_results,"pred-tab-",RUN,"-",outfile_comment, ".Robj", sep="")
	print(fn_predfile_tab)
	write.table(tab, fn_predfile_tab)

} # end of repeat for several runs.

fn_predfile_tablist <- paste(basedir_results,"pred-tablist-",outfile_comment,".Robj", sep="")
write.table(tablist, fn_predfile_tablist)

###################
# GET ALL RESULTS #
###################

# Now get the aggregated results per class
tottab <- tablist[[1]]
for (tabid in 2:RUNS) {
	tottab <- tottab + tablist[[tabid]]
}
tab <- tottab
tmp <- attr(tab, 'dimnames')
Ntot <- sum(tab)

Classes <- tmp[[1]]

i <- 1
Classes_Real <- list()
for (class in tmp[[1]]) {
	Classes_Real[[class]] <- i
	i <- i + 1
}

i <- 1
Classes_Predict <- list()
for (class in tmp[[2]]) {
	Classes_Predict[[class]] <- i
	i <- i + 1
}

res <- array(0, dim=c(13,9))
res <- as.data.frame(res)
for (class in Classes) {
	Index_Real <- Classes_Real[[class]]
	Prediction_Index <- Classes_Predict[[class]]
	Class_Total <- sum(tab[Index_Real,])
	
	if ( any(Prediction_Index) ) {
		Class_Correct <- tab[Index_Real,Prediction_Index]
		Class_FN <- sum(tab[Index_Real,-Prediction_Index])
		Class_FP <- sum(tab[-Index_Real,Prediction_Index])
		Class_TN <- Ntot - (Class_Correct+Class_FP+Class_FN)
	} else {
		Class_Correct <- 0
		Class_FN <- sum(tab[Index_Real,])
		Class_FP <- 0
		Class_TN <- Ntot - (Class_Correct+Class_FP+Class_FN)
	}
	Class_TP <- Class_Correct
	#if (Class_TP == 0)
	if (any(Class_Correct)) {
		Recall <- (Class_TP/(Class_TP+Class_FN))
		Precision <- (Class_TP/(Class_TP+Class_FP))
	} else {
		Recall <- 0
		Precision <- 0
	}
	res[Index_Real,1] <- class
	res[Index_Real,2] <- Class_Total
	res[Index_Real,3] <- Class_TP
	res[Index_Real,4] <- Class_TN
	res[Index_Real,5] <- Class_FN
	res[Index_Real,6] <- Class_FP
	res[Index_Real,7] <- Recall
	res[Index_Real,8] <- Precision
	res[Index_Real,9] <- CalcMCC(Class_TP, Class_TN, Class_FP, Class_FN)

}
names(res) <- c("Class","Cases", "TP", "TN", "FN","FP","Recall_p", "Precision_p", "MCC")
options(digits=4)
Tot_MCC <- CalcMCC( sum(res[,3]), sum(res[,4]), sum(res[,6]), sum(res[,5]) )
Tot_TP <- sum(res[,3])
Tot_TN <- sum(res[,4])
Tot_FN <- sum(res[,5])
Tot_FP <- sum(res[,6])
Tot_Recall_p <- Tot_TP/(Tot_TP+Tot_FN)
Tot_Precision_p <- Tot_TP/(Tot_TP+Tot_FP)
Tot_Recall_n <- Tot_TN/(Tot_TN+Tot_FP)
Tot_Precision_n <- Tot_TN/(Tot_TN+Tot_FN)
Tot_APCC <- (Tot_Recall_p+Tot_Recall_n)/2
print(paste("Total MCC:",Tot_MCC))
FCP <- sum(diag(tottab))/sum(tottab)
res[13,1] <- "FCP"
res[13,9] <- FCP
print(res)
print(paste("FCP is", FCP))
fn_results <- paste(basedir_results,"all-classes-",outfile_comment,".Robj", sep="")
write.table(res,fn_results)

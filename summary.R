summaryStats <- function(confusionMatrix) {
        TP <- confusionMatrix[1]
        TN <- confusionMatrix[4]
        FN <- confusionMatrix[2]
        FP <- confusionMatrix[3]
        total <- TP + TN + FN + FP
        if (TP + FN > 0) {
                sensitivity <- TP / (TP + FN) 
        }
        else {
                sensitivity <- 0.0
        }
        if (TN + FP > 0) {
                specificity <-  TN / (TN + FP)
        }
        else {
                specificity <- 0.0
        }
        if (TP + FP > 0) {
                positivePredictiveValue <- TP / (TP + FP)
        }
        else {
                positivePredicitveValue <- 0.0;
        }
        if (TN + FN > 0) {
                negativePredictiveValue <- TN / (TN + FN)
        }
        else {
                negativePredictiveValue <- 0.0;
        }         
        falsePositiveRate <- 1 - positivePredictiveValue
        falseNegativeRate <- 1 - negativePredictiveValue
        accuracy <- (TP + TN)/total
        expectedAgreement <- ((TP+FN)*(TP+FP)+(FN+TN)*(FP+TN))/(total*total)
        kappa <- (accuracy - expectedAgreement)/(1.0 - expectedAgreement)
        print(confusionMatrix)
        cat("sensitivity    ",round(sensitivity,digits=2),            "     specificity          ",round(specificity,digits=2),"\n")
        cat("PPV            ",round(positivePredictiveValue,digits=2),"     NPV                  ",round(negativePredictiveValue,digits=2),"\n")
        cat("FP Rate        ",round(falsePositiveRate,digits=2),      "     FN Rate              ",round(falseNegativeRate,digits=2),"\n")
        cat("Accuracy       ",round(accuracy,digits=2),               "     Expected Agreement   ",round(expectedAgreement,digits=2),"\n")
        cat("Kappa          ",round(kappa,digits=2),"\n")
}

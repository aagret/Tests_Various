
library (Rcpp)

cumsum_bounded <- cppFunction(
    'NumericVector cumsum_bounded (NumericVector x, const double lower, const double upper) {
    
    double acc = 0;
    NumericVector result(x.size());
    
    for(int i = 0; i < x.size(); i++) {
    acc += x[i];
    
    if (acc < lower) acc = lower;
    if (acc > upper) acc = upper;
    
    result[i] = acc;
    }
    
    return result;
    }')

position <- function(x, min, max){
    pos <- x
    for (ii in 2:ncol(x)){
        tmp <- cumsum_bounded(x[,ii], min, max)
        pos[,ii] <- tmp
    }
    colnames(pos) <- gsub("Sig","Pos",colnames(pos))  
    return(pos)
}



library(TTR)
library(quantmod)

stk <- c("AAPL", "SPY", "CAT")
getSymbols(stk)

dys <- c(5, 30)

rsi<- xts()

for (d in dys) {
    
    rsi <- cbind(RSI(CAT[,4], n=d, maType="EMA", wilder=TRUE), rsi)
    
}

SPY$rsi <- rowMeans(rsi)
SPY$vol <- volatility(SPY[,4], n= 30) * 100
SPY$oc  <- log(SPY[,4] / SPY[,2])
SPY$roc <- ROC(SPY[,4])

SPY$sig <- ifelse(SPY[,7] > (100 - (SPY[,8] /2)), -1, 0)
SPY$sig <- ifelse(SPY[,7] < (100 - (SPY[,8] /2)),  1, SPY$sig)
SPY$sig <- lag(SPY$sig)

SPY[is.na(SPY)] <- 0

SPY$pos <- cumsum_bounded(SPY$sig, -2, 3)
SPY$PnL <- ifelse(diff(SPY$sig)==0, SPY$roc, SPY$oc) * SPY$pos
SPY$PnL[1] <- 0
SPY$PnL <- exp(cumsum(SPY$PnL))


plot(exp(cumsum(SPY[,10])), type="l")
lines(SPY[, 13], col="red")
     

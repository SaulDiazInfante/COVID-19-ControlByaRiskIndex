computeCost <- function(x, u, par){
    xS <- x[1]
    xI <- x[2]
    xV <- x[3]
    xR <- x[4]
    xC <- x[5]
    u_beta <- u[1]
    u_k <- u[2]
    # 
    a_I <- as.numeric(par["costWeights.a_I"])
    a_C <- as.numeric(par["costWeights.a_I"])

    yld <- a_I * xI
    c_risk <- a_C * x_C

}
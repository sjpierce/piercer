# NON-LINEAR SEM COMPOSITE RELIABILITY COEFFICIENT
# Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
#   structural equation modeling: An alternative to coefficient alpha.
#   Psychometrika, 74(1), 155-167. doi:10.1007/s11336-008-9099-3

# Yang, Y., & Green, S. B. (2015). Evaluation of structural equation modeling
#   estimates of reliability for scales with ordered categorical items.
#   Methodology: European Journal of Research Methods for the Behavioral and
#   Social Sciences, 11(1), 23-34. doi:10.1027/1614-2241/a000087
#   http://myweb.fsu.edu/yyang3/yang.html
#   http://myweb.fsu.edu/yyang3/files/examplecodes.txt  (SAS code)

# Custom function to compute Green & Yang's (2009) nonlinear SEM reliability
# coefficient from CFA with ordinal indicators. Adapted from Yang & Green's
# (2015) SAS macro by Steven J. Pierce. This implements Green & Yang's (2009)
# Equation 21, which is also shown as Yang & Green's (2015) Equation 9.

# If using WLSMV estimation with latent variance fixed to 1 and all indicator
# loadings freely estimated, then unstandardized & standardized estimates are
# identical (you can use either one as input to this function). However, if you
# are using Bayesian estimation with PX parameterization, use the STDYX
# estimates as the inputs here!

rxx.NL <- function(lambda, tau, fcor = matrix(1, ncol = 1), digits = NULL) {
        # Arguments:
        #   lambda: factor loading matrix (items x factors)
        #   tau: threshold matrix (items x thresholds, same set for all items)
        #   fcor: latent factor correlation matrix (factors x factors)
        #         defaults to a 1 x 1 matrix: [cor = 1]
        #   digits: Number of digits to be used in rounding the results
            require(pbivnorm)   # for bivariate normal distribution, pbivnorm()
            NTHRESH <- ncol(tau)
            NCAT    <- NTHRESH + 1
            NITEM   <- nrow(lambda)
            NFACT   <- ncol(lambda)
        # Compute polychoric correlation matrix & set diagonals to 1.
            POLYR   <- lambda %*% fcor %*% t(lambda)
            diag(POLYR) <- 1
        # Compute numerator & denominator (strictly parallels the SAS code)
            sumnum <- 0
            addden <- 0
            for(j in 1:NITEM) {
             for(jp in 1:NITEM) {
              sumprobn2 <- 0
              addprobn2 <- 0
              for(c in 1:NTHRESH) {
               for(cp in 1:NTHRESH) {
                   sumrvstar <- 0
                      for(k in 1:NFACT) {
                          for(kp in 1:NFACT){
                              sumrvstar <- sumrvstar + lambda[j,k]*lambda[jp,kp]*fcor[k,kp]}}
                      sumprobn2 <- sumprobn2 + pbivnorm(tau[j,c], tau[jp,cp], sumrvstar)
                      addprobn2 <- addprobn2 + pbivnorm(tau[j,c], tau[jp,cp], POLYR[j,jp])}}
              sumprobn1  <- 0
              sumprobn1p <- 0
              for (cc in 1:NTHRESH){
                 sumprobn1  <- sumprobn1 + pnorm(tau[j,cc])
                 sumprobn1p <- sumprobn1p + pnorm(tau[jp,cc])}
             sumnum <- sumnum + (sumprobn2 - sumprobn1*sumprobn1p)
             addden <- addden + (addprobn2 - sumprobn1*sumprobn1p)}}
        # Compute NL-SEM reliability
            rxx <- data.frame(Numerator = sumnum,
                              Denominator = addden,
                              r.NL = sumnum/addden)
        # Round result if digits is specified via a number
            if(is.numeric(digits)) rxx <- round(rxx, digits = digits)
            return(rxx)}

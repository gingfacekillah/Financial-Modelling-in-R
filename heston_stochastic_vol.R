#### Heston Stochastic Volatility for Options Pricing
#### From NMOF package, by Enrico Schumann
### Saving for later for experimentation


callHestoncf <- function(S, X, tau, r, q, v0, vT, rho, k, sigma,
                         implVol = FALSE, ...) {
    ## S     = spot
    ## X     = strike
    ## tau   = time to mat
    ## r     = riskfree rate
    ## q     = dividend yield
    ## v0    = initial variance
    ## vT    = long run variance (theta in Heston's paper)
    ## rho   = correlation
    ## k     = speed of mean reversion (kappa in Heston's paper)
    ## sigma = vol of vol
    ## implVol = compute equivalent BSM volatility?

    if (sigma < 0.01)
        sigma <- 0.01
    P1 <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        p <- Re(exp(-1i * log(X) * om) *
                cfHeston(om - 1i, S, tau, r, q, v0, vT, rho, k, sigma) /
                (1i * om * S * exp((r-q) * tau)))
        p
    }
    P2 <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
        p <- Re(exp(-1i * log(X) * om) *
                cfHeston(om  ,S,tau,r,q,v0,vT,rho,k,sigma) /
                (1i * om))
        p
    }
    cfHeston <- function(om,S,tau,r,q,v0,vT,rho,k,sigma) {
        d <- sqrt((rho * sigma * 1i * om - k)^2 + sigma^2 *
                  (1i * om + om ^ 2))
        g <- (k - rho * sigma * 1i * om - d) /
            (k - rho * sigma * 1i * om + d)
        cf1 <- 1i * om * (log(S) + (r - q) * tau)
        cf2 <- vT*k/(sigma^2)*((k - rho * sigma * 1i * om - d) *
                               tau - 2 * log((1 - g * exp(-d * tau)) / (1 - g)))
        cf3 <- v0 / sigma^2 * (k - rho * sigma * 1i * om - d) *
            (1 - exp(-d * tau)) / (1 - g * exp(-d * tau))
        cf  <- exp(cf1 + cf2 + cf3)
        cf
    }

    ## pricing
    vP1 <- 0.5 + 1/pi * integrate(P1, lower = 0, upper = Inf,
                                  S, X, tau, r, q, v0, vT, rho, k, sigma, ...)$value
    vP2 <- 0.5 + 1/pi * integrate(P2, lower = 0, upper = Inf,
                                  S, X, tau, r, q, v0, vT, rho, k, sigma, ...)$value
    result <- exp(-q * tau) * S * vP1 - exp(-r * tau) * X * vP2;

    ## implied BSM vol
    if (implVol) {
        diffPrice <- function(vol,call,S,X,tau,r,q){
            d1 <- (log(S/X)+(r - q + vol^2/2)*tau)/(vol*sqrt(tau))
            d2 <- d1 - vol*sqrt(tau)
            callBSM <- S * exp(-q * tau) * pnorm(d1) -
                X * exp(-r * tau) * pnorm(d2)
            call - callBSM
        }
        impliedVol <- uniroot(diffPrice, interval = c(0.0001, 2),
                              call = result, S = S, X = X,
                              tau = tau, r = r, q = q)[[1L]]
        result <- list(value = result, impliedVol = impliedVol)
    }
    result
}

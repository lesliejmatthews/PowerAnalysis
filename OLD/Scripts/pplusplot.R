## This function generates a plot showing the relationship between
## PPlus and GMratio

## This function is used to generate Chapter 13: Figure 5
## revised June 17, 2018 EJG
pplusplot <- function(georat, std1,std2) {
    ## Natural logarithm of GMratio
    lnrat <- log(georat)
    
    ## Pooled standard deviation
    stdall <- sqrt(std1^2 + std2^2)
    
    ## PPlus
    pplus <- pnorm(c(0), mean = lnrat, sd = stdall, lower.tail = FALSE)
    
    ## Make a data frame that holds the GMratios and PPlus values
    RESULTS <- data.frame(Gmratio = georat, Pplus = pplus)
    
    ## Print this data frame
    print(RESULTS)
    
    ## Set up formatting for figure
    par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
    
    ## Plot relationship between PPlus and GMratio
    plot(Pplus ~ Gmratio, type = "p", xlab= "Gmratio", cex = 1.2,
         ylab = "Pplus", data = RESULTS, cex.axis = 1.3, cex.lab = 1.3)
    #title("PPlus vs GMratio for molybdenum data of chapter5")
}

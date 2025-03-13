## Lab #2 -- Temporal Processes


## 1. (a) Run an OLS regression, including at least one independent variable and a time variable (as dummies).  Explain how you think your independent variable relates to your dependent variable.  Interpret your results.  Did you find what you expected to find?    
## (b) Then run a fixed effect model version of that OLS model.  Interpret your results.  Did you find what you expected to find?  Why?  Why not? 
## (c) Then include an additional predictor in your fixed effects model that you think might account for the initial relationship you found between your X and your Y.  What effect does that new independent variable have in your new regression?
## (d) Then run a random effects model equivalent to your fixed effects model in step (b).  Interpret the results.
## (e) Run a Hausman test to compare your fixed effects and your random effects models.  What do you conclude? 

## install.packages("plm")
library(plm)

## install.packages("plyr")
library(plyr)

## install.packages("stargazer")
library(stargazer)

pan=read.csv(file.choose()) ## choose panel-for-R.csv

vars = c("eqwlth", "finan4", "idnum", "panelwave", "realinc", "polviews", "wrkstat") ## pick these variables ##
lab = pan[,vars] ## generate a subset ##

lab$equalize = 8-lab$eqwlth ## reverse code the question "Some people think that the government in Washington ought to reduce the income differences between the rich and the poor, perhaps by raising the taxes of wealthy families or by giving income assistance to the poor. Others think that the government should not concern itself with reducing this income difference between the rich and the poor. Think of a score of 1 as meaning that the government ought to reduce the income differences between rich and poor, and a score of 7 meaning that the government should not concern itself with reducing income differences." 
lab$bills = ifelse(lab$finan4==1, 1,0) ## dummy for Did any of the following financial matters happen to you during the last year... 4. Pressured to pay bills by stores, creditors, or bill collectors, where NOW 1 means yes, that is an issue and 0 means, no, not an issue


## Here is the firstD command in the QMSS package

#' Compute first differences 
#'
#' @param var Variable to be first-differenced.
#' @param group Optional grouping variable (see 'Details').
#' @param df Optional data frame containing \code{var} and \code{group} (see 'Details'). 
#' @details If \code{df} is specified then \code{group} must also be specified. So it is possible 
#' to specify all three parameters, \code{var} and \code{group} only, or \code{var} by itself. 
#' An example of when one might wish to omit both \code{group} and \code{df} is when using \code{firstD} 
#' in conjunction with  \pkg{plyr}'s \code{\link[plyr]{ddply}} (see 'Examples'). If \code{df} is specified then it 
#' should be sorted by \code{group} and, if necessary, a second variable (e.g. time) that orders the 
#' observations of \code{var} in the appropriate sequence. 
#' @return \code{firstD(var)} returns a first-differenced version of \code{var}. 
#' \code{firstD(var,group)} returns a first-differenced version of \code{var} by \code{group}. 
#' And \code{firstD(var,group,df)} returns a first-differenced version of \code{var} by \code{group}, 
#' where \code{var} and \code{group} are searched for in \code{df}. Note that the first value of 
#' \code{firstD(var)} will be \code{NA} since there is no difference to compute. Similarly, for
#' \code{firstD(var,group)} and \code{firstD(var,group,df)} the first value for each group 
#' will be \code{NA}.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' # Specifying both group and df
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10))
#' df$Xdiff <- firstD(X, id, df)
#' df
#' 
#' # Omitting df
#' id <- rep(1:3, each = 3)
#' X <- rpois(9, 10)
#' Xdiff <- firstD(X, id)
#' 
#' # Omitting group and df 
#' \dontrun{
#' library(plyr)
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10), Y = rpois(9, 5))
#' ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
#' }

firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}


lab <- ddply(lab, "idnum", mutate, d.equalize = firstD(equalize), d.realinc = firstD(realinc), d.bills = firstD(bills)) ## some first differences ##

table(lab$d.equalize)

## Here is the movement of opinion on equalize

prop.table(table(lab$d.equalize))

prop.table(table(lab$d.bills)) ## Here is the movement on bills 


lab$inn = ifelse(lab$d.bills==1,1,0) ## ID those entering bills struggles ##
lab$out = ifelse(lab$d.bills==-1,1,0) ## ID those leaving bills struggles ##

table(lab$out)
table (lab$inn)
prop.table(table(lab$out, lab$inn))

lab$equp = ifelse(lab$d.equalize>=1,1,0) ## people increased their equalizing opinions ##
lab$eqdown = ifelse(lab$d.equalize<=1,1,0) ## people decreased their equalizing opinions ##

table(lab$equp)
table (lab$eqdown)

## (a) Run an OLS regression, including at least one independent variable and a time variable (as dummies).  Explain how you think your independent variable relates to your dependent variable.  Interpret your results.  Did you find what you expected to find?    
## If you have bill troubles, does that make you more inclined to support active measures to equalize wealth?

summary(lm(equalize ~ bills + as.factor(panelwave), lab)) ## simple OLS ##

##Answer: it looks like those are related

## Here is the same regression as above, within plm package

eq.pool <- plm(equalize ~ bills + as.factor(panelwave), # model formula
               index = c("idnum", "panelwave"), # id & time variables
               model = "pooling", 
               data = lab) ## this is equivalent to above OLS ##

summary(eq.pool)

## Here is the first difference model for this

eq.fd <- plm(equalize ~ bills + as.factor(panelwave), # model formula
             index = c("idnum", "panelwave"), # id & time variables
             model = "fd", 
             data = lab) ## first differences ##

summary(eq.fd)

## (b) Then run a fixed effect model version of that OLS model.  Interpret your results.  Did you find what you expected to find?  Why?  Why not? 

eq.fe <- plm(equalize ~ bills + as.factor(panelwave), # model formula
             index = c("idnum", "panelwave"), # id & time variables
             model = "within", 
             data = lab) ## fixed effects ##

summary(eq.fe)

## The result is curious: if one goes from not having to worry about bills, to worrying about them, their thirst for redistribution DROPS by .29 points (though only marginally statistically significantly)


## (c) Then include an additional predictor in your fixed effects model that you think might account for the initial relationship you found between your X and your Y.  What effect does that new independent variable have in your new regression?
## Perhaps people got more conservative for other reasons too, so I will include polviews, which is how people place themselves on a liberal-conservative scale


eq.fe2 <- plm(equalize ~ bills + polviews + as.factor(panelwave), # model formula
             index = c("idnum", "panelwave"), # id & time variables
             model = "within", 
             data = lab) ## fixed effects ##

summary(eq.fe2)


## (d) Then run a random effects model equivalent to your fixed effects model in step (b).  Interpret the results.


eq.re <- plm(equalize ~ bills + as.factor(panelwave), # model formula
             index = c("idnum", "panelwave"), # id & time variables
             model = "random", 
             data = lab) ## random effects ##

summary(eq.re)

## (e) Run a Hausman test to compare your fixed effects and your random effects models.  What do you conclude? 


phtest(eq.fe, eq.re) ## Hausman test comparing RE and FE ##

stargazer(eq.pool, eq.fd, eq.fe, eq.re,
          title="Regression Results", 
          align=TRUE, 
          dep.var.labels=c("Equalize"), 
          covariate.labels=c("Bills","2010"),  
          no.space=TRUE, 
          column.labels=c("Pooled", "First Diff", "Fixed Effects", "Random Effects"), 
          dep.var.caption="", 
          model.numbers=FALSE,
          type = "text", omit = "Constant")


#### EXTRA STUFF I TRIED

eq.fe.2 <- plm(equalize ~ bills + as.factor(panelwave) + I(log(realinc)), # model formula
               index = c("idnum", "panelwave"), # id & time variables
               model = "within", 
               data = lab) ## what about income as a mediator? ##

summary(eq.fe.2)

eq.fe.3 <- plm(equalize ~ bills + as.factor(panelwave) + I(log(realinc)) + as.factor(wrkstat), # model formula
               index = c("idnum", "panelwave"), # id & time variables
               model = "within", 
               data = lab) ## what about work status as a mediator ##

summary(eq.fe.3)

## just to compare the fixed effects vs. pooled OLS ##

eq.pool.3 <- plm(equalize ~ bills + as.factor(panelwave) + I(log(realinc)) + as.factor(wrkstat), # model formula
                 index = c("idnum", "panelwave"), # id & time variables
                 model = "pooling", 
                 data = lab) 

summary(eq.pool.3)


## what about discontinuous measures of these outcomes ##

summary(lm(eqdown ~ out + inn, lab)) ## predicting direction ##
summary(lm(equp ~ out + inn, lab)) ## predicting direction ##

summary(lm(eqdown ~ out + inn, lab, subset=(d.equalize!=0 & d.bills!=0))) ## only changers on X and Y ##
summary(lm(equp ~ out + inn, lab, subset=(d.equalize!=0 & d.bills!=0))) ## only changers on X and Y ##
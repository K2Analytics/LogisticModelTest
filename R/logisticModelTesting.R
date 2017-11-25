## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

# List of Libraries
# library(data.table)
# library(scales)

## deciling code
#' deciling function
#'
#' Takes the data and divide in to 10 parts.
#'
#' @param x A numeric variable
#' @return It will give back series of numbers, in which decile the observation goes to.
#' @example DEC_exam.R
#' @export
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


#' Rrate function
#'
#' Takes the data and divid in to 10 parts and looks the target distribution.
#' @author Rajesh Jakhotia
#' @param x  varibale need to look the target distribution
#' @return It will give back you the deciled data with target disribution
#' @example exam_Rrate.R
#' @export

Rrate <- function(df, target, var)
{

  tmp <- df[, c(var , target)]
  colnames(tmp)[1] = "Xvar"
  colnames(tmp)[2] = "Target"


  tmp$deciles <- decile(tmp$Xvar)


  tmp_DT = data.table(tmp)

  RRate <- tmp_DT[, list(
    min_ = min(Xvar), max_ = max(Xvar), avg_ = mean(Xvar),
    cnt = length(Target), cnt_resp = sum(Target),
    cnt_non_resp = sum(Target == 0)
  ) ,
  by=deciles][order(deciles)]
  RRate$rrate<-RRate$cnt_resp*100/RRate$cnt
  RRate
}

#' Visualizations & variable transformation
#'
#' Produce graph for the variable
#' @author Rajesh Jakhotia
#' @param df  dataframe which target and variable present.
#' @param target Target variable.
#' @param var Variable need to consider.
#' @param ln_tranfm optional variable.
#' @return Graphs.
#' @example vis_exam.R
#' @export


fn_visualize <- function(df, target, var, ln_trnfm=0)
{
  tmp <- df[, c(var , target)]
  head(tmp)
  colnames(tmp)[1] = "Xvar"
  colnames(tmp)[2] = "Target"
  if (ln_trnfm == 1){
    tmp$Xvar = log(tmp$Xvar + 1)
  }
  tmp$deciles <- decile(tmp$Xvar)
  tmp_DT = data.table(tmp)
  RRate <- tmp_DT[, list(min_ = min(Xvar), max_ = max(Xvar), avg_ = mean(Xvar),
                         cnt = length(Target), cnt_responder = sum(Target), cnt_non_responder = sum(Target == 0)) ,
                  by=deciles][order(deciles)]
  RRate$prob <- RRate$cnt_responder / RRate$cnt;
  RRate$log_odds <- log(RRate$prob / (1 - RRate$prob))
  plot(x=RRate$avg_, y=RRate$log_odds, type="b", pch = 20,
       xlab=var, ylab=" Log Odds")
  abline(fit <- lm(RRate$log_odds ~ RRate$avg_), col="red")
  legend("topright", bty="n", legend=paste("R2 is",
                                           format(summary(fit)$adj.r.squared, digits=4)))
}


#' Rank Ordering
#'
#' Produce Rank ordering table
#' @author Rajesh Jakhotia
#' @param df  dataframe which target and variable present.
#' @param target Target variable.
#' @param probability Predicted probabilities.
#' @return Rank ordering table
#' @example exam_ROTable.R
#' @export
ROTable <- function(df, target, probability)
{
  tmp <- df[, c(target,probability)]
  colnames(tmp)[1] = "Target"
  colnames(tmp)[2] = "prob"
  tmp$deciles<-decile(tmp$prob)


  mydata.DT = data.table(tmp) ## Converting the data frame to data table object
  ## Creating Aggregation and Group By similar to as in SQL
  rank <- mydata.DT[, list(
    min_prob = min(prob),
    max_prob = max(prob),
    cnt = length(Target),
    cnt_resp = sum(Target),
    cnt_non_resp = sum(Target == 0)
  ) ,
  by = deciles][order(-deciles)]
  rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
  rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
  rank$cum_non_resp <-
    cumsum(rank$cnt_non_resp) ## computing cum non-responders
  rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)

  rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)

  rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
  ## KS
  rank ## display Rank Ordering Table
}

#' Ks statistics & ROC curve
#'
#' Plot ROC curve and calculate KS statistics.
#' @author Rajesh Jakhotia
#' @param df  dataframe which target and variable present.
#' @param target Target variable.
#' @param probability Predicted probabilities.
#' @return ROC curve and KS value
#' @example exam_KS.R
#' @export
KS <- function(df, target, probability)
{
  mydata <- df[, c(target, probability)]
  colnames(mydata)[1] = "Target"
  colnames(mydata)[2] = "prob"

  pred <- prediction(mydata$prob, mydata$Target)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf)
  ks <- max(attr(perf, 'y.values')[[1]] - attr(perf, 'x.values')[[1]])
  ks
}

#' Chi Sq - Goodness of Fit
#'
#' Checking Chi Sq - Goodness of Fit.
#' @author Peter D. M. Macdonald, McMaster University
#' @param df  dataframe which target and variable present.
#' @param target Target variable.
#' @param probability Predicted probabilities.
#' @return Chi Sq. values
#' @example exam_GOFit.R
#' @export
hosmerlem_gof <- function(df, target, probability,g=10)
{
  tmp <- df[, c(target, probability)]
  colnames(tmp)[1] = "Target"
  colnames(tmp)[2] = "prob"
  tmp$deciles<-decile(tmp$prob)

  hosmerlem <-
    function (y, yhat, g1=g) {
      cutyhat <-
        cut(yhat,
            breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
            include.lowest = T)
      obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
      expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
      chisq <-sum((obs - expect) ^ 2 / expect)
      P <-1 - pchisq(chisq, g1 - 2)
      c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
    }
  hl_gof <- hosmerlem(tmp$Target, tmp$prob)
  print(hl_gof)
  print("Table")
  sqldf ("select deciles, count(1) as cnt,
sum (Target) as Obs_Resp, count (Target == 0) as Obs_Non_Resp,
         sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
         from tmp
         group by deciles
         order by deciles desc")
}


#' Concordance
#'
#' Checking Concordance.
#' @param df  dataframe which target and variable present.
#' @param target Target variable.
#' @param probability Predicted probabilities.
#' @return Concordance outputs
#' @example exam_con.R
#' @export

concordance <- function(df, target, probability)
{
  tmp <- df[, c(target, probability)]
  colnames(tmp)[1] = "Target"
  colnames(tmp)[2] = "prob"

  concordance1 = function(y, yhat)
  {
    Con_Dis_Data = cbind(y, yhat)
    ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
    zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
    conc = matrix(0, dim(zeros)[1], dim(ones)[1])
    disc = matrix(0, dim(zeros)[1], dim(ones)[1])
    ties = matrix(0, dim(zeros)[1], dim(ones)[1])
    for (j in 1:dim(zeros)[1])
    {
      for (i in 1:dim(ones)[1])
      {
        if (ones[i, 2] > zeros[j, 2])
        {
          conc[j, i] = 1
        }
        else if (ones[i, 2] < zeros[j, 2])
        {
          disc[j, i] = 1
        }
        else if (ones[i, 2] == zeros[j, 2])
        {
          ties[j, i] = 1
        }
      }
    }
    Pairs = dim(zeros)[1] * dim(ones)[1]
    PercentConcordance = (sum(conc) / Pairs) * 100
    PercentDiscordance = (sum(disc) / Pairs) * 100
    PercentTied = (sum(ties) / Pairs) * 100
    return(
      list(
        "Percent Concordance" = PercentConcordance,
        "Percent Discordance" = PercentDiscordance,
        "Percent Tied" = PercentTied,
        "Pairs" = Pairs
      )
    )
  }
  concordance_output <- concordance1(tmp$Target, tmp$prob)
  concordance_output
}

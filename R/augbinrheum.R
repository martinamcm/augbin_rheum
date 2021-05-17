#' Augmented binary method 
#'
#' Fits the augmented binary method using an underlying latent
#' variable model allowing for up to two continuous components 
#' and one binary component, common in rheumatic conditions.
#' 
#' The responder outcome is assumed to define a responder when
#' continuous components are below a particular dichotomisation threshold and
#' a binary component is 0.
#' 
#' Available as a Shiny app with documentation at https://github.com/martinamcm/AugBin
#' 
#' @references 
#' McMenamin M, Grayling MJ, Berglind A, Wason JMS. Increasing power in the 
#' analysis of responder endpoints in rheumatology: a software tutorial.
#' medRxiv. 2020. doi: 10.1101/2020.07.28.20163378
#'
#' @param data A dataset to analyse. Must be an object of class data.frame
#' details. Defaults to NULL. 
#' @param cts Number of continuous components within the composite endpoint. Allows for
#' 1 or 2 continuous components. Defaults to 1. 
#' @param bin Number of binary components within the composite endpoint. Allows for
#' 0 or 1 binary components. Any additional components can be collapsed and accounted for within
#' this indicator. Defaults to 1. 
#' @param dichot Vector of dichotomisation threshold(s) for continuous component(s).
#' For more than one continuous components values must be entered as e.g. c(0,0). 
#' Defaults to NULL.
#' @author Martina McMenamin.
#' @importFrom cubature cuhre
#' @importFrom Matrix nearPD
#' @importFrom brglm brglm
#' @importFrom boot inv.logit
#' @importFrom optimx optimx
#' @importFrom MASS ginv
#' @importFrom mvtnorm pmvnorm dmvnorm
#' @importFrom stats dnorm qnorm pnorm lm var coef cor
#' @export


augbinrheum <- function(data= NULL,
                          cts=1,
                          bin=1,
                          dichot=NULL){

  
  ####### check input parameters ###############

  check_augbinrheum_data(data,cts,bin,dichot)

  ####### Computations ########################
    
    if(cts==2 && bin==1){
      
      Analysis <- LatVarfunc_21(data,dichot)
      mle <- Analysis[23:35]
      
    }else if(cts==2 && bin==0){
      
      Analysis <- LatVarfunc_20(data,dichot)
      mle <- Analysis[23:31]
      
    }else if(cts==1 && bin==1){
      
      Analysis <- LatVarfunc_11(data,dichot)
      mle <- Analysis[23:29]
      
    }else{
      
      Analysis <- LatVarfunc_10(data,dichot)
      mle <- Analysis[23:26]
    }
  
 
    ########## Output ##########################3
    
    Method <- c("Latent Variable", "Standard Binary")
    ResponseT <- Analysis[c(10,21)]
    ResponseC <- Analysis[c(11,22)]
    OddsRatio <- Analysis[c(2,13)]
    ClowOR <- Analysis[c(1,12)]
    CuppOR <- Analysis[c(3,14)]
    RiskRatio <- Analysis[c(5,16)]
    ClowRR <- Analysis[c(4,15)]
    CuppRR <- Analysis[c(6,17)]
    RiskDiff <- Analysis[c(8,19)]
    ClowRD <- Analysis[c(7,18)]
    CuppRD <- Analysis[c(9,20)]
      
      
      
    output <-  list(response_prob = tibble::tibble(Method = Method,
                                                   treat_resp = ResponseT,
                                                   control_resp = ResponseC),
      
                    odds_ratio = tibble::tibble(Method = Method,
                                                est = exp(OddsRatio),
                                                ci_lower = exp(ClowOR),
                                                ci_upper = exp(CuppOR)),
                                     
                    risk_ratio = tibble::tibble(Method = Method,
                                                est = exp(RiskRatio),
                                                ci_lower = exp(ClowRR),
                                                ci_upper = exp(CuppRR)),
                    
                    risk_diff = tibble::tibble(Method = Method,
                                               est = RiskDiff,
                                               ci_lower = ClowRD,
                                               ci_upper = CuppRD),
                    
                    param_est = mle
                    )
    
    output    
}

  
  

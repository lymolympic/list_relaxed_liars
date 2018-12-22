# list_relaxed_liars.R
# Yimeng Li
# This script contains a function that obtains the proposed identified set and its confidence set
#   for the prevalence of a sensitive behavior or attitude
#   under the relaxed liars assumption
#   described in "Relaxing the No Liars Assumption in List Experiment Analyses".
# This version: Dec 18, 2018.

# function list_relaxed_liars
# Inputs:
# (1) dist_control: distribution of answers by control respondents
#       e.g., dist_control = c(1172, 3603, 2482, 458) or (0.15, 0.47, 0.32, 0.06)
#             indicates 1172 respondents answered 0, 3603 respondents answered 1...
# (2) dist_treated: distribution of answers by treated respondents
#       e.g., dist_treated = c(1315, 3322, 2720, 883, 281) or (0.15, 0.39, 0.32, 0.10, 0.03)
# (3) J: number of control items
# (4) affirmative:
#       TRUE (1) if an affirmative (latent) response to the sensitive item is considered sensitive;
#       FALSE (0) if a negative (latent) response to the sensitive item is considered sensitive.
# (5) combine (= FALSE by default)
#       FALSE (0) to implement bounds given by (3) in Section 2.4;
#       TRUE (1) to collapse the case of all control items and all but one control items
#         and implement the bounds described in Section 2.5.2,
#         recommended when there are few respondents answering the maximal number of items
#         (or the minimal number of items for list experiments with negative sensitive responses),
#         e.g., < 50 respondents for a list experiment of size 1000-2000 (footnote 8).
# (6) CI (= TRUE by default): If TRUE, calculate (1-alpha)-confidence set given by (4) in Section 2.4.
# (7) alpha (= 0.05 by default): nominal significance level for the confidence set
# (8) warning (= TRUE by default):
#       Print warning when enable warning when some estimated proportions are smaller than 0 or greater than 1.
#       Significant departure is an indication of the violation of no design effect.
# Outputs:
# (1) the identified proportions of different types of respondents
# (2) the maximal liar ratio
# (3) the maximal proportions of liars with k control items answered affirmatively, k = 0, 1, ..., J
# (4) the identified lower bound (= the difference-in-means estimate) and the identified upper bound
# (5) the confidence set for the identified set

library(nleqslv)

list_relaxed_liars <- function(dist_control, dist_treated, J, affirmative, combine = FALSE, CI = TRUE, alpha = 0.05, warning = TRUE) {
  
  # check argument inputs are compatible
  if (J != length(dist_control) - 1) {
    stop("Distribution of answers by control respondents has an incorrect dimension.")
  }
  if (J != length(dist_treated) - 2) {
    stop("Distribution of answers by treated respondents has an incorrect dimension.")
  }
  if (affirmative != TRUE & affirmative != FALSE) {
    stop("Only TRUE (1) and FALSE (0) are allowed for argument 'affirmative'.")
  }
  if (combine != TRUE & combine != FALSE) {
    stop("Only TRUE (1) and FALSE (0) are allowed for argument 'combine'.")
  }
  
  # Total number of respondents under control and treatment
  c_total = sum(dist_control)
  t_total = sum(dist_treated)
  
  # Distribution of responses in proportions:
  c_prop = dist_control/c_total
  t_prop = dist_treated/t_total
  
  # Calculate
  # (1) pt: proportion of truth-tellers with the sensitve behavior or attitude;
  # (2) pnl: sum of proportions of liars with the sensitve behavior or attitude
  #          and respondents without the sensitve behavior or attitude.
  # See Section 2.3 for detail.
  if (affirmative == TRUE) {
    pt = cumsum(c_prop[1:(J+1)] - t_prop[1:(J+1)])
    pnl = cumsum(t_prop[1:(J+1)] - c(0,c_prop[1:J]))
  } else {
    pt = cumsum(t_prop[1:(J+1)] - c(0,c_prop[1:J]))
    pnl = cumsum(c_prop[1:(J+1)] - t_prop[1:(J+1)])
  }
  id_props = list(pt = pt, pnl = pnl)
  
  if ((min(pt) < 0 | max(pt) > 1 | min(pnl) < 0 | max(pnl) > 1) & (warning == TRUE)) {
    warning("Some estimated proportions are smaller than 0 or greater than 1.")
  }
  
  # Lower bound:
  lower_bound = sum(pt)
  
  # Maximal liars ratio:
  pt = pmax(0, pt)
  pnl = pmax(0, pnl)
  if (affirmative == TRUE & combine == FALSE) {
    lambda = pnl[J+1]/(pnl[J+1]+pt[J+1])
  } else if (affirmative == TRUE & combine == TRUE) {
    lambda = (pnl[J+1]+pnl[J])/(pnl[J+1]+pnl[J]+pt[J+1]+pt[J])
  } else if (affirmative == FALSE & combine == FALSE) {
    lambda = pnl[1]/(pnl[1]+pt[1])
  } else {
    lambda = (pnl[1]+pnl[2])/(pnl[1]+pnl[2]+pt[1]+pt[2])
  }
  
  # Maximal proportion of liars:
  pl_max = pmin(lambda/(1-lambda)*pt, pnl)
  if (combine == TRUE){
    if (affirmative == TRUE) {
      pl_max[J+1] = pnl[J+1]
      pl_max[J] = pnl[J]
    } else {
      pl_max[1] = pnl[1]
      pl_max[2] = pnl[2]
    }
  }
  
  # Upper bound:
  upper_bound = lower_bound + sum(pl_max)
  id_bounds = c(lower_bound, upper_bound)
  
  # Outputs:
  if (CI == FALSE){
    return(list(id_props = id_props, lambda = lambda, pl_max = pl_max, bounds = id_bounds))
  } else{
    B = 1000
    set.seed(1000)
    
    boot_c_prop = rmultinom(B, size=c_total, prob=c_prop)/c_total
    boot_t_prop = rmultinom(B, size=t_total, prob=t_prop)/t_total
    
    boot_pt <- vector("list", length = B)
    boot_pnl <- vector("list", length = B)
    boot_lb <- vector("double", B)
    boot_ub <- vector("double", B)
    boot_lambda <- vector("double", B)
    boot_pl_max <- vector("list", length = B)
    
    for (i in 1:B) {
      # Proportions of different types of respondents for each bootstrap sample:
      if (affirmative == TRUE) {
        boot_pt[[i]] = cumsum(boot_c_prop[1:(J+1),i] - boot_t_prop[1:(J+1),i])
        boot_pnl[[i]] = cumsum(boot_t_prop[1:(J+1),i] - c(0,boot_c_prop[1:J,i]))
      } else {
        boot_pt[[i]] = cumsum(boot_t_prop[1:(J+1),i] - c(0,boot_c_prop[1:J,i]))
        boot_pnl[[i]] = cumsum(boot_c_prop[1:(J+1),i] - boot_t_prop[1:(J+1),i])
      }
      # Lower bound estimate for each bootstrap sample:
      boot_lb[i] = sum(boot_pt[[i]])
      # Maximal liars ratio for each bootstrap sample:
      boot_pt[[i]] = pmax(0, boot_pt[[i]])
      boot_pnl[[i]] = pmax(0, boot_pnl[[i]])
      if (affirmative == TRUE & combine == FALSE) {
        boot_lambda[i] = boot_pnl[[i]][J+1]/(boot_pnl[[i]][J+1]+boot_pt[[i]][J+1])
      } else if (affirmative == TRUE & combine == TRUE) {
        boot_lambda[i] = (boot_pnl[[i]][J+1]+boot_pnl[[i]][J])/(boot_pnl[[i]][J+1]+boot_pnl[[i]][J]+boot_pt[[i]][J+1]+boot_pt[[i]][J])
      } else if (affirmative == FALSE & combine == FALSE) {
        boot_lambda[i] = boot_pnl[[i]][1]/(boot_pnl[[i]][1]+boot_pt[[i]][1])
      } else {
        boot_lambda[i] = (boot_pnl[[i]][1]+boot_pnl[[i]][2])/(boot_pnl[[i]][1]+boot_pnl[[i]][2]+boot_pt[[i]][1]+boot_pt[[i]][2])
      }
      # Maximal proportion of liars for each bootstrap sample:
      boot_pl_max[[i]] = pmin(boot_lambda[i]/(1-boot_lambda[i])*boot_pt[[i]], boot_pnl[[i]])
      if (combine == TRUE){
        if (affirmative == TRUE) {
          boot_pl_max[[i]][J+1] = boot_pnl[[i]][J+1]
          boot_pl_max[[i]][J] = boot_pnl[[i]][J]
        } else {
          boot_pl_max[[i]][1] = boot_pnl[[i]][1]
          boot_pl_max[[i]][2] = boot_pnl[[i]][2]
        }
      }
      # Upper bound for each bootstrap sample:
      boot_ub[i] = boot_lb[i] + sum(boot_pl_max[[i]])
    }
    # Imbens-Manski confidence set for the identified set:
    IMcriticalval <- function(c) { return(pnorm(c+(upper_bound-lower_bound)/max(sd(boot_lb),sd(boot_ub)))-pnorm(-c)-(1-alpha)) }
    c_IM <- nleqslv(1.96, IMcriticalval)$x
    CI_bounds <- c(max(0,lower_bound-c_IM*sd(boot_lb)), min(1,upper_bound+c_IM*sd(boot_ub)))
    
    return(list(id_props = id_props, lambda = lambda, pl_max = pl_max, bounds = id_bounds, CI = CI_bounds))
  }
}

library(MASS)
library(mnonr)

#--- IMPORT USER-SPECIFIED VALUES --------------------------------------------#
obj = input$obj
effect = input$effect
scale.t = input$scale.t
scale.m = input$scale.m
scale.y = input$scale.y
rand.t = input$rand.t
if(rand.t == FALSE){
  R2x.t = input$R2x.t
} else {
  R2x.t = 0
  if(scale.t == "binary"){
    p.t = input$p.t
  }
} 
if(scale.t == "binary"){
  treat.t = 1
  control.t = 0
} else {
  treat.t = input$treat.t
  control.t = input$control.t
}
seed = input$seed
bt.m = input$bt.m
R2x.m = input$R2x.m
n.x = input$n.x
bt.y = input$bt.y
bm.y = input$bm.y
btm.y = input$btm.y 
R2x.y = input$R2x.y
skewness.t = input$skewness.t
kurtosis.t = input$kurtosis.t
skewness.m = input$skewness.m
kurtosis.m = input$kurtosis.m
skewness.y = input$skewness.y
kurtosis.y = input$kurtosis.y
Nlow = input$Nlow
Nhigh = input$Nhigh
Nsteps = input$Nsteps
powReps = input$powReps
mcmcReps = input$mcmcReps
sig = input$sig * 100
if (obj == "choose_n") {
  TarN = input$TarN
} else {
  TarPow = input$TarPow
}

#--- INPUT VALUE CHECKS -------------------------------------------------------#
if(!is.null(kurtosis.t)){
  if (kurtosis.t < skewness.t^2 - 2) {
    stop("Kurtosis must be no less than squared skewness minus 2.")
  }
}
if(!is.null(kurtosis.m)){
  if (kurtosis.m < skewness.m^2 - 2) {
    stop("Kurtosis must be no less than squared skewness minus 2.")
  }
}
if(!is.null(kurtosis.y)){
  if (kurtosis.y < skewness.y^2 - 2) {
    stop("Kurtosis must be no less than squared skewness minus 2.")
  }
}

if (obj == "choose_n") {
  # CHECK: Is N greater than 5 and an integer?
  if (TarN < 5 | !abs(TarN - round(TarN)) < .Machine$double.eps ^ 0.5) {
    stop("\"Target Sample Size\" must be an integer greater than 5. Please change this value.")
  }
  
  # CHECK: Is TarN greater than Nlow?
  if (Nlow >= TarN) {
    stop("\"Target Sample Size\" must be larger than \"Minimum Sample Size\". Please change these values.")
  }
  
  # CHECK: Is TarN smaller than Nhigh?
  if (Nhigh <= TarN) {
    stop("\"Target Sample Size\" must be smaller than \"Maximum Sample Size\". Please change these values.")
  }
} else {
  # CHECK: Is Target Power between 0 and 1?
  if (TarPow < 0 | TarPow > 1) {
    stop("\"Target Power\" must be a number between 0 and 1. Please change this value.")
  }
}  

# CHECK: Is the significance level (%) between 0 and 100?
if (sig < 0 | sig > 100) {
  stop("\"Significance Level (%)\" must be a number between 0 and 100. Please change this value.")
}

# CHECK: Is Nlow greater than 5 and an integer?
if (Nlow < 5 | !abs(Nlow - round(Nlow)) < .Machine$double.eps ^ 0.5) {
  stop("\"Minimum Sample Size\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is Nhigh greater than 5 and an integer?
if (Nhigh < 5 | !abs(Nhigh - round(Nhigh)) < .Machine$double.eps ^ 0.5) {
  stop("\"Maximum Sample Size\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is Nsteps greater than 1 and an integer?
if (Nsteps < 1 | !abs(Nsteps - round(Nsteps)) < .Machine$double.eps ^ 0.5) {
  stop("\"Sample Size Steps\" must be an integer greater than 1. Please change this value.")
}

# CHECK: Is Nhigh greater than nlow?
if (Nlow >= Nhigh) {
  stop("\"Maxmimum Sample Size\" must be larger than \"Minimum Sample Size\". Please change these values.")
}

# CHECK: Is Nsteps smaller than N range?
if (abs(Nhigh - Nlow) < Nsteps) {
  stop("\"Sample Size Steps\" must be smaller than the sample size range. Please change this value.")
}  

# CHECK: Is the number of replications > 5 and an integer?
if (powReps < 5 | !abs(powReps - round(powReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"Number of replications per sample size for power calculation\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the number of MC replications > 5 and an integer?
if (mcmcReps < 5 | !abs(mcmcReps - round(mcmcReps)) < .Machine$double.eps ^ 0.5) {
  stop("\"Number of Monte Carlo draws per replication for causal mediation analysis\" must be an integer greater than 5. Please change this value.")
}

# CHECK: Is the number of covariates 0?
if(n.x == 0 & rand.t == FALSE)
  stop("If the treatment is not randomized, the number of covariates should not be 0.")

if(R2x.t == 0 & rand.t == FALSE)
  stop("If treatment is not randomized, the Proportion of variance in T explained by X should not be 0.")

if(n.x == 0 & R2x.t != 0){
  if(scale.t == "continuous")
    stop("If the proportion of variance in T explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in T explained by X can be both 0 only when both the treatment and mediator are randomized.")
  if(scale.t == "binary")
    stop("If the proportion of variance in latent T explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent T explained by X can be both 0 only when both the treatment and mediator are randomized.")
}

if(n.x == 0 & R2x.m != 0){
  if(scale.m == "continuous")
    stop("If the proportion of variance in M explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in M explained by X can be both 0 only when both the treatment and mediator are randomized.")
  if(scale.m == "binary")
    stop("If the proportion of variance in latent M explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent M explained by X can be both 0 only when both the treatment and mediator are randomized.")
}

if(n.x == 0 & R2x.y != 0){
  if(scale.y == "continuous")
    stop("If the proportion of variance in Y explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
  if(scale.y == "binary")
    stop("If the proportion of variance in latent Y explained by X is not 0, the number of covariates should not be 0. The number of covariates and the proportion of variance in latent Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
}

if(n.x != 0 & R2x.m == 0){
  if(scale.m == "continuous")
    stop("If the number of covariates is not 0, the proportion of variance in M explained by X should not be 0. The number of covariates and the proportion of variance in M explained by X can be both 0 only when both the treatment and mediator are randomized.")
  if(scale.m == "binary")
    stop("If the number of covariates is not 0, the proportion of variance in latent M explained by X should not be 0. The number of covariates and the proportion of variance in latent M explained by X can be both 0 only when both the treatment and mediator are randomized.")
}

if(n.x != 0 & R2x.y == 0){
  if(scale.y == "continuous")
    stop("If the number of covariates is not 0, the proportion of variance in Y explained by X should not be 0. The number of covariates and the proportion of variance in Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
  if(scale.y == "binary")
    stop("If the number of covariates is not 0, the proportion of variance in latent Y explained by X should not be 0. The number of covariates and the proportion of variance in latent Y explained by X can be both 0 only when both the treatment and mediator are randomized.")
}

sgn.Rxt = sgn.Rxm = sgn.Rxy = 1 
if(scale.m == "continuous" & scale.y == "binary"){
  skewness.m = 0
  kurtosis.m = 0
}

################ Below is the code for updating R_tx and R_mx for the calculation of bx.m and bx.y      
# As written in Appendix B, when T (or M) is binary, R_tx in Equation (B.1) and Equation (B.2) (or R_mx in Equation (B.2)) is the correlation between X and the predictor T (or M), which is on the original scale rather than the latent scale, and thus it needs to be recalculated. 
# Hence, I generate a large data set for accurate calculation of R, with the same code as in server.r.
# Here I focus on one single X, which should be the same for multiple X's
# Generate X
N = 100000 # Based on my testing through simulations, N = 10000 is still not enough for getting an accurate estimation of Rx.t.predictor or Rx.m.predictor.
# The number of X does not affect true effect calculation. Hence, I only generate one X here.
if(n.x != 0){
  x = rnorm(N)	
} else {
  x = rep(0, N)
}

# Generate the treatment
if(rand.t == TRUE){
  R2x.t = 0
  if(scale.t == "binary"){
    t = rbinom(N, 1, p.t)
  } else {
    t = rnorm(N)
  }
} else {
  # Transform R2x to bx
  bx.t = sqrt(R2x.t) * sgn.Rxt
  var.t = 1
  b0.t = 0
  var.et = var.t - var(b0.t + bx.t * x)
  if(var.et < 0)
    stop("The error term of the treatment model has a negative variance in at least one replication. Please respecify the treatment model parameters.")
  et = rnorm(N, 0, sqrt(var.et))
  t = b0.t + bx.t * x + et
  if(scale.t == "binary"){
    t.star = t
    t[t.star > 0] = 1
    t[t.star <= 0] = 0
  } 
}
if(scale.t == "binary"){
  if(n.x != 0){
    Rx.t.predictor = as.numeric(cor(x, t))
  } else {
    Rx.t.predictor = 0
  }
} else {
  Rx.t.predictor = sqrt(R2x.t) * sgn.Rxt
}

var.m = 1
b0.m = 0
bx.m = sqrt(R2x.m) * sgn.Rxm - bt.m * Rx.t.predictor
var.em = var.m - var(b0.m + bt.m * scale(t) + bx.m * x)
if(var.em < 0)
  stop("The error term of the mediator model has a negative variance in at least one replication. Please respecify the mediator model parameters.")
em = rnorm(N, 0, sqrt(var.em))
# For any skewness and kurtosis, the true effects and R_x are the same. Hence, we can use em = rnorm() throughout when calculating true effects and R_x.
m = b0.m + bt.m * scale(t) + bx.m * x + em  
if(scale.m == "binary"){
  m.star = m
  m[m.star > 0] = 1
  m[m.star <= 0] = 0
} 
if(n.x != 0){
  Rx.tm.predictor = as.numeric(cor(x, t * m)) # The sign of Rx.tm is determined by the relationships between x and t/m. Hence, I do not force it to be positive.
} else {
  Rx.tm.predictor = 0
}
if(scale.m == "binary"){
  if(n.x != 0){
    Rx.m.predictor = as.numeric(cor(x, m))
  } else {
    Rx.m.predictor = 0
  }
} else {
  Rx.m.predictor = sqrt(R2x.m) * sgn.Rxm
}

############### Below is the code for power analysis
# When transforming R2x to bx in the power analysis, we should now take into account the number of covariates
if(rand.t == FALSE){
  if(n.x != 0){
    bx.t = sqrt(R2x.t) * sgn.Rxt/sqrt(n.x)
  } else {
    bx.t = 0
  }
}
if(n.x != 0){
  bx.m = (sqrt(R2x.m) * sgn.Rxm - bt.m * Rx.t.predictor)/sqrt(n.x)
} else {
  bx.m = 0
}
if(n.x != 0){
  bx.y = (sqrt(R2x.y) * sgn.Rxy - btm.y * Rx.tm.predictor - bm.y * Rx.m.predictor - bt.y * Rx.t.predictor)/sqrt(n.x)
} else {
  bx.y = 0
}

withProgress(message = 'Running Replications', value = 0, {
  
  # Create function for 1 rep
  powRep = function(N){
    
    incProgress(1 / (powReps * length(seq(Nlow, Nhigh, Nsteps))))
    
    ######## Generate data
    
    try({
      # Generate X
      if(n.x != 0){
        var.x = 1
        X = matrix(NA, N, n.x)
        for(k in 1:n.x){
          X[, k] = rnorm(N, 0, sqrt(var.x))
        }
        colnames(X) = paste0("x", rep(1:n.x))	
      } else {
        x = rep(0, N)
      }
      
      # Generate the treatment
      if(rand.t == TRUE){
        if(scale.t == "binary"){
          t = rbinom(N, 1, p.t)
        } else {
          t = unonr(N, rep(0, 2), diag(rep(1, 2)), skewness = rep(skewness.t, 2), kurtosis = rep(kurtosis.t, 2), empirical = FALSE)[, 1]
        }
      } else {
        var.t = 1
        b0.t = 0
        var.et = var.t - var(b0.t + bx.t * apply(X, 1, sum))
        et = unonr(N, rep(0, 2), diag(rep(var.et, 2)), skewness = rep(skewness.t, 2), kurtosis = rep(kurtosis.t, 2), empirical = FALSE)[, 1]
        t = b0.t + bx.t * apply(X, 1, sum) + et
        if(scale.t == "binary"){
          t.star = t
          t[t.star > 0] = 1
          t[t.star <= 0] = 0
        } 
      }
      
      # Generate the mediator
      var.m = 1
      b0.m = 0
      if(n.x != 0){
        var.em = var.m - var(b0.m + bt.m * scale(t) + bx.m * apply(X, 1, sum))
      } else {
        var.em = var.m - var(b0.m + bt.m * scale(t))
      }
      em = unonr(N, rep(0, 2), diag(rep(var.em, 2)), skewness = rep(skewness.m, 2), kurtosis = rep(kurtosis.m, 2), empirical = FALSE)[, 1] #unonr is for generating multivariate distribution. Hence, I generate two independent variables that follow the same distribution and then use the first one
      if(n.x != 0){
        m = b0.m + bt.m * scale(t) + bx.m * apply(X, 1, sum) + em
      } else {
        m = b0.m + bt.m * scale(t) + em
      }
      if(scale.m == "binary"){
        m.star = m
        m[m.star > 0] = 1
        m[m.star <= 0] = 0
      } 
      
      # Generate the outcome
      var.y = 1
      b0.y = 0
      if(n.x != 0){
        var.ey = var.y - var(b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m) + bx.y * apply(X, 1, sum))
      } else {
        var.ey = var.y - var(b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m))
      }
      if(var.ey < 0)
        stop("The error term of the outcome model has a negative variance in at least one replication. Please respecify the outcome model parameters.")
      ey = unonr(N, rep(0, 2), diag(rep(var.ey, 2)), skewness = rep(skewness.y, 2), kurtosis = rep(kurtosis.y, 2), empirical = FALSE)[, 1]
      if(n.x != 0){
        y = b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m) + bx.y * apply(X, 1, sum) + ey
      } else {
        y = b0.y + bt.y * scale(t) + bm.y * scale(m) + btm.y * scale(t * m) + ey
      }
      if(scale.y == "binary"){
        y.star = y
        y[y.star > 0] = 1
        y[y.star <= 0] = 0
      } 
      
      if(n.x != 0){
        data = data.frame(t, m, y, X)
      } else {
        data = data.frame(t, m, y)
      }
      
      # Fit mediator and outcome models
      if(scale.m == "continuous"){
        if(n.x != 0){
          l.m = lm(as.formula(paste("m ~", paste(c("t", colnames(X)), collapse = " + "))), data = data)
        } else {
          l.m = lm(as.formula(paste("m ~", paste("t", collapse = " + "))), data = data)
        }
        sd.m = sigma(l.m)
      } else {
        if(n.x != 0){
          l.m = glm(as.formula(paste("m ~", paste(c("t", colnames(X)), collapse = " + "))), data = data, family = binomial(link = "probit"))
        } else {
          l.m = glm(as.formula(paste("m ~", paste("t", collapse = " + "))), data = data, family = binomial(link = "probit"))
        }
      }
      if(scale.y == "continuous"){
        if(n.x != 0){
          if(btm.y == 0){
            l.y = lm(as.formula(paste("y ~", paste(c("t", "m", colnames(X)), collapse = " + "))), data = data)
          } else {
            l.y = lm(as.formula(paste("y ~", paste(c("t", "m", "t:m", colnames(X)), collapse = " + "))), data = data)
          }
        } else {
          if(btm.y == 0){
            l.y = lm(as.formula(paste("y ~", paste(c("t", "m"), collapse = " + "))), data = data)
          } else {
            l.y = lm(as.formula(paste("y ~", paste(c("t", "m", "t:m"), collapse = " + "))), data = data)
          }
        }
      } else {
        if(n.x != 0){
          if(btm.y == 0){
            l.y = glm(as.formula(paste("y ~", paste(c("t", "m", colnames(X)), collapse = " + "))), data = data, family = binomial(link = "probit"))
          } else {
            l.y = glm(as.formula(paste("y ~", paste(c("t", "m", "t:m", colnames(X)), collapse = " + "))), data = data, family = binomial(link = "probit"))
          }
        } else {
          if(btm.y == 0){
            l.y = glm(as.formula(paste("y ~", paste(c("t", "m"), collapse = " + "))), data = data, family = binomial(link = "probit"))
          } else {
            l.y = glm(as.formula(paste("y ~", paste(c("t", "m", "t:m"), collapse = " + "))), data = data, family = binomial(link = "probit"))
          }
        }
      }
      
      # Simulate draws of mediator and outcome model coefficients from multivariate normal distribution
      mcmc.m = mvrnorm(mcmcReps, coef(l.m), vcov(l.m), empirical = FALSE)
      mcmc.y = mvrnorm(mcmcReps, coef(l.y), vcov(l.y), empirical = FALSE)
      if(btm.y == 0){
        mcmc.y = cbind(mcmc.y, 0)
        colnames(mcmc.y)[ncol(mcmc.y)] = "t:m"
      }
        
      # Calculate causal effects
      if(scale.m == "continuous" & scale.y == "continuous"){
        if(effect == "TIE")
          effect.est = mcmc.m[, "t"] * (mcmc.y[, "m"] + mcmc.y[, "t:m"] * treat.t) * (treat.t - control.t)
        if(effect == "PDE"){
          if(n.x == 1){
            if(n.x != 0){
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * control.t + mcmc.m[, colnames(X)] * apply(X, 2, mean)))
            } else {
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * control.t))
            }
          } else {
            if(n.x != 0){
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * control.t + mcmc.m[, colnames(X)] %*% apply(X, 2, mean)))
            } else {
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * control.t))
            }
          }
        }
          
        if(effect == "PIE")
          effect.est = mcmc.m[, "t"] * (mcmc.y[, "m"] + mcmc.y[, "t:m"] * control.t) * (treat.t - control.t)
        if(effect == "TDE"){
          if(n.x == 1){
            if(n.x != 0){
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * treat.t + mcmc.m[, colnames(X)] * apply(X, 2, mean)))
            } else {
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * treat.t))
            }
          } else {
            if(n.x != 0){
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * treat.t + mcmc.m[, colnames(X)] %*% apply(X, 2, mean)))
            } else {
              effect.est = (treat.t - control.t) * (mcmc.y[, "t"] + mcmc.y[, "t:m"] * (mcmc.m[, 1] + mcmc.m[, "t"] * treat.t))
            }
          }
        }
        if(effect == "INT")
          effect.est = mcmc.m[, "t"] * mcmc.y[, "t:m"] * (treat.t - control.t)^2
      }
      
      if(scale.m == "binary" & scale.y == "continuous"){
        pm = function(t.prime.value){
          if(n.x != 0){
            return(pnorm(mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value + mcmc.m[, colnames(X)] %*% t(data[, colnames(X)])))
          } else {
            return(pnorm(mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value))
          }
        }
        if(effect == "TIE")
          effect.est = (mcmc.y[, "m"] + mcmc.y[, "t:m"] * treat.t) * (pm(treat.t) - pm(control.t))
        if(effect == "PDE")
          effect.est = (treat.t - control.t) * (mcmc.y[, "t:m"] * pm(control.t) + mcmc.y[, "t"])
        if(effect == "PIE")
          effect.est = (mcmc.y[, "m"] + mcmc.y[, "t:m"] * control.t) * (pm(treat.t) - pm(control.t))
        if(effect == "TDE")
          effect.est = (treat.t - control.t) * (mcmc.y[, "t:m"] * pm(treat.t) + mcmc.y[, "t"])
        if(effect == "INT")
          effect.est = (treat.t - control.t) * mcmc.y[, "t:m"] * (pm(treat.t) - pm(control.t))
        
        if(n.x != 0){
          effect.est = apply(effect.est, 1, mean)
        }
      }
      
      if(scale.m == "continuous" & scale.y == "binary"){
        py = function(t.value, t.prime.value){
          if(n.x != 0){
            numerator = mcmc.y[, 1] + mcmc.y[, "t"] * t.value + (mcmc.y[, "m"] + mcmc.y[, "t:m"] * t.value) * (mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value + mcmc.m[, colnames(X)] %*% t(data[, colnames(X)])) + mcmc.y[, colnames(X)] %*% t(data[, colnames(X)])
          } else {
            numerator = mcmc.y[, 1] + mcmc.y[, "t"] * t.value + (mcmc.y[, "m"] + mcmc.y[, "t:m"] * t.value) * (mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value)
          }
          denominator = sqrt(((mcmc.y[, "m"] + mcmc.y[, "t:m"] * t.value) * sd.m)^2 + 1)
          return(pnorm(numerator/denominator))
        }
        if(effect == "TIE"){
          y1m1 = py(treat.t, treat.t)
          y1m0 = py(treat.t, control.t)
          effect.est = y1m1 - y1m0
        }
        if(effect == "PDE"){
          y1m0 = py(treat.t, control.t)
          y0m0 = py(control.t, control.t)
          effect.est = y1m0 - y0m0
        }
        if(effect == "PIE"){
          y0m1 = py(control.t, treat.t)
          y0m0 = py(control.t, control.t)
          effect.est = y0m1 - y0m0
        }
        if(effect == "TDE"){
          y1m1 = py(treat.t, treat.t)
          y0m1 = py(control.t, treat.t)
          effect.est = y1m1 - y0m1
        }
        if(effect == "INT"){
          y1m1 = py(treat.t, treat.t)
          y1m0 = py(treat.t, control.t)
          y0m1 = py(control.t, treat.t)
          y0m0 = py(control.t, control.t)
          effect.est = (y1m1 - y1m0) - (y0m1 - y0m0)
        }
        if(n.x != 0){
          effect.est = apply(effect.est, 1, mean)
        }
      }
      
      if(scale.m == "binary" & scale.y == "binary"){
        pm = function(t.prime.value){
          if(n.x != 0){
            return(pnorm(mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value + mcmc.m[, colnames(X)] %*% t(data[, colnames(X)])))
          } else {
            return(pnorm(mcmc.m[, 1] + mcmc.m[, "t"] * t.prime.value))
          }
        }
        py = function(t.value, m.value){
          if(n.x != 0){
            return(pnorm(mcmc.y[, 1] + mcmc.y[, "t"] * t.value + (mcmc.y[, "m"] + mcmc.y[, "t:m"] * t.value) * m.value + mcmc.y[, colnames(X)] %*% t(data[, colnames(X)])))
          } else {
            return(pnorm(mcmc.y[, 1] + mcmc.y[, "t"] * t.value + (mcmc.y[, "m"] + mcmc.y[, "t:m"] * t.value) * m.value))
          }
        }
        if(effect == "TIE"){
          effect.est = (py(treat.t, 1) - py(treat.t, 0)) * (pm(treat.t) - pm(control.t))
        }
        if(effect == "PDE"){
          py.diff.1 = (py(treat.t, 1) - py(control.t, 1))
          py.diff.0 = (py(treat.t, 0) - py(control.t, 0))
          effect.est = (py.diff.1 - py.diff.0) * pm(control.t) + py.diff.0
        }
        if(effect == "PIE"){
          effect.est = (py(control.t, 1) - py(control.t, 0)) * (pm(treat.t) - pm(control.t))
        }
        if(effect == "TDE"){
          py.diff.1 = (py(treat.t, 1) - py(control.t, 1))
          py.diff.0 = (py(treat.t, 0) - py(control.t, 0))
          effect.est = (py.diff.1 - py.diff.0) * pm(treat.t) + py.diff.0
        }
        if(effect == "INT"){
          effect.est = ((py(treat.t, 1) - py(treat.t, 0)) - (py(control.t, 1) - py(control.t, 0))) * (pm(treat.t) - pm(control.t))
        }
        if(n.x != 0){
          effect.est = apply(effect.est, 1, mean)
        }
      }
      
      # Calculate confidence intervals
      low = (sig/100)/2
      upp = 1 - (sig/100)/2
      LL = quantile(effect.est, low)
      UL = quantile(effect.est, upp)
      
      # Is rep significant?
      return(sum(LL * UL > 0))
    }, silent = TRUE)
  }
  
  # Create vector of sample sizes
  Nvec = rep(seq(Nlow, Nhigh, Nsteps), powReps)
  
  # Run power analysis
  set.seed(seed)
  pow.ori = unlist(lapply(Nvec, powRep))
  pow = pow.ori = as.numeric(pow.ori)
  if(any(is.na(pow.ori))){
    Nvec = Nvec[-which(is.na(pow.ori))]
    pow = pow.ori[-which(is.na(pow.ori))]
  }

  mean.pow = NULL
  for(i in unique(Nvec)){
    mean.pow = c(mean.pow, mean(pow[which(Nvec == i)]))
  }
  data.power = cbind.data.frame(Nvec = unique(Nvec), power = mean.pow)
  
  # Run power analysis 
  loess.fit = suppressWarnings(loess(power ~ Nvec, data = data.power))
  if (obj == "choose_power"){
    TarN = round(approx(x = loess.fit$fitted, y = data.power$Nvec, xout = TarPow)$y)
  }
  if (obj == "choose_n"){
    TarPow = predict(loess.fit, newdata = data.frame(Nvec = TarN))
  }
  if(TarN %in% Nvec){
    data.power = cbind.data.frame(Sample_Size = c(data.power$Nvec[-which(data.power$Nvec == TarN)], TarN), Power = c(data.power$power[-which(data.power$Nvec == TarN)], TarPow))
  } else {
    data.power = cbind.data.frame(Sample_Size = c(data.power$Nvec, TarN), Power = c(data.power$power, TarPow))
  }
  data.power = data.power[order(data.power$Power, decreasing = FALSE), ]
})

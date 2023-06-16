#'=============================================================================
#' @name sim_2arm_prepost
#'
#' @title Simulate Pretest-Postest data
#'
#' @description Generates simulated data for use in study sample size planning
#'   for a 2 arm, pretest/posttest design.
#'
#' @param Npg An integer value for the number of subjects per group.
#'
#' @param VNames A character vector with the names of the variables, in the
#'  order they will be used in the population matrix row and column names. These
#'  will be used as dimnames. It defaults to the list of two variable names (x
#'  and y), where x is the pretest data and y is the posttest data.
#'
#' @param R A population correlation matrix. This will be used by
#'   mvtnorm::rmvnorm(). The default value is arbitrary.
#'
#' @param MV.C A numeric vector of means for the control group. This must be the
#'   same length as VNames and in the same order. This will be used by
#'   mvtnorm::rmvnorm(). It defaults to means on 1.
#'
#' @param MV.T A numeric vector of means for the treatment group. This must be
#'   the same length as VNames and in the same order. This will be used by
#'   mvtnorm::rmvnorm().
#'
#' @param SV.C A numeric vector of standard deviations for the control group.
#'   This must be the same length as VNames and in the same order. This will be
#'   used by mvtnorm::rmvnorm().
#'
#' @param SV.T A numeric vector of standard deviations for the control group.
#'   This must be the same length as VNames and in the same order. This will be
#'   used by mvtnorm::rmvnorm().
#'
#' @details This function generates simulated data for use in study sample size
#'   planning, based on the supplied inputs. It is strictly designed to work
#'   with a two-group, pretest/posttest design where the planned analysis would
#'   be a a simple ANCOVA model. The simulation creates separate data frames for
#'   the control and treatment groups that each contain pretest data (x) and
#'   posttest data (y). The two groups always have equal sample sizes.
#'
#'   PID is a person level identifier, and Arm indicates whether the person
#'   belongs to the control or treatment group.
#'
#' @return A tibble or a data frame.
#'
#' @importFrom mvtnorm rmvnorm
#' @importFrom MBESS cor2cov
#' @importFrom dplyr full_join
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' # Load packages.
#' library(dplyr)
#' library(mvtnorm)
#' library(MBESS)
#' # Default settings.
#' set.seed(1528)
#' sim_2arm_prepost()
#'
#' @export
sim_2arm_prepost <- function(Npg = 5,
                             VNames = c("x", "y"),
                             R = matrix(data = c( 1.0000, 0.5000,
                                                  0.5000, 1.0000),
                                        nrow = 2, ncol = 2, byrow = TRUE),
                             MV.C = c(x = 0, y = 0),
                             MV.T = c(x = 0, y = 1),
                             SV.C = c(x = 1, y = 1),
                             SV.T = c(x = 1, y = 1)){
  # Bind local variables to the function to avoid "no visible binding for global
  # variable" notes from devtools::check()
  Arm <- NULL
  PID <- NULL
  x   <- NULL

  # Add names to R matrix
  dimnames(R) <- list(VNames, VNames)

  # Convert population correlation matrix R to group-specific population
  # covariance matrices by using the standard deviation vectors.
  CV.C <- cor2cov(cor.mat = R, sd = SV.C)
  CV.T <- cor2cov(cor.mat = R, sd = SV.T)
  # Create control group data frame in wide (multivariate) format.
  DF.C <- data.frame(Arm = 0,
                     PID = 1:Npg,
                     rmvnorm(n = Npg, mean = MV.C, sigma = CV.C))
  # Create treatment group data frame in wide (multivariate) format.
  DF.T <- data.frame(Arm = 1,
                     PID = (Npg + 1:Npg),
                     rmvnorm(n = Npg, mean = MV.T, sigma = CV.T))
  # Combine control and treatment group data frames, then convert arm and pid
  # to factors.
  WDF.CT <- full_join(DF.C, DF.T, by = c("Arm", "PID", "x", "y")) %>%
            mutate(Arm = factor(Arm, levels = 0:1, labels = c("Ctl", "Trt")),
                   PID = factor(PID),
                   # Create centered version of x
                   cx = x - mean(x))
  # Return the resulting data.
  return(WDF.CT)
}

#'=============================================================================

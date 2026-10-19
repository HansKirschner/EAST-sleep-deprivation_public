#######################################################################
#                         EAST Main Model Fitting                     #
#######################################################################
EAST_linear_models <- function() {
  packages <- c("brms", "dplyr")
  # Function to check if packages are installed, install them if not, and load them
  load_packages <- function(packages) {
    for (package in packages) {
      if (!require(package, character.only = TRUE)) {
        install.packages(package)
        if (!require(package, character.only = TRUE)) {
          stop(paste("Package", package, "not found and installation failed. Please install it manually."))
        }
      }
      library(package, character.only = TRUE)
    }
  }
  load_packages(packages)

  data <- read.csv('data_clean.csv')
  data$RuleType <- factor(data$RuleType)
  data$Button <- factor(data$Button)
  
  #######################################################################
  #                      Response Time Linear Model                     #
  #######################################################################
  
  
  inform_priors_big <- c(
            # Fixed effects
            prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
            prior(normal(0.02, 0.04), class = b, coef = Congruency),
            prior(normal(0.03, 0.04), class = b, coef = Error),
            prior(normal(-0.06,0.05), class = b, coef = Block),
            prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
            prior(normal(0.14, 0.1), class = b, coef = RuleType2),
            prior(normal(0, 0.04), class = b, coef = PrevAcc),
            prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
            prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
            prior(normal(-0.03, 0.06), class = b, coef = RSI),
            prior(normal(0,0.05), class = b, coef = Congruency:Block),
            prior(normal(0.01, 0.04), class = b, coef = Block:Error),
            prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
            prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
            prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
            prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
            prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),
            prior(normal(0.02, 0.05), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
            prior(normal(-0.01, 0.05), class = b, coef = Congruency:PrevSubjAcc),
            prior(normal(-0.00, 0.05), class = b, coef = Congruency:PrevAcc),

            prior(normal(0, 0.2), class = b, coef = Button1),
            prior(normal(0, 0.2), class = b, coef = Button2),
            prior(normal(0, 0.2), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
            prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
            prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
            prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
            prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
            prior(normal(0, 0.2), class = b, coef = SleepOrder),
            prior(normal(0, 0.2), class = b, coef = TSD),
            prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
            prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
            prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
            prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
            prior(normal(0,0.2), class = b, coef = Congruency:TSD),
            prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
            prior(normal(0,0.2), class = b, coef = TSD:Error),
            prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
            prior(normal(0,0.2), class = b, coef = TSD:Block),
            # Random effects
            prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )

  big_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                      (RSI) +
                      (Button) +
                      (PrevAcc * PrevSubjAcc * Congruency * TSD) +
                      (Error * TSD * Block) +
                      (SleepOrder) +
                      (TSD * RuleType * Block) +
                      (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                    data = data,
                    prior = inform_priors_big,
                    control = list(adapt_delta = 0.99, max_treedepth = 12),
                    save_pars = save_pars(all = TRUE),
                    threads = threading(4),
                    cores = 4,
                    iter = 4000)
  save(big_model, file = "EAST_ButtonmodelRT.RData")
  inform_priors_noButton <- c(
    # Fixed effects
    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
    prior(normal(0.02, 0.04), class = b, coef = Congruency),
    prior(normal(0.03, 0.04), class = b, coef = Error),
    prior(normal(-0.06,0.05), class = b, coef = Block),
    prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
    prior(normal(0.14, 0.1), class = b, coef = RuleType2),
    prior(normal(0, 0.04), class = b, coef = PrevAcc),
    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = RSI),
    prior(normal(0,0.05), class = b, coef = Congruency:Block),
    prior(normal(0.01, 0.04), class = b, coef = Block:Error),
    prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
    prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
    prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
    prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
    prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),
    prior(normal(0.02, 0.05), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
    prior(normal(-0.01, 0.05), class = b, coef = Congruency:PrevSubjAcc),
    prior(normal(-0.00, 0.05), class = b, coef = Congruency:PrevAcc),

    prior(normal(0, 0.2), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
    prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
    prior(normal(0, 0.2), class = b, coef = SleepOrder),
    prior(normal(0, 0.2), class = b, coef = TSD),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
    prior(normal(0,0.2), class = b, coef = TSD:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block),
    # Random effects
    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )

  noButton_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                     (RSI) +
                     (PrevAcc * PrevSubjAcc * Congruency * TSD) +
                     (Error * TSD * Block) +
                     (SleepOrder) +
                     (TSD * RuleType * Block) +
                     (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                   data = data,
                   prior = inform_priors_noButton,
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   save_pars = save_pars(all = TRUE),
                   threads = threading(4),
                   cores = 4,
                   iter = 4000)
  save(noButton_model, file = "EAST_NoButtonmodelRT.RData")

  inform_priors <- c(
                    # Fixed effects
                    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
                    prior(normal(0.02, 0.04), class = b, coef = Congruency),
                    prior(normal(0.03, 0.04), class = b, coef = Error),
                    prior(normal(-0.06,0.05), class = b, coef = Block),
                    prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
                    prior(normal(0.14, 0.1), class = b, coef = RuleType2),
                    prior(normal(0, 0.04), class = b, coef = PrevAcc),
                    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
                    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
                    prior(normal(-0.03, 0.06), class = b, coef = RSI),
                    prior(normal(0,0.05), class = b, coef = Congruency:Block),
                    prior(normal(0.01, 0.04), class = b, coef = Block:Error),
                    prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
                    prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
                    prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
                    prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
                    prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
                    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
                    prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
                    prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
                    prior(normal(0, 0.2), class = b, coef = SleepOrder),
                    prior(normal(0, 0.2), class = b, coef = TSD),
                    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                    prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
                    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
                    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
                    prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
                    prior(normal(0,0.2), class = b, coef = TSD:Error),
                    prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
                    prior(normal(0,0.2), class = b, coef = TSD:Block),
                    # Random effects
                    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )


main_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                             (RSI) +
                              (Error * TSD * Block) +
                               (SleepOrder) +
                              (TSD * RuleType * Block) +
                              (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                  data = data,
                  prior = inform_priors,
                  control = list(adapt_delta = 0.99, max_treedepth = 12),
                  save_pars = save_pars(all = TRUE),
                  threads = threading(4),
                  cores = 4,
                  iter = 4000)

  #summary(main_model)
  save(main_model, file = "EAST_modelRT.RData")

  priors2 <- c(
    # Fixed effects
    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
    prior(normal(0.02, 0.04), class = b, coef = Congruency),
    prior(normal(0.03, 0.04), class = b, coef = Error),
    #prior(normal(-0.06,0.05), class = b, coef = Block),
    prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
    prior(normal(0.14, 0.1), class = b, coef = RuleType2),
    prior(normal(0, 0.04), class = b, coef = PrevAcc),
    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = RSI),
    #prior(normal(0,0.05), class = b, coef = Congruency:Block),
    #prior(normal(0.01, 0.04), class = b, coef = Block:Error),
    #prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
    #prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
    #prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
    #prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
    #prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),

    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
    prior(normal(0, 0.2), class = b, coef = SleepOrder),
    prior(normal(0, 0.2), class = b, coef = TSD),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
    #prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
    prior(normal(0,0.2), class = b, coef = TSD:Error),
    #prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
    #prior(normal(0,0.2), class = b, coef = TSD:Block),
    # Random effects
    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )

  NoBlock_model <- brm(RTlog ~ 1 + (Congruency * TSD) +
                     (RSI) +
                     (Error * TSD) +
                     (SleepOrder) +
                    (TSD * RuleType) +
                     (TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                   data = data,
                   prior = priors2,
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   save_pars = save_pars(all = TRUE),
                   threads = threading(4),
                  cores = 4,
                   iter = 4000)

  save(NoBlock_model, file = "EAST_NoBlockmodelRT.RData")

  priors3 <- c(
    # Fixed effects
    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
    prior(normal(0.02, 0.04), class = b, coef = Congruency),
    prior(normal(0.03, 0.04), class = b, coef = Error),
    prior(normal(-0.06,0.05), class = b, coef = Block),
    #prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
    #prior(normal(0.14, 0.1), class = b, coef = RuleType2),
    prior(normal(0, 0.04), class = b, coef = PrevAcc),
    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = RSI),
    prior(normal(0,0.05), class = b, coef = Congruency:Block),
    prior(normal(0.01, 0.04), class = b, coef = Block:Error),
    #prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
    #prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
    prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
    prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
    prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),

    #prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:TrialType),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:Block:TrialType),
    prior(normal(0, 0.2), class = b, coef = TrialType),
    prior(normal(0, 0.2), class = b, coef = Block:TrialType),
    prior(normal(0, 0.2), class = b, coef = SleepOrder),
    prior(normal(0, 0.2), class = b, coef = TSD),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
    prior(normal(0,0.2), class = b, coef = TSD:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block),
    # Random effects
    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )


  TrialType_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                     (RSI) +
                     (Error * TSD * Block) +
                    (SleepOrder) +
                     (TSD * TrialType * Block) +
                     (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                   data = data,
                   prior = priors3,
                   control = list(adapt_delta = 0.99, max_treedepth = 12),
                   save_pars = save_pars(all = TRUE),
                   threads = threading(4),
                   cores = 4,
                   iter = 4000)

  save(TrialType_model, file = "EAST_TrialTypemodelRT.RData")

  priors4 <- c(
    # Fixed effects
    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
    prior(normal(0.02, 0.04), class = b, coef = Congruency),
    prior(normal(0.03, 0.04), class = b, coef = Error),
    #prior(normal(-0.06,0.05), class = b, coef = Block),
    prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
    prior(normal(0.14, 0.1), class = b, coef = RuleType2),
    prior(normal(0, 0.04), class = b, coef = PrevAcc),
    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = RSI),
    #prior(normal(0,0.05), class = b, coef = Congruency:Block),
    #prior(normal(0.01, 0.04), class = b, coef = Block:Error),
    #prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
    #prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
    #prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
    #prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
    #prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),

    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
    prior(normal(0, 0.2), class = b, coef = SleepOrder),
    prior(normal(0, 0.2), class = b, coef = TSD),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc:RuleType2),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
    #prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
    prior(normal(0,0.2), class = b, coef = TSD:Error),
    #prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
    #prior(normal(0,0.2), class = b, coef = TSD:Block),
    # Random effects
    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )

  NoBlockRuleType_model <- brm(RTlog ~ 1 + (Congruency * TSD) +
                        (RSI) +
                        (Error * TSD) +
                        (SleepOrder) +
                        (TSD * PrevAcc * PrevSubjAcc * RuleType) + (1 || SubInfo),
                      data = data,
                      prior = priors4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      save_pars = save_pars(all = TRUE),
                      threads = threading(4),
                      cores = 4,
                      iter = 4000)

  save(NoBlockRuleType_model, file = "EAST_NoBlockRuleTypemodelRT.RData")



  priors5 <- c(
    # Fixed effects
    prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
    prior(normal(0.02, 0.04), class = b, coef = Congruency),
    prior(normal(0.03, 0.04), class = b, coef = Error),
    prior(normal(-0.06,0.05), class = b, coef = Block),
    prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
    prior(normal(0.14, 0.1), class = b, coef = RuleType2),
    prior(normal(0, 0.04), class = b, coef = PrevAcc),
    prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
    prior(normal(-0.03, 0.06), class = b, coef = RSI),
    prior(normal(0,0.05), class = b, coef = Congruency:Block),
    prior(normal(0.01, 0.04), class = b, coef = Block:Error),
    #prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
    #prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
    prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
    prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
    prior(normal(-0.03, 0.05), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),

    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
    prior(normal(0, 0.2), class = b, coef = Congruency:RuleType1),
    prior(normal(0, 0.2), class = b, coef = Congruency:RuleType2),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
    #prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
    prior(normal(0, 0.2), class = b, coef = SleepOrder),
    prior(normal(0, 0.2), class = b, coef = TSD),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType1:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:RuleType2:PrevAcc:PrevSubjAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
    prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD),
    prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
    prior(normal(0,0.2), class = b, coef = TSD:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block:Error),
    prior(normal(0,0.2), class = b, coef = TSD:Block),
    # Random effects
    prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
  )

  ConruencyRuleType_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                         (Congruency * RuleType * TSD) +
                         (RSI) +
                         (Error * TSD * Block) +
                         (SleepOrder) +
                         (TSD * PrevAcc * PrevSubjAcc * RuleType) +
                         (Block * TSD  * PrevAcc * PrevSubjAcc) +
                         (1 || SubInfo),
                       data = data,
                        prior = priors5,
                        control = list(adapt_delta = 0.99, max_treedepth = 12),
                        save_pars = save_pars(all = TRUE),
                        threads = threading(4),
                        cores = 4,
                        iter = 4000)
  #
   save(ConruencyRuleType_model, file = "EAST_CongRuleTypemodelRT.RData")

  #######################################################################
  #                        Accuracy Logistic Model                      #
  #######################################################################

  # Specifying the priors manually - important to specify them in the same order the model is later specified
  inform_priors <- c(prior(normal(1.99,0.5), class = Intercept),
                     prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                     prior(normal(0.48, 0.16), class = b, coef = Block),
                     prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                     prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
                     prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                     prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                     prior(normal(-0.10, 0.08), class = b, coef = RSI),
                     prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                     prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                     prior(normal(-0.12, 0.24), class = b, coef = Block:RuleType2),
                     prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                     prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                     prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                     prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                     prior(normal(0, 0.5), class = b, coef = SleepOrder),
                     prior(normal(0, 0.5), class = b, coef = TSD),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
                     prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )

    log_model_main <- brm(ACC ~ 1 + (Congruency * TSD * Block) +
                            (RSI) +
                            (SleepOrder) +
                            (TSD * RuleType * Block) +
                            (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                          data = data,
                          prior = inform_priors,
                          save_pars = save_pars(all = TRUE),
                          threads = threading(4),
                          cores = 4,
                          iter = 4000,
                          family = bernoulli(link = logit))
  # # Saving the model
   save(log_model_main, file = "EAST_modelACC.RData")

  inform_priors2 <- c(prior(normal(1.99,0.5), class = Intercept),
                     prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                     #prior(normal(0.48, 0.16), class = b, coef = Block),
                     prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                     prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
                     prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                     prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                     prior(normal(-0.10, 0.08), class = b, coef = RSI),
                     #prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                     #prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                     #prior(normal(-0.12, 0.24), class = b, coef = Block:RuleType2),
                     prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                     #prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                     #prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                     #prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                     #prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                     prior(normal(0, 0.5), class = b, coef = SleepOrder),
                     prior(normal(0, 0.5), class = b, coef = TSD),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                     #prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
                     prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )
       log_NoBlockmodel_main <- brm(ACC ~ 1 + (Congruency * TSD) +
                             (RSI) +
                             (SleepOrder) +
                             (TSD * RuleType) +
                             (TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                            data = data,
 		                        control = list(adapt_delta = 0.99, max_treedepth = 12),
                       	    save_pars = save_pars(all = TRUE),
                       	    threads = threading(4),
                            prior = inform_priors2,
                            iter = 4000,
                            family = bernoulli(link = logit))
#
#   # Saving the model
   save(log_NoBlockmodel_main, file = "EAST_NoBlockmodelACC.RData")

  inform_priors3 <- c(prior(normal(1.99,0.5), class = Intercept),
                      prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                      prior(normal(0.48, 0.16), class = b, coef = Block),
                      #prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                      prior(normal(-1.08, 0.2), class = b, coef = TrialType),
                      prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                      prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                      prior(normal(-0.10, 0.08), class = b, coef = RSI),
                      prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                      #prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                      prior(normal(-0.12, 0.24), class = b, coef = Block:TrialType),
                      prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                      prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                      prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                      prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                      prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                      prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                      prior(normal(0, 0.5), class = b, coef = SleepOrder),
                      prior(normal(0, 0.5), class = b, coef = TSD),
                      prior(normal(0, 0.5), class = b, coef = TSD:Block),
                      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                      prior(normal(0, 0.5), class = b, coef = TSD:Block:TrialType),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                     #prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                      prior(normal(0, 0.5), class = b, coef = TSD:TrialType),
                      prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )

    log_TrialTypemodel_main <- brm(ACC ~ 1 + (Congruency * TSD * Block) +
                                    (RSI) +
                                    (SleepOrder) +
                                    (TSD * Block *TrialType) +
                                    (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                                  data = data,
  			control = list(adapt_delta = 0.99, max_treedepth = 12),
                       	save_pars = save_pars(all = TRUE),
                       	threads = threading(4),
                                  prior = inform_priors3,
                                  iter = 4000,
                                  family = bernoulli(link = logit))
 #
 #  #  Saving the model
   save(log_TrialTypemodel_main, file = "EAST_TrialTypemodelACC.RData")

  inform_priors4 <- c(prior(normal(1.99,0.5), class = Intercept),
                      prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                      #prior(normal(0.48, 0.16), class = b, coef = Block),
                      prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                      prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
                      prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                      prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                      prior(normal(-0.10, 0.08), class = b, coef = RSI),
                      #prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                      #prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                      #prior(normal(-0.12, 0.24), class = b, coef = Block:RuleType2),
                      prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                      #prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                      #prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                      #prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                      prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                      #prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                      prior(normal(0, 0.5), class = b, coef = SleepOrder),
                      prior(normal(0, 0.5), class = b, coef = TSD),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc:RuleType1),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc:RuleType2),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                      #prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                      prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                      prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
                      prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )

     log_NoBlockRuleTypemodel_main <- brm(ACC ~ 1 + (Congruency * TSD) +
                                      (RSI) +
                                      (SleepOrder) +
                                      (TSD * PrevAcc * PrevSubjAcc * RuleType) + (1 || SubInfo),
                                    data = data,
                                    prior = inform_priors4,
 				control = list(adapt_delta = 0.99, max_treedepth = 12),
                       		save_pars = save_pars(all = TRUE),
                       		threads = threading(4),
                                    iter = 4000,
                                  family = bernoulli(link = logit))
#   # Saving the model
   save(log_NoBlockRuleTypemodel_main, file = "EAST_NoBlockRuleTypemodelACC.RData")

  inform_priors_big <- c(prior(normal(1.99,0.5), class = Intercept),
                     prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                     prior(normal(0.48, 0.16), class = b, coef = Block),
                     prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                     prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
                     prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                     prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                     prior(normal(-0.10, 0.08), class = b, coef = RSI),
                     prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                     prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                     prior(normal(-0.12, 0.24), class = b, coef = Block:RuleType2),
                     prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                     prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                     prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                     prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                     prior(normal(-0.08, 0.15), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
                     prior(normal(-0.07, 0.15), class = b, coef = Congruency:PrevSubjAcc),
                     prior(normal(-0.05, 0.15), class = b, coef = Congruency:PrevAcc),

                     prior(normal(0, 0.5), class = b, coef = Button1),
                     prior(normal(0, 0.5), class = b, coef = Button2),
                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                     prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                     prior(normal(0, 0.5), class = b, coef = SleepOrder),
                     prior(normal(0, 0.5), class = b, coef = TSD),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                     prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                     prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
                     prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )
   log_big_model <- brm(ACC ~ 1 + (Congruency * TSD * Block) +
                             (RSI) +
 			    (Button) +
                             (SleepOrder) +
                             (TSD * RuleType * Block) +
                             (PrevAcc * PrevSubjAcc * Congruency * TSD) +
                             (Block * TSD * PrevAcc * PrevSubjAcc) +
                             (1 || SubInfo),
                             data = data,
                             prior = inform_priors_big,
                             iter = 4000,
                             control = list(adapt_delta = 0.99, max_treedepth = 12),
                             save_pars = save_pars(all = TRUE),
                             threads = threading(4),
                             cores = 4,
                             family = bernoulli(link = logit))
#   # Saving the model
   save(log_big_model, file = "EAST_ButtonmodelACC.RData")

  inform_priors_noButton <- c(prior(normal(1.99,0.5), class = Intercept),
                         prior(normal(-0.08, 0.1), class = b, coef = Congruency),
                         prior(normal(0.48, 0.16), class = b, coef = Block),
                         prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
                         prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
                         prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
                         prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
                         prior(normal(-0.10, 0.08), class = b, coef = RSI),
                         prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
                         prior(normal(-0.23, 0.16), class = b, coef = Block:RuleType1),
                         prior(normal(-0.12, 0.24), class = b, coef = Block:RuleType2),
                         prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
                         prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
                         prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
                         prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

                         prior(normal(-0.08, 0.15), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
                         prior(normal(-0.07, 0.15), class = b, coef = Congruency:PrevSubjAcc),
                         prior(normal(-0.05, 0.15), class = b, coef = Congruency:PrevAcc),

                         prior(normal(0, 0.5), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
                         prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
                         prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
                         prior(normal(0, 0.5), class = b, coef = SleepOrder),
                         prior(normal(0, 0.5), class = b, coef = TSD),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
                         prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
                         prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                         prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
                         prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
                         prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
  )
   log_noButton_model <- brm(ACC ~ 1 + (Congruency * TSD * Block) +
                          (RSI) +
                          (SleepOrder) +
                          (TSD * RuleType * Block) +
                          (PrevAcc * PrevSubjAcc * Congruency * TSD) +
                          (Block * TSD * PrevAcc * PrevSubjAcc) +
                          (1 || SubInfo),
                        data = data,
                        prior = inform_priors_noButton,
                        iter = 4000,
                        control = list(adapt_delta = 0.99, max_treedepth = 12),
                        save_pars = save_pars(all = TRUE),
                        threads = threading(4),
                        cores = 4,
                        family = bernoulli(link = logit))
#
   save(log_noButton_model, file = "EAST_NoButtonmodelACC.RData")

  priors5 <-
    c(prior(normal(1.99,0.5), class = Intercept),
      prior(normal(-0.08, 0.1), class = b, coef = Congruency),
      prior(normal(0.48, 0.16), class = b, coef = Block),
      prior(normal(-0.67, 0.15), class = b, coef = RuleType1),
      prior(normal(-1.08, 0.2), class = b, coef = RuleType2),
      prior(normal(0.12, 0.15), class = b, coef = PrevAcc),
      prior(normal(-0.22, 0.15), class = b, coef = PrevSubjAcc),
      prior(normal(-0.10, 0.08), class = b, coef = RSI),

      prior(normal(-0.04, 0.10), class = b, coef = Congruency:Block),
      prior(normal(-0.04, 0.12), class = b, coef = PrevAcc:PrevSubjAcc),
      prior(normal(0.25, 0.15), class = b, coef = Block:PrevAcc),
      prior(normal(-0.26, 0.15), class = b, coef = Block:PrevSubjAcc),
      prior(normal(-0.07, 0.14), class = b, coef = Block:PrevAcc:PrevSubjAcc),

      prior(normal(-0.08, 0.15), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
      prior(normal(-0.07, 0.15), class = b, coef = Congruency:PrevSubjAcc),
      prior(normal(-0.05, 0.15), class = b, coef = Congruency:PrevAcc),

      prior(normal(0, 0.5), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = Congruency:TSD),
      prior(normal(0, 0.5), class = b, coef = Congruency:TSD:Block),
      prior(normal(0, 0.5), class = b, coef = SleepOrder),
      prior(normal(0, 0.5), class = b, coef = TSD),
      prior(normal(0, 0.5), class = b, coef = TSD:Block),
      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:Block:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType1),
      prior(normal(0, 0.5), class = b, coef = TSD:Block:RuleType2),
      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:RuleType1),
      prior(normal(0, 0.5), class = b, coef = TSD:RuleType2),
      prior(normal(0, 0.5), class = b, coef = Congruency:RuleType1),
      prior(normal(0, 0.5), class = b, coef = Congruency:RuleType2),
      prior(normal(0, 0.5), class = b, coef = TSD:RuleType1:PrevAcc:PrevSubjAcc),
      prior(normal(0, 0.5), class = b, coef = TSD:RuleType2:PrevAcc:PrevSubjAcc),
      prior(normal(0.68, 0.35), class = sd, coef = Intercept, group = SubInfo)
    )


  log_ConruencyRuleType_model <- brm(ACC ~ 1 + (Congruency * TSD * Block) +
                                   (Congruency * RuleType * TSD) +
                                   (RSI) +
                                   (SleepOrder) +
                                   (TSD * PrevAcc * PrevSubjAcc * RuleType) +
                                   (Block * TSD  * PrevAcc * PrevSubjAcc) +
                                   (1 || SubInfo),
                                 data = data,
                                 prior = priors5,
                                 control = list(adapt_delta = 0.99, max_treedepth = 12),
                                 save_pars = save_pars(all = TRUE),
                                 threads = threading(4),
                                 cores = 4,
                                 family = bernoulli(link = logit),
                                 iter = 4000)

  save(log_ConruencyRuleType_model, file = "EAST_CongRuleTypemodelACC.RData")


  # Model fitted only on correct trials
  
  data <- subset(data, data$ACC == 1)
  
  inform_priors_big <- c(
                # Fixed effects
                prior(normal(6.45,0.2), class = Intercept, lb = 4.6, ub = 8),
                prior(normal(0.02, 0.04), class = b, coef = Congruency),
                prior(normal(-0.06,0.05), class = b, coef = Block),
                prior(normal(-0.01, 0.04), class = b, coef = RuleType1),
                prior(normal(0.14, 0.1), class = b, coef = RuleType2),
                prior(normal(0, 0.04), class = b, coef = PrevAcc),
                prior(normal(0.07, 0.04), class = b, coef = PrevSubjAcc),
                prior(normal(-0.03, 0.06), class = b, coef = PrevAcc:PrevSubjAcc),
                prior(normal(-0.03, 0.06), class = b, coef = RSI),
                prior(normal(0,0.05), class = b, coef = Congruency:Block),
                prior(normal(0.01, 0.04), class = b, coef = Block:RuleType1),
                prior(normal(0.02, 0.05), class = b, coef = Block:RuleType2),
                prior(normal(0.01, 0.04), class = b, coef = Block:PrevAcc),
                prior(normal(-0.01, 0.04), class = b, coef = Block:PrevSubjAcc),
                prior(normal(-0.03, 0.05), class = b, coef = Block:PrevAcc:PrevSubjAcc),
                prior(normal(0.02, 0.05), class = b, coef = Congruency:PrevAcc:PrevSubjAcc),
                prior(normal(-0.01, 0.05), class = b, coef = Congruency:PrevSubjAcc),
                prior(normal(-0.00, 0.05), class = b, coef = Congruency:PrevAcc),

                prior(normal(0, 0.2), class = b, coef = Button1),
                prior(normal(0, 0.2), class = b, coef = Button2),
                prior(normal(0, 0.2), class = b, coef = Congruency:TSD:PrevAcc:PrevSubjAcc),
                prior(normal(0, 0.2), class = b, coef = TSD:RuleType1),
                prior(normal(0, 0.2), class = b, coef = TSD:RuleType2),
                prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType1),
                prior(normal(0, 0.2), class = b, coef = TSD:Block:RuleType2),
                prior(normal(0, 0.2), class = b, coef = SleepOrder),
                prior(normal(0, 0.2), class = b, coef = TSD),
                prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc:PrevSubjAcc),
                prior(normal(0, 0.2), class = b, coef = TSD:Block:PrevAcc:PrevSubjAcc),
                prior(normal(0, 0.2), class = b, coef = TSD:PrevAcc),
                prior(normal(0, 0.2), class = b, coef = TSD:PrevSubjAcc),
                prior(normal(0,0.2), class = b, coef = Congruency:TSD),
                prior(normal(0,0.2), class = b, coef = Congruency:TSD:Block),
                prior(normal(0,0.2), class = b, coef = TSD:Block),
                # Random effects
                prior(normal(0.16, 0.15), class = sd, coef = Intercept, group = SubInfo)
      )

      OnlyCorrect_model <- brm(RTlog ~ 1 + (Congruency * TSD * Block) +
                          (RSI) +
                          (Button) +
                          (PrevAcc * PrevSubjAcc * Congruency * TSD) +
                          (SleepOrder) +
                          (TSD * RuleType * Block) +
                          (Block * TSD * PrevAcc * PrevSubjAcc) + (1 || SubInfo),
                        data = data,
                        prior = inform_priors_big,
                        control = list(adapt_delta = 0.99, max_treedepth = 12),
                        save_pars = save_pars(all = TRUE),
                        threads = threading(4),
                        cores = 4,
                        iter = 4000)
      save(OnlyCorrect_model, file = "EAST_OnlyCorrectRT_revisionModel.RData")
  
}


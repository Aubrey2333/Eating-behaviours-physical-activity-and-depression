
covs4 <- names(data[,c(5:10)])

strata <- names(data[,c(20:24)])

summary = data.frame(matrix(NA, nrow = 0, ncol = 10))
colnames(summary) <- c("Y","X","beta","SE","t.value","CI_lower","CI_upper",
                       "p.value","classify","level")

med_summary = data.frame(matrix(NA, nrow = 0, ncol = 14))
colnames(med_summary) <- c("Y","X","total_estimate","total_estimate_CI","total_P",
                           "ACME_estimate","ACME_estimate_CI","ACME_P",
                           "ADE_estimate", "ADE_estimate_CI","ADE_P",
                           "prop_estimate","prop_estimate_CI","prop_P")






#### first loop: extract one Stratified variable
for (k in 1:length(strata)) {
  #k=1
  
  factor_variable_name <- strata[k]
  
  covs <- covs4[-(k+1)]
  
  ## split the data "sig_pro_dep" according to hierarchical variables
  unique_factors <- levels(data[[factor_variable_name]])
  split_data <- lapply(unique_factors, function(factor_level) {
    subset_data <- data[data[[factor_variable_name]] == factor_level, ]
    return(subset_data)
  })
  
  
  ### seecond loop:  extract one layer of stratified variable
  for (l in 1:length(split_data)) {
    #l=1
    df <- as.data.frame(split_data[[l]])
    #df <- df[,1:24]
    
    ## lm
    all_pro_cox(df,covs)
    
    
    
    ## mediation
    mediation_all_list <- list()
    mediation_summary_list <- list()
    
    
    for (i in 11:19) {
      #i=11
      formula1 <- as.formula(paste("GESE~",colnames(df)[i],
                                   "+",paste(covs, collapse = " + ")))
      
      fit1 <- lm(formula1, data = df)
      
      formula2 <- as.formula(paste("PHQ~", "GESE+",colnames(df)[i], 
                                   "+",paste(covs, collapse = " + ")))
      
      #fit2 <- glm(formula = formula2, data = data, family = binomial())
      fit2 <- lm(formula2, data = df)
      
      med.fit<- mediate(fit1, fit2, treat = colnames(df)[i], mediator ="GESE",
                        robustSE = TRUE, sims =1000)###treat填自变量，mediator填中介变量
      
      
      mediation <- summary(med.fit)
      
      # Standardization
      total_estimate <- signif(mediation$tau.coef, digits = 4)
      total_estimate_ci <- signif(mediation$tau.ci, digits = 4)
      total_estimate_CI <- paste(total_estimate_ci[1],"-",total_estimate_ci[2])
      total_P <- mediation$tau.p
      
      ACME_estimate <- signif(mediation$d.avg, digits = 4)
      ACME_estimate_ci <- signif(mediation$d.avg.ci, digits = 4)
      ACME_estimate_CI <- paste(ACME_estimate_ci[1],"-",ACME_estimate_ci[2])
      ACME_P <- mediation$d.avg.p
      
      ADE_estimate <- signif(mediation$z.avg, digits = 4)
      ADE_estimate_ci <- signif(mediation$z.avg.ci, digits = 4)
      ADE_estimate_CI <- paste(ADE_estimate_ci[1],"-",ADE_estimate_ci[2])
      ADE_P <- mediation$z.avg.p
      
      
      prop_estimate <- signif(mediation$n0, digits = 4)
      prop_estimate_ci <- signif(mediation$n0.ci, digits = 4)
      prop_estimate_CI <- paste(prop_estimate_ci[1],"-",prop_estimate_ci[2])
      prop_P <- mediation$n0.p
      
      
      
      # establish result data frame
      mediation_result <- data.frame(
        Y = "PHQ",
        X = colnames(data)[i],
        total_estimate <- total_estimate,
        total_estimate_CI <- total_estimate_CI,
        total_P <- total_P,
        
        ACME_estimate <- ACME_estimate,
        ACME_estimate_CI <- ACME_estimate_CI,
        ACME_P <- ACME_P,
        
        ADE_estimate <- ADE_estimate,
        ADE_estimate_CI <- ADE_estimate_CI,
        ADE_P <- ADE_P,
        
        prop_estimate <- prop_estimate,
        prop_estimate_CI <- prop_estimate_CI,
        prop_P <- prop_P
      )
      
      
      
      # extract all pro result
      mediation_summary_list[[i-10]] <- mediation_result
    }
    
    mediation_summary <- do.call(rbind, mediation_summary_list) 
    
    ## Label the strata factors and the number of levels
    lm_summary$classify <- factor_variable_name
    lm_summary$level <- l
    
    mediation_summary$classify <- factor_variable_name
    mediation_summary$level <- l
    
    summary <- rbind(summary,lm_summary)
    med_summary <- rbind(med_summary,mediation_summary)
  }
  
}

fwrite(summary, "all_strata_lm.csv")

fwrite(med_summary, "all_strata_med.csv")

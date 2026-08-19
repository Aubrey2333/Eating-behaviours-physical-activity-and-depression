library(data.table)
setwd("/data/")

data <- fread("data.txt")

data$sweet[data$sweet == 2] <- NA
data$sweet[data$sweet == 4] <- 2
table(data$sweet)

data$breakfast[data$breakfast == 1] <- 4
data$breakfast[data$breakfast == 3] <- 1
data$breakfast[data$breakfast == 4] <- 3
table(data$breakfast)

data$regular[data$regular == 1] <- 4
data$regular[data$regular == 3] <- 1
data$regular[data$regular == 4] <- 3
table(data$regular)



str(data)


dep$sex_cat <- dep$sex
dep$grade_cat <- ifelse(dep$grade %in% c(1, 2, 3), 1, 
                        ifelse(dep$grade %in% c(4, 5, 6), 2, NA))

dep$huji_cat <- dep$huji
dep$BMI_cat <- cut(dep$BMI, breaks = c(0, 24, Inf), labels = c(1, 2), right = FALSE)


dep_all$nation_cat <- dep_all$nation

dep_all[, c(20:24)] <- lapply(dep_all[, c(20:24)], factor)

dep_all$status <- ifelse(dep_all$PHQ < 10, 0, 1)

single_pro_cox <- function(lm_model) {
  lm_result <- summary(lm_model)
  
  # Standardization
  p.value <- signif(lm_result$coefficients[, "Pr(>|t|)"], digits = 3)
  t.value <- signif(lm_result$coefficients[, "t value"], digits = 3)
  beta <- signif(coef(lm_model), digits = 3)
  SE <- signif(lm_result$coefficients[, "Std. Error"], digits = 3)
  CI <- confint(lm_model, level = 0.95)
  CI_lower <- signif(CI[, "2.5 %"], digits = 3)
  CI_upper <- signif(CI[, "97.5 %"], digits = 3)
  
  
  # establish result data frame
  lm_result <- data.frame(
    Y = "PHQ_bi",
    beta = beta,
    SE = SE,
    t.value  = t.value,
    p.value = p.value,
    CI_lower <- CI_lower,
    CI_upper <- CI_upper
  )
  
  lm_result$X <- row.names(lm_result)
  
  lm_result <- lm_result[,c(1,8,2:7)]
  
  lm_result <- lm_result[-1,]
  
  assign("lm_result", lm_result, envir = .GlobalEnv)
}

all_pro_cox <- function(data, covs) {
  
  bi_summary_list <- list()
  
  for (i in 11:19) {
    #i=14
    formula <- as.formula(paste("status ~", colnames(data)[i], "+",paste(covs, collapse = " + ")))
    
    #formula <- as.formula(paste("PHQ_bi ~", colnames(data)[i]))
    
    model <- glm(formula = formula, data = data, family = binomial())
    
    glm_result <- summary(model)
    
    # extract coef
    p.value <- signif(glm_result$coefficients[, "Pr(>|z|)"], digits = 3)
    z.value <- signif(glm_result$coefficients[, "z value"], digits = 3)
    beta <- signif(glm_result$coefficients[, "Estimate"], digits = 3)
    
    OR <- signif(odds.ratio(model)[,-4], digits=2)
    OR <- paste0(OR$OR, " (", OR$`2.5 %`, "-", OR$`97.5 %`, ")")
    
    glm_result <- data.frame(
      Y = "PHQ_bi",
      beta = beta,
      z.value  = z.value ,
      p.value = p.value,
      OR = OR
    )
    
    glm_result$X <- row.names(glm_result)
    
    glm_result <- glm_result[,c(1,6,2:5)]
    
    glm_result <- glm_result[-1,]
    
    
    bi_summary_list[[i-10]] <- glm_result[grep(colnames(data)[i], glm_result$X),]
  }
  
  bi_summary <- do.call(rbind, bi_summary_list)
  assign("bi_summary", bi_summary, envir = .GlobalEnv)
  
}


M_cox <- function(data, covs) {
  
  M_summary_list <- list()
  
  for (i in 11:19) {
    formula <- as.formula(paste("GESE ~", colnames(data)[i], "+",paste(covs, collapse = " + ")))
    #formula <- as.formula(paste("GESE ~", colnames(data)[i]))
    
    lm_model <- lm(formula, data = data)
    
    summary(lm_model)
    
    single_pro_cox(lm_model)
    
    
    # extract all pro result
    M_summary_list[[i-10]] <- lm_result[grep(colnames(data)[i], lm_result$X) ,]
  }
  
  M_summary <- do.call(rbind, M_summary_list)
  
  assign("M_summary", M_summary, envir = .GlobalEnv)
  
  
}

covs1 <- NULL
covs2 <- names(data[,c(5:6)])
covs3 <- names(data[,c(5:10)])
cov_list <- list(covs1, covs2, covs3)


out_summary = data.frame(matrix(NA, nrow = 0, ncol = 9))
colnames(out_summary) <- c("Y","X","beta","SE","t.value",
                           "p.value"," CI_lower"," CI_upper","model")

Media_summary = data.frame(matrix(NA, nrow = 0, ncol = 9))
colnames(Media_summary) <- c("Y","X","beta","SE","t.value",
                             "p.value"," CI_lower"," CI_upper","model")


##### exp_out  exp_M
for (m in 1:length(cov_list)){
  covs <- cov_list[[m]]
  
  all_pro_cox(data,covs)
  
  lm_summary$model <- m
  out_summary <- rbind(out_summary,lm_summary)
  
  
  M_cox(data,covs)
  
  M_summary$model <- m
  Media_summary <- rbind(Media_summary,M_summary)
}

fwrite(out_summary,"all_exp_out.csv")
fwrite(Media_summary,"all_exp_M.csv")

fwrite(out_summary,"sen_exp_out.csv")
fwrite(Media_summary,"sen_exp_M.csv")


##### M_out
covs = covs3
formula <- as.formula(paste("PHQ ~", "GESE", "+",paste(covs, collapse = " + ")))
lm_model <- lm(formula, data = data)

summary(lm_model)

single_pro_cox(lm_model)
fwrite(lm_result,"all_M_out.csv")
fwrite(lm_result,"sen_M_out.csv")

lm_model <- lm(formula, data = data)
str(data)
summary(lm_model)

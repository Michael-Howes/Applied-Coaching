library(tidyverse)

# Raw data
father_son_raw <- tibble(
  count = c(50, 45, 8, 18, 8,
            28, 174, 84, 154, 55,
            11, 78, 110, 223, 96,
            14, 150, 185, 714, 447,
            3, 42, 72, 320, 411),
  son_status = rep((1:5),5),
  father_status = unlist(map((1:5), function(x) rep(x,5))),
) 

# Add diagonal variable
# Make variables categorical
father_son <- father_son_raw %>% 
  mutate(diff = son_status - father_status,
         father_fct = factor(father_status),
         son_fct = factor(son_status),
         diff_fct = factor(diff))

# Fit model assuming independence
indep_model <- glm(count ~ father_fct + son_fct, 
                   data = father_son,
                   family = "poisson")

# Get deviance residuals
father_son$indep_resids <- resid(indep_model, "deviance")
indep_residuals <- matrix(father_son$indep_resids,
                           5,
                           5,
                           byrow = T)

# Fit model with "diagonal" affects
full_model <- glm(count ~ father_fct + son_fct + diff_fct,
                  data= father_son,
                  family = "poisson")

# Get deviance residuals
father_son$full_resids <- resid(full_model, "deviance")
full_residuals <- matrix(father_son$full_resids,
                          5,
                          5,
                          byrow = T)
# Compare AICs
AIC(indep_model)
AIC(full_model)

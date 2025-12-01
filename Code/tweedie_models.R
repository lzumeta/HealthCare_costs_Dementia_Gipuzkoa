## tweedie_models.R
## Script for fitting the models (call it from main.R)

library(openxlsx)
library(tidyverse)
library(statmod) ## for tweedie distr.
library(tweedie)


# Model Total healthcare cost ---------------------------------------------
ppredictors <- sum(c(1, nlevels(data_cost_san$edad_cat) - 1,
                     nlevels(data_cost_san$genero) - 1,
                     nlevels(data_cost_san$cci_cat) - 1,
                     nlevels(data_cost_san$nse_tres) - 1,
                     nlevels(data_cost_san$Grupo_poblacion3) - 1)
)
ppredictors_inter <- sum(c(ppredictors,
                           (nlevels(data_cost_san$nse_tres)-1)*
                             (nlevels(data_cost_san$Grupo_poblacion3)-1)
))


## Power parameter estimation (sensitivity analysis)
myformula <- "Coste_sanitario ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3"
(best_par <- best_power_par(myformula, num_predicts = ppredictors)) ## 1.8

## Best model
mod1 <- glm(Coste_sanitario ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3,
            data = data_cost_san,
            family = tweedie(var.power = best_par, link.power = 1),
            start = rep(0.1, ppredictors) # supply stating  values (for convergence issues)
)
summary(mod1)  
plot(mod1)

## save model
saveRDS(mod1, paste0(res_path, "mod1.rds"), compress = TRUE)


## Interaction model
## Power parameter estimation (sensitivity analysis)
myformula_inter <- "Coste_sanitario ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3"
(best_par_inter <- best_power_par(myformula_inter, num_predicts = ppredictors_inter)) ## 1.8

## Best model
mod1_inter <- glm(Coste_sanitario ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3,
                  data = data_cost_san,
                  family = tweedie(var.power = best_par_inter, link.power = 1),
                  start = rep(0.1, ppredictors_inter) 
)
summary(mod1_inter)

## save model
saveRDS(mod1_inter, paste0(res_path, "mod1_inter.rds"), compress = TRUE)

# Model Hospital care cost ------------------------------------------------

## Power parameter estimation (sensitivity analysis)
myformula <- "Cost_Hosp_Total ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3"
(best_par <- best_power_par(myformula, num_predicts = ppredictors)) ## 1.6

## Best model
mod2 <- glm(Cost_Hosp_Total ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3,
            data = data_cost_san,
            family = tweedie(var.power = best_par, link.power = 1),
            start = rep(0.1, ppredictors) # supply stating  values (for convergence issues)
)
summary(mod2)  

## save model
saveRDS(mod2, paste0(res_path, "mod2.rds"), compress = TRUE)



## Interaction model
## Power parameter estimation (sensitivity analysis)
myformula_inter <- "Cost_Hosp_Total ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3"
(best_par_inter <- best_power_par(myformula_inter, num_predicts = ppredictors_inter)) ## 1.6

## Best model
mod2_inter <- glm(Cost_Hosp_Total ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3,
                  data = data_cost_san,
                  family = tweedie(var.power = best_par_inter, link.power = 1),
                  start = rep(0.1, ppredictors_inter) 
)
summary(mod2_inter)

## save model
saveRDS(mod2_inter, paste0(res_path, "mod2_inter.rds"), compress = TRUE)


# Model Primary care cost -------------------------------------------------

## Power parameter estimation (sensitivity analysis)
myformula <- "Coste_AP ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3"
(best_par <- best_power_par(myformula, num_predicts = ppredictors))

## Best model
mod3 <- glm(Coste_AP ~  edad_cat + nse_tres + genero + cci_cat + Grupo_poblacion3,
            data = data_cost_san,
            family = tweedie(var.power = best_par, link.power = 1),
            start = rep(0.1, ppredictors) # supply stating  values (for convergence issues)
)
summary(mod3)  

## save model
saveRDS(mod3, paste0(res_path, "mod3.rds"), compress = TRUE)


## Interaction model
myformula_inter <- "Coste_AP ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3"
(best_par_inter <- best_power_par(myformula_inter, num_predicts = ppredictors_inter)) ## 1.5

## Best model
mod3_inter <- glm(Coste_AP ~  edad_cat + genero + cci_cat + nse_tres*Grupo_poblacion3,
                  data = data_cost_san,
                  family = tweedie(var.power = best_par_inter, link.power = 1),
                  start = rep(0.1, ppredictors_inter) 
)
summary(mod3_inter)

## save model
saveRDS(mod3_inter, paste0(res_path, "mod3_inter.rds"), compress = TRUE)



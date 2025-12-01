library(openxlsx)
library(tidyverse)
library(compareGroups)
library(ggplot2)
library(ggridges)
library(sjPlot)
library(broom)
library(gt)
library(patchwork)
library(knitr)
library(flextable)
library(officer)

theme_set(theme_bw(base_size = 14))

res_path <- "Results/"
fig_path <- "Results/Figures/"
tab_path <- "Results/Tables/"


# Data --------------------------------------------------------------------
data_cost_san <- read.xlsx("path_to_the_dataset(not_available)", sheet = "Datos")
dim(data_cost_san) ## 215859 25
data_cost_san[which(is.na(data_cost_san$Residenciado)), "Residenciado"] <- "Si"  ## impute Residenciado

## Initial setting
table(data_cost_san$Grupo_poblacion3)
data_cost_san <- data_cost_san |> 
  mutate(
    Grupo_poblacion3 = factor(case_when(Grupo_poblacion3 == "No" ~ "Sanos",
                                        Grupo_poblacion3 == "SoloDemencia" ~ "Solo demencia",
                                        Grupo_poblacion3 == "SoloSNP" ~ "Solo SNP",
                                        Grupo_poblacion3 == "DemenciaySNP" ~ "Ambos"),
                              levels = c("Sanos", "Solo demencia", "Solo SNP", "Ambos")),
    genero = factor(genero),
    edad_cat = factor(edad_cat),
    nse_tres = factor(nse_tres, levels = c("Alto", "Medio", "Bajo")),
    cci_cat = factor(cci_cat)
  )


# Recode factors ----------------------------------------------------------
## Only if we want to recode the factors!
# data_cost_san <- data_cost_san |> 
#   mutate(edad_cat = factor(edad_cat,
#                            levels = c("[80-89]", "[60-69]", "[70-79]", ">=90")),
#          cci_cat  = factor(cci_cat,
#                            levels = c("2-4", "0-1", "5-19")))


# Run the models ----------------------------------------------------------
source("05. Analisis/functions.R", echo = TRUE)
source("05. Analisis/tweedie_models.R", echo = TRUE)

# Table 1 -----------------------------------------------------------------
tab1 <- compareGroups(Demencia ~ genero + edad + edad_cat + SNP + cci_cat + nse_tres,
                      data = data_cost_san)
res <- createTable(tab1, show.all = TRUE)
export2md(res, show.n = T,
          caption = "Demographic and clinical characteristics of the study population disaggregated by dementia status. Means (standard deviation) are shown for continuous variables and absolute (relative) frequencies for categorical variables.")
export2word(res, file = normalizePath(paste0("./", tab_path, "table1.docx")))



# Table 2 -----------------------------------------------------------------
tab2 <- compareGroups(Grupo_poblacion3 ~ genero + edad_cat + cci_cat + nse_tres + Residenciado,
                      data = data_cost_san)
res <- createTable(tab2, show.all = TRUE)
export2md(res, show.n = T,
          caption = "Demographic and clinical characteristics of the study population disaggregated by dementia and neuropsychiatric symptom status with absolute (relative) frequencies.")
export2word(res, file = normalizePath(paste0("./", tab_path, "table2.docx")))



# Table 3 -----------------------------------------------------------------
(tab3_top <- data_cost_san |> 
   summarise(mean_sanitario = mean(Coste_sanitario),
             sd_sanitario   = sd(Coste_sanitario),
             mean_hosp      = mean(Cost_Hosp_Total),
             sd_hosp        = sd(Cost_Hosp_Total),
             mean_ap        = mean(Coste_AP),
             sd_ap          = sd(Coste_AP)) |> 
   mutate(var = "total", var2 = "total") |> 
   select(var, var2, everything())
)
vars <- c("genero", "edad_cat", "Demencia", "SNP",
          "Grupo_poblacion3", "cci_cat",
          "nse_tres", "Residenciado")
(tab3_middle <- map_dfr(vars, ~{
  data_cost_san |> 
    group_by(across(all_of(.x))) |> 
    summarise(mean_sanitario = mean(Coste_sanitario),
              sd_sanitario   = sd(Coste_sanitario),
              mean_hosp      = mean(Cost_Hosp_Total),
              sd_hosp        = sd(Cost_Hosp_Total),
              mean_ap        = mean(Coste_AP),
              sd_ap          = sd(Coste_AP)) |> 
    mutate(var = .x)
}) |> 
    relocate(var, .before = 1) |> 
    mutate(genero = ifelse(!is.na(genero), as.character(genero),
                           ifelse(!is.na(edad_cat), as.character(edad_cat),
                                  ifelse(!is.na(Demencia), Demencia,
                                         ifelse(!is.na(SNP), SNP,
                                                ifelse(!is.na(Grupo_poblacion3), as.character(Grupo_poblacion3),
                                                       ifelse(!is.na(cci_cat), as.character(cci_cat),
                                                              ifelse(!is.na(nse_tres), as.character(nse_tres),
                                                                     Residenciado
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
    )
    ) |> 
    select(var, var2 = genero, mean_sanitario, sd_sanitario,
           mean_hosp, sd_hosp, mean_ap, sd_ap)
)

nr <- nrow(data_cost_san)
(tab3_bottom <- data_cost_san |> 
    summarise(n_coste_san0 = sum(Coste_sanitario == 0),
              # n_coste_san0 = paste0(n_coste_san0, " ", round(n_coste_san0/nr*100, 1), "%"),
              pct_coste_san0 = round(n_coste_san0/nr*100, 1),
              n_coste_sanPos = sum(Coste_sanitario > 0),
              # n_coste_sanPos = paste0(n_coste_sanPos, " ", round(n_coste_sanPos/nr*100, 1), "%"),
              pct_coste_sanPos = round(n_coste_sanPos/nr*100, 1),
              n_coste_hosp0 = sum(Cost_Hosp_Total == 0),
              # n_coste_Hosp0 = paste0(n_coste_Hosp0, " ", round(n_coste_Hosp0/nr*100, 1), "%"),
              pct_coste_hosp0 = round(n_coste_hosp0/nr*100, 1),
              n_coste_hospPos = sum(Cost_Hosp_Total > 0),
              # n_coste_HospPos = paste0(n_coste_HospPos, " ", round(n_coste_HospPos/nr*100, 1), "%"),
              pct_coste_hospPos = round(n_coste_hospPos/nr*100, 1),
              n_coste_ap0 = sum(Coste_AP == 0),
              # n_coste_ap0 = paste0(n_coste_ap0, " ", round(n_coste_ap0/nr*100, 1), "%"),
              pct_coste_ap0 = round(n_coste_ap0/nr*100, 1),
              n_coste_apPos = sum(Coste_AP > 0),
              # n_coste_apPos = paste0(n_coste_apPos, " ", round(n_coste_apPos/nr*100, 1), "%"),
              pct_coste_apPos = round(n_coste_apPos/nr*100, 1))
)
(tab3_bottom <- data.frame(var = c("coste", "coste"),
                           var2 = c("coste=0", "coste>0"),
                           mean_sanitario = c(tab3_bottom$n_coste_san0, tab3_bottom$n_coste_sanPos),
                           sd_sanitario   = c(tab3_bottom$pct_coste_san0, tab3_bottom$pct_coste_sanPos),
                           mean_hosp = c(tab3_bottom$n_coste_hosp0, tab3_bottom$n_coste_hospPos),
                           sd_hosp   = c(tab3_bottom$pct_coste_hosp0, tab3_bottom$pct_coste_hospPos),
                           mean_ap = c(tab3_bottom$n_coste_ap0, tab3_bottom$n_coste_apPos),
                           sd_ap   = c(tab3_bottom$pct_coste_ap0, tab3_bottom$pct_coste_apPos)
)
)

rbind(tab3_top,
      tab3_middle,
      tab3_bottom) |> 
  kable(digits = 1,
        caption = "Mean raw costs for total healthcare, hospital care and primary care, broken down by descriptive variable categories, in euros in 2022.",
  ) |> 
  save_as_docx(path = paste0(tab_path, "table3.docx"))

## Shape of the outcome variables
# data_cost_san |>
#   pivot_longer(cols = c("Coste_sanitario", "Cost_Hosp_Total", "Coste_AP"),
#                names_to = "tipo_coste",
#                values_to = "coste") |>
#   ggplot(aes(x = coste)) +
#   geom_histogram() +
#   facet_wrap(~tipo_coste) +
#   scale_x_continuous(labels = scales::comma)
#
# data_cost_san |>
#   pivot_longer(cols = c("Coste_sanitario", "Cost_Hosp_Total", "Coste_AP"),
#                names_to = "tipo_coste",
#                values_to = "coste") |>
#   ggplot(aes(y = coste)) +
#   geom_density(aes(fill = tipo_coste), alpha = 0.2) +
#   scale_y_continuous(labels = scales::comma)
# 
# data_cost_san |>
#   pivot_longer(cols = c("Coste_sanitario", "Cost_Hosp_Total", "Coste_AP"),
#                names_to = "tipo_coste",
#                values_to = "coste") |>
#   ggplot(aes(x = coste, y = tipo_coste)) +
#   geom_density_ridges(aes(fill = tipo_coste)) +
#   theme(legend.position = "none")


# Table 3 version 2 -------------------------------------------------------

(tab3_top <- data_cost_san |> 
   summarise(median_sanitario = round(median(Coste_sanitario)),
             iqr_sanitario    = paste0(round(quantile(Coste_sanitario, probs = 0.25)), " - ", round(quantile(Coste_sanitario, probs = 0.75))),
             median_ap        = round(median(Coste_AP)),
             iqr_ap           = paste0(round(quantile(Coste_AP, probs = 0.25)), " - ", round(quantile(Coste_AP, probs = 0.75))),
             median_hosp      = round(median(Cost_Hosp_Total)),
             iqr_hosp         = paste0(round(quantile(Cost_Hosp_Total, probs = 0.25)), " - ", round(quantile(Cost_Hosp_Total, probs = 0.75)))) |> 
   mutate(var = "total", var2 = "total") |> 
   select(var, var2, everything())
)
vars <- c("genero", "edad_cat", "Demencia", "SNP",
          "Grupo_poblacion3", "cci_cat",
          "nse_tres", "Residenciado")
(tab3_middle <- map_dfr(vars, ~{
  data_cost_san |> 
    group_by(across(all_of(.x))) |> 
    summarise(median_sanitario = round(median(Coste_sanitario)),
              iqr_sanitario    = paste0(round(quantile(Coste_sanitario, probs = 0.25)), " - ", round(quantile(Coste_sanitario, probs = 0.75))),
              median_ap        = round(median(Coste_AP)),
              iqr_ap           = paste0(round(quantile(Coste_AP, probs = 0.25)), " - ", round(quantile(Coste_AP, probs = 0.75))),
              median_hosp      = round(median(Cost_Hosp_Total)),
              iqr_hosp         = paste0(round(quantile(Cost_Hosp_Total, probs = 0.25)), " - ", round(quantile(Cost_Hosp_Total, probs = 0.75)))) |> 
    mutate(var = .x)
}) |> 
    relocate(var, .before = 1) |> 
    mutate(genero = ifelse(!is.na(genero), as.character(genero),
                           ifelse(!is.na(edad_cat), as.character(edad_cat),
                                  ifelse(!is.na(Demencia), Demencia,
                                         ifelse(!is.na(SNP), SNP,
                                                ifelse(!is.na(Grupo_poblacion3), as.character(Grupo_poblacion3),
                                                       ifelse(!is.na(cci_cat), as.character(cci_cat),
                                                              ifelse(!is.na(nse_tres), as.character(nse_tres),
                                                                     Residenciado
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
    )
    ) |> 
    select(var, var2 = genero, median_sanitario, iqr_sanitario,
           median_ap, iqr_ap, median_hosp, iqr_hosp)
)

nr <- nrow(data_cost_san)
(tab3_bottom <- data_cost_san |> 
    summarise(n_coste_san0 = sum(Coste_sanitario == 0),
              # n_coste_san0 = paste0(n_coste_san0, " ", round(n_coste_san0/nr*100, 1), "%"),
              pct_coste_san0 = round(n_coste_san0/nr*100, 1),
              n_coste_sanPos = sum(Coste_sanitario > 0),
              # n_coste_sanPos = paste0(n_coste_sanPos, " ", round(n_coste_sanPos/nr*100, 1), "%"),
              pct_coste_sanPos = round(n_coste_sanPos/nr*100, 1),
              n_coste_hosp0 = sum(Cost_Hosp_Total == 0),
              # n_coste_Hosp0 = paste0(n_coste_Hosp0, " ", round(n_coste_Hosp0/nr*100, 1), "%"),
              pct_coste_hosp0 = round(n_coste_hosp0/nr*100, 1),
              n_coste_hospPos = sum(Cost_Hosp_Total > 0),
              # n_coste_HospPos = paste0(n_coste_HospPos, " ", round(n_coste_HospPos/nr*100, 1), "%"),
              pct_coste_hospPos = round(n_coste_hospPos/nr*100, 1),
              n_coste_ap0 = sum(Coste_AP == 0),
              # n_coste_ap0 = paste0(n_coste_ap0, " ", round(n_coste_ap0/nr*100, 1), "%"),
              pct_coste_ap0 = round(n_coste_ap0/nr*100, 1),
              n_coste_apPos = sum(Coste_AP > 0),
              # n_coste_apPos = paste0(n_coste_apPos, " ", round(n_coste_apPos/nr*100, 1), "%"),
              pct_coste_apPos = round(n_coste_apPos/nr*100, 1))
)
(tab3_bottom <- data.frame(var = c("coste", "coste"),
                           var2 = c("coste=0", "coste>0"),
                           median_sanitario = c(tab3_bottom$n_coste_san0, tab3_bottom$n_coste_sanPos),
                           iqr_sanitario   = c(tab3_bottom$pct_coste_san0, tab3_bottom$pct_coste_sanPos),
                           median_ap = c(tab3_bottom$n_coste_ap0, tab3_bottom$n_coste_apPos),
                           iqr_ap   = c(tab3_bottom$pct_coste_ap0, tab3_bottom$pct_coste_apPos),
                           median_hosp = c(tab3_bottom$n_coste_hosp0, tab3_bottom$n_coste_hospPos),
                           iqr_hosp   = c(tab3_bottom$pct_coste_hosp0, tab3_bottom$pct_coste_hospPos)
)
)

rbind(tab3_top,
      tab3_middle,
      tab3_bottom) |> 
  flextable() |> 
  set_caption(caption = "Median raw costs for total healthcare, hospital care and primary care, broken down by descriptive variable categories, in euros in 2022.",
  ) |> 
  save_as_docx(path = paste0(tab_path, "table3_v2.docx"))

# Three models ------------------------------------------------------------
# Table 4 -----------------------------------------------------------------

# |- Total Health Cost ----------------------------------------------------
mod1 <- readRDS(paste0(res_path, "mod1.rds"))
summary(mod1)

# sjPlot::tab_model(mod1)
mod1_tidy <- tidy(mod1, conf.int = TRUE)
mod1_tab <- mod1_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod1_tab)
# Save
print(doc, target = paste0(tab_path, "tab_total.docx"))


plot_data <- get_model_data(mod1, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p1 <- plot_estimates(plot_data,
                      title_text = "Total health cost",
                      annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                      x_pos = 1 - 0.5, y_pos = 1250, ylimits_sum = 200, breaks_max = 1500) +
  labs(caption = "Tweedie family (var.power = 1.8, link.power = 1)") +
  theme(plot.title = element_text(face = "bold"))
)

ggsave(filename = paste0(fig_path, "fig1.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig1.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)


plot_data <- get_model_data(mod1, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p1_sex <- plot_estimates_sex(plot_data,
                              title_text = "Total health cost",
                              annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-CCI = 0-1",
                              x_pos = 1 - 0.5, y_pos = 1250, ylimits_sum = 200, breaks_max = 1500) + 
    labs(caption = "Tweedie family (var.power = 1.8, link.power = 1)") +
    theme(plot.title = element_text(face = "bold"))
) 
ggsave(filename = paste0(fig_path, "fig1_sex.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig1_sex.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)



# |-- Interaction ---------------------------------------------------------

mod1_inter <- readRDS(paste0(res_path, "mod1_inter.rds"))
summary(mod1_inter)

# tab_model(mod1_inter)
mod1_inter_tidy <- tidy(mod1_inter, conf.int = TRUE)
mod1_inter_tab <- mod1_inter_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod1_inter_tab)
# Save
print(doc, target = paste0(tab_path, "tab_inter_total.docx"))


plot_data <- get_model_data(mod1_inter, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))

plot_estimates(plot_data,
               title_text = "Total health cost",
               annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
               x_pos = 1 - 0.5, y_pos = 250, ylimits_sum = 200, breaks_max = 1500) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig1_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig1_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)

plot_data <- get_model_data(mod1_inter, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
plot_estimates_sex(plot_data,
                   title_text = "Total health cost",
                   annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                   x_pos = 1 - 0.5, y_pos = 250, ylimits_sum = 200, breaks_max = 1500) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig1_sex_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig1_sex_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)



# |- Hospital Cost --------------------------------------------------------
mod2 <- readRDS(paste0(res_path, "mod2.rds"))
summary(mod2)

# tab_model(mod2)
mod2_tidy <- tidy(mod2, conf.int = TRUE)
mod2_tab <- mod2_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod2_tab)
# Save
print(doc, target = paste0(tab_path, "tab_hospital.docx"))


plot_data <- get_model_data(mod2, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p2 <- plot_estimates(plot_data,
                      title_text = "Total hospital cost",
                      annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                      x_pos = 1 - 0.5, y_pos = 900, ylimits_sum = 259.6178) + 
    labs(caption = "Tweedie family (var.power = 1.6, link.power = 1)") +
    theme(plot.title = element_text(face = "bold")))
ggsave(filename = paste0(fig_path, "fig2.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig2.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)


plot_data <- get_model_data(mod2, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p2_sex <- plot_estimates_sex(plot_data,
                              title_text = "Total hospital cost",
                              annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-CCI = 0-1",
                              x_pos = 1 - 0.5, y_pos = 900, ylimits_sum = 259.6178) + 
    labs(caption = "Tweedie family (var.power = 1.6, link.power = 1)") +
    theme(plot.title = element_text(face = "bold"))) 
ggsave(filename = paste0(fig_path, "fig2_sex.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig2_sex.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)


# |-- Interaction ---------------------------------------------------------
mod2_inter <- readRDS(paste0(res_path, "mod2_inter.rds"))
summary(mod2_inter)


# tab_model(mod2_inter)
mod2_inter_tidy <- tidy(mod2_inter, conf.int = TRUE)
mod2_inter_tab <- mod2_inter_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod2_inter_tab)
# Save
print(doc, target = paste0(tab_path, "tab_inter_hospital.docx"))


plot_data <- get_model_data(mod2_inter, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))

plot_estimates(plot_data,
               title_text = "Total hospital cost",
               annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
               x_pos = 1 - 0.5, y_pos = 550, ylimits_sum = 200) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig2_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig2_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)

plot_data <- get_model_data(mod2_inter, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
plot_estimates_sex(plot_data,
                   title_text = "Total hospital cost",
                   annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                   x_pos = 1 - 0.5, y_pos = 550, ylimits_sum = 200) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig2_sex_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig2_sex_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)




# |- Primary Care Cost ----------------------------------------------------
mod3 <- readRDS(paste0(res_path, "mod3.rds"))
summary(mod3)

# tab_model(mod3)
mod3_tidy <- tidy(mod3, conf.int = TRUE)
mod3_tab <- mod3_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod3_tab)
# Save
print(doc, target = paste0(tab_path, "tab_primary.docx"))


plot_data <- get_model_data(mod3, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p3 <- plot_estimates(plot_data,
                      title_text = "Total primary care cost",
                      annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                      x_pos = 1 - 0.5, y_pos = 900, ylimits_sum = 813.9393) +
    labs(caption = "Tweedie family (var.power = 1.5, link.power = 1)") +
    theme(plot.title = element_text(face = "bold")))
ggsave(filename = paste0(fig_path, "fig3.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig3.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)


plot_data <- get_model_data(mod3, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
(p3_sex <- plot_estimates_sex(plot_data,
                              title_text = "Total primary care cost",
                              annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-CCI = 0-1",
                              x_pos = 1 - 0.5, y_pos = 900, ylimits_sum = 803.044) +
    labs(caption = "Tweedie family (var.power = 1.5, link.power = 1)") +
    theme(plot.title = element_text(face = "bold"))
) 
ggsave(filename = paste0(fig_path, "fig3_sex.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig3_sex.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)


# |-- Interaction ---------------------------------------------------------

mod3_inter <- readRDS(paste0(res_path, "mod3_inter.rds"))
summary(mod3_inter)


# tab_model(mod3_inter)
mod3_inter_tidy <- tidy(mod3_inter, conf.int = TRUE)
mod3_inter_tab <- mod3_inter_tidy |> 
  mutate(`p.value` = ifelse(`p.value` < 0.001, "<.001", round(`p.value`, 3)),
         across(where(is.numeric), ~round(.x, 1)),
         ci = paste0("[", conf.low, "-", conf.high, "]")) |> 
  select(Variable = term, Estimate = estimate, `95% CI` = ci, Statistic = statistic, `p-value` = `p.value`) |> 
  flextable()
doc <- read_docx()
doc <- body_add_flextable(doc, value = mod3_inter_tab)
# Save
print(doc, target = paste0(tab_path, "tab_inter_primary.docx"))


plot_data <- get_model_data(mod3_inter, type = "pred", terms = c("Grupo_poblacion3"))
plot_data$group_col <- factor(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"),
                              levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))

plot_estimates(plot_data,
               title_text = "Total primary care cost",
               annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
               x_pos = 1 - 0.5, y_pos = 100, ylimits_sum = 50) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig3_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig3_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)

plot_data <- get_model_data(mod3_inter, type = "pred", terms = c("Grupo_poblacion3", "genero"))
plot_data$x <- factor(rep(c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"), each = 2),
                      levels = c("Healthy", "Only dementia", "Only NPSs", "Dementia\nand NPSs"))
plot_estimates_sex(plot_data,
                   title_text = "Total primary care cost",
                   annotate_text = "Adjusted for:\n-Age = [60-69]\n-SES = High\n-Sex = Male\n-CCI = 0-1",
                   x_pos = 1 - 0.5, y_pos = 100, ylimits_sum = 50) +
  labs(subtitle = "Interaction model")
ggsave(filename = paste0(fig_path, "fig3_sex_inter.pdf"), plot = last_plot(),
       width = 9, height = 5.5, device = "pdf")
ggsave(filename = paste0(fig_path, "fig3_sex_inter.jpg"), plot = last_plot(),
       width = 9, height = 5.5, dpi = 300)



# ALL in ALL --------------------------------------------------------------

p1/(p3 + p2 ) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") & theme(plot.tag = element_text(face = "bold"))
ggsave(paste0(fig_path, "All_costs_by_group.pdf"), device = "pdf", width = 14*0.8, height = 11*0.88)
ggsave(paste0(fig_path, "All_costs_by_group.jpg"), width = 14*0.8, height = 11*0.88)
p1_sex/(p3_sex + p2_sex ) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom",
                                          plot.tag = element_text(face = "bold"))
ggsave(paste0(fig_path, "All_costs_by_group_sex.pdf"), device = "pdf", width = 14*0.8, height = 11*0.88)
ggsave(paste0(fig_path, "All_costs_by_group_sex.jpg"), width = 14*0.8, height = 11*0.88)

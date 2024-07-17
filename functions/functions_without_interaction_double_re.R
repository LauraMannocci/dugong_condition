
#' Function to produce model outputs for GLMM without interaction with double random effects (the discrete variable is turned into a re)
#'
#' @param model GLMM model (R object)
#' @param model_name name of GLMM model (character)
#' @param list_quantitative list quantitative variables in model (list)
#' @param ylim limit on y axis (vector of 2 numbers)
#'
#' @return
#' @export
#'
#' @examples
produce_model_outputs <- function(model, model_name, list_quantitative, ylim){
  
  
  #create model directory  
  dir_name <- here::here("outputs", "7-join_explore_environmental_mes_dat_final_img_without_interaction_double_re", model_name)
  if (dir.exists(dir_name) == TRUE){
    unlink(dir_name, recursive = TRUE, force = TRUE)
  }
  dir.create(dir_name)
  
  
  #get summary
  sink(here::here(dir_name, paste0("summary_", model_name, ".txt")))
  print(summary(model))
  sink(NULL)
  
  
  #get diagnosis
  sink(here::here(dir_name, paste0("diagnosis_", model_name, ".txt")))
  glmmTMB::diagnose(model)
  sink(NULL)
  
  
  
  #get fixed effects
  print("*********get fixed effect***********")
  print(glmmTMB::fixef(model))
  
  #get random effects
  print("*********get random effects***********")
  print(glmmTMB::ranef(model))
  
  #get residuals
  print("*********get residuals***********")
  print(summary(residuals(model)))
  
  
  
  #------------- plot all model coefficients and their significance -------------------
  png(here::here(dir_name, paste0("plot_coef_", model_name, ".png")), width = 900, height = 900)
  print(sjPlot::plot_model(model, sort.est = TRUE, show.values = TRUE, width = 0.5, type = "std"))
  dev.off()
  
  
  
  
  #------------- plot all model coefficients and their significance for significant terms (p-value < 0,05) -------------------
  mod_coef <- sjPlot::plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "std")
  
  #select terms with pvalue greater than a significance cutoff
  mod_coef$data %>%
    dplyr::filter(p.value < 0.05) -> dat
  
  #reconstruct ggplot
  cols <- c("pos" = "blue", "neg" = "red")
  p <- ggplot2::ggplot(data = dat, ggplot2::aes(y = estimate, x = term, color = group, group = group)) +
    ggplot2::scale_y_continuous(limits = c(min(dat$conf.low), max(dat$conf.high))) +
    ggplot2::coord_flip() +
    ggplot2::geom_point() +
    ggplot2::geom_text(label = dat$p.label, nudge_x = 0.3, check_overlap = T) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = conf.low, ymax = conf.high)) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::ylab("Estimates") +
    ggplot2::xlab("") + 
    ggplot2::ggtitle(model_name) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position ='none', axis.text = ggplot2::element_text(size=12))
  
  #save plot 
  ggplot2::ggsave(here::here(dir_name, paste0("plot_coef_significant_", model_name, ".png")), p, width = 7, height = 5)
  
  
  
  
  #------------- plot random effects -------------------
  print("*********plot random effects***********")
  png(here::here(dir_name, paste0("plot_random_effect_", model_name, ".png")), width = 1000, height = 2500)
  print(sjPlot::plot_model(model, show.values = TRUE, type ="re"))
  dev.off()

  
  
  #------------- plot marginal effects per quantitative term (different y scale) -------------------
  print("*********plot marginal effects per quantitative term***********")
  for (i in list_quantitative){
    assign(paste0("plot", which(list_quantitative == i)), sjPlot::plot_model(model, show.values = TRUE, axis.title = c(i, "log(BCI)"),
                                                                             type = "pred", terms = i, title = ""))
  }
  if (length(list_quantitative) == 6) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)
  }
  if (length(list_quantitative) == 7) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)
  }
  
  #to remove grey background
  combined_plot <- cowplot::ggdraw(combined_plot) + 
    ggplot2::theme(plot.background = ggplot2::element_rect(fill="white", color = NA))
  
  png(here::here(dir_name, paste0("plot_marginal_effect_quantitative_", model_name, "_different_yscale.png")), width = 900, height = 900)
  plot(combined_plot)
  dev.off()
  

  
  
  #------------- plot marginal effects per quantitative term (same y scale) -------------------
  print("*********plot marginal effects per quantitative term***********")
  for (i in list_quantitative){
    assign(paste0("plot", which(list_quantitative == i)), sjPlot::plot_model(model, show.values = TRUE, 
                                                                             axis.title = c(i, "log(BCI)"), type = "pred", term = i, title = "") +
             ggplot2::scale_y_continuous(limits = ylim))
  }
  if (length(list_quantitative) == 6) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)
  }
  if (length(list_quantitative) == 7) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)
  }
  
  #to remove grey background
  combined_plot <- cowplot::ggdraw(combined_plot) + 
    ggplot2::theme(plot.background = ggplot2::element_rect(fill="white", color = NA))
  
  png(here::here(dir_name, paste0("plot_marginal_effect_quantitative_", model_name, "_same_yscale.png")), width = 900, height = 900)
  plot(combined_plot)
  dev.off()
  
  
  
  
  
  #------------- plot marginal effects per significant term (different y scale) -------------------
  print("*********plot marginal effects per significant term***********")
  
  #get all model coefficients and their significance for significant terms (p-value < 0,05)
  mod_coef <- sjPlot::plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "std")
  
  #select terms with pvalue greater than a significance cutoff
  mod_coef$data %>%
    dplyr::filter(p.value < 0.05) -> dat
  
  #create vector of clean terms after correcting names
  terms = as.character(dat$term)
  terms_clean = NA
  for (j in 1:length(terms)){
    if(terms[j] == "type_stage_refinedyoung") {terms_clean[j] = "type_stage_refined"}
    if(terms[j] == "genetic_clustersea") {terms_clean[j] = "genetic_cluster"}
    if(terms[j] == "genetic_clusterswio") {terms_clean[j] = "genetic_cluster"}
    if(terms[j] == "countryNew Caledonia") {terms_clean[j] = "country"}
    if(terms[j] == "countryPalau") {terms_clean[j] = "country"}
    if(terms[j] == "countrySeychelles") {terms_clean[j] = "country"}
    if(terms[j] == "countrySri Lanka") {terms_clean[j] = "country"}
    if(terms[j] == "countryThailand") {terms_clean[j] = "country"}
    if(terms[j] == "countryUnited Arab Emirates") {terms_clean[j] = "country"}
    if(terms[j] == "poly(mean_turbidity, 2)1") {terms_clean[j] = "mean_turbidity"}
    if(terms[j] == "poly(mean_turbidity, 2)2") {terms_clean[j] = "mean_turbidity"}
    if(terms[j] == "poly(percent_mpas_partial_no_take, 2)1") {terms_clean[j] = "percent_mpas_partial_no_take"}
    if(terms[j] == "poly(percent_mpas_partial_no_take, 2)2") {terms_clean[j] = "percent_mpas_partial_no_take"}
    if(terms[j] == "poly(percent_mpas_full_no_take, 2)1") {terms_clean[j] = "percent_mpas_full_no_take"}
    if(terms[j] == "poly(percent_mpas_full_no_take, 2)2") {terms_clean[j] = "percent_mpas_full_no_take"}
    if(terms[j] == "poly(mean_gravity, 2)1") {terms_clean[j] = "mean_gravity"}
    if(terms[j] == "poly(mean_gravity, 2)2") {terms_clean[j] = "mean_gravity"}
    if(terms[j] == "poly(gdp_per_capita, 2)1") {terms_clean[j] = "gdp_per_capita"}
    if(terms[j] == "poly(gdp_per_capita, 2)2") {terms_clean[j] = "gdp_per_capita"}
    if(terms[j] == "poly(mean_sst_celsius, 2)1") {terms_clean[j] = "mean_sst_celsius"}
    if(terms[j] == "poly(mean_sst_celsius, 2)2") {terms_clean[j] = "mean_sst_celsius"}
    if(terms[j] == "poly(mean_seagrass_patch_area_km2, 2)1") {terms_clean[j] = "mean_seagrass_patch_area_km2"}
    if(terms[j] == "poly(mean_seagrass_patch_area_km2, 2)2") {terms_clean[j] = "mean_seagrass_patch_area_km2"}
    if(terms[j] == "poly(seagrass_patch_number, 2)1") {terms_clean[j] = "seagrass_patch_number"}
    if(terms[j] == "poly(seagrass_patch_number, 2)2") {terms_clean[j] = "seagrass_patch_number"}
    if(terms[j] == "poly(percent_seagrass, 2)1") {terms_clean[j] = "percent_seagrass"}
    if(terms[j] == "poly(percent_seagrass, 2)2") {terms_clean[j] = "percent_seagrass"}
    if(terms[j] %in% c("mean_turbidity", "percent_mpas_partial_no_take", "percent_mpas_full_no_take", "mean_gravity", 
                       "gdp_per_capita", "mean_sst_celsius", "mean_seagrass_patch_area_km2", "seagrass_patch_number", "percent_seagrass")) {terms_clean[j] = terms[j]}
  }
  
  #remove duplicates
  terms_clean = unique(terms_clean)
  
  for (i in terms_clean){
    assign(paste0("plot", which(terms_clean == i)), sjPlot::plot_model(model, show.values = TRUE, type = "pred", 
                                                                       axis.title = c(i, "log(BCI)"), terms = i, title = ""))
  }
  if (length(terms_clean) == 2) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2)
  }
  if (length(terms_clean) == 3) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3)
  }
  if (length(terms_clean) == 4) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4)
  }
  if (length(terms_clean) == 5) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5)
  }
  if (length(terms_clean) == 6) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)
  }
  if (length(terms_clean) == 7) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)
  }
  if (length(terms_clean) == 8) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)
  }
  if (length(terms_clean) == 9) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9)
  }
  if (length(terms_clean) == 10) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)
  }
  if (length(terms_clean) == 11) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11)
  }
  
  #to remove grey background
  combined_plot <- cowplot::ggdraw(combined_plot) + 
    ggplot2::theme(plot.background = ggplot2::element_rect(fill="white", color = NA))
  
  png(here::here(dir_name, paste0("plot_marginal_effect_significant_", model_name, "_different_yscale.png")), width = 900, height = 900)
  plot(combined_plot)
  dev.off()
  
  
  
  
  #------------- plot marginal effects per significant term (same y scale) -------------------
  print("*********plot marginal effects per significant term***********")
  #get all model coefficients and their significance for significant terms (p-value < 0,05)
  mod_coef <- sjPlot::plot_model(model, sort.est = TRUE, show.values = TRUE, value.offset = .3, type = "std")
  
  #select terms with pvalue greater than a significance cutoff
  mod_coef$data %>%
    dplyr::filter(p.value < 0.05) -> dat
  
  #create vector of clean terms after correcting names
  terms = as.character(dat$term)
  terms_clean = NA
  for (j in 1:length(terms)){
    if(terms[j] == "type_stage_refinedyoung") {terms_clean[j] = "type_stage_refined"}
    if(terms[j] == "genetic_clustersea") {terms_clean[j] = "genetic_cluster"}
    if(terms[j] == "genetic_clusterswio") {terms_clean[j] = "genetic_cluster"}
    if(terms[j] == "countryNew Caledonia") {terms_clean[j] = "country"}
    if(terms[j] == "countryPalau") {terms_clean[j] = "country"}
    if(terms[j] == "countrySeychelles") {terms_clean[j] = "country"}
    if(terms[j] == "countrySri Lanka") {terms_clean[j] = "country"}
    if(terms[j] == "countryThailand") {terms_clean[j] = "country"}
    if(terms[j] == "countryUnited Arab Emirates") {terms_clean[j] = "country"}
    if(terms[j] == "poly(mean_turbidity, 2)1") {terms_clean[j] = "mean_turbidity"}
    if(terms[j] == "poly(mean_turbidity, 2)2") {terms_clean[j] = "mean_turbidity"}
    if(terms[j] == "poly(percent_mpas_partial_no_take, 2)1") {terms_clean[j] = "percent_mpas_partial_no_take"}
    if(terms[j] == "poly(percent_mpas_partial_no_take, 2)2") {terms_clean[j] = "percent_mpas_partial_no_take"}
    if(terms[j] == "poly(percent_mpas_full_no_take, 2)1") {terms_clean[j] = "percent_mpas_full_no_take"}
    if(terms[j] == "poly(percent_mpas_full_no_take, 2)2") {terms_clean[j] = "percent_mpas_full_no_take"}
    if(terms[j] == "poly(mean_gravity, 2)1") {terms_clean[j] = "mean_gravity"}
    if(terms[j] == "poly(mean_gravity, 2)2") {terms_clean[j] = "mean_gravity"}
    if(terms[j] == "poly(gdp_per_capita, 2)1") {terms_clean[j] = "gdp_per_capita"}
    if(terms[j] == "poly(gdp_per_capita, 2)2") {terms_clean[j] = "gdp_per_capita"}
    if(terms[j] == "poly(mean_sst_celsius, 2)1") {terms_clean[j] = "mean_sst_celsius"}
    if(terms[j] == "poly(mean_sst_celsius, 2)2") {terms_clean[j] = "mean_sst_celsius"}
    if(terms[j] == "poly(mean_seagrass_patch_area_km2, 2)1") {terms_clean[j] = "mean_seagrass_patch_area_km2"}
    if(terms[j] == "poly(mean_seagrass_patch_area_km2, 2)2") {terms_clean[j] = "mean_seagrass_patch_area_km2"}
    if(terms[j] == "poly(seagrass_patch_number, 2)1") {terms_clean[j] = "seagrass_patch_number"}
    if(terms[j] == "poly(seagrass_patch_number, 2)2") {terms_clean[j] = "seagrass_patch_number"}
    if(terms[j] == "poly(percent_seagrass, 2)1") {terms_clean[j] = "percent_seagrass"}
    if(terms[j] == "poly(percent_seagrass, 2)2") {terms_clean[j] = "percent_seagrass"}
    if(terms[j] %in% c("mean_turbidity", "percent_mpas_partial_no_take", "percent_mpas_full_no_take", "mean_gravity", 
                       "gdp_per_capita", "mean_sst_celsius", "mean_seagrass_patch_area_km2", "seagrass_patch_number", "percent_seagrass")) {terms_clean[j] = terms[j]}
  }
  
  #remove duplicates
  terms_clean = unique(terms_clean)
  
  for (i in terms_clean){
    assign(paste0("plot", which(terms_clean == i)), sjPlot::plot_model(model, show.values = TRUE, type = "pred", 
                                                                       axis.title = c(i, "log(BCI)"), terms = i, title = "") +
      ggplot2::scale_y_continuous(limits = ylim))
  }
  if (length(terms_clean) == 2) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2)
  }
  if (length(terms_clean) == 3) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3)
  }
  if (length(terms_clean) == 4) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4)
  }
  if (length(terms_clean) == 5) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5)
  }
  if (length(terms_clean) == 6) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)
  }
  if (length(terms_clean) == 7) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7)
  }
  if (length(terms_clean) == 8) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)
  }
  if (length(terms_clean) == 9) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9)
  }
  if (length(terms_clean) == 10) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)
  }
  if (length(terms_clean) == 11) {
    combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11)
  }

  #to remove grey background
  combined_plot <- cowplot::ggdraw(combined_plot) + 
    ggplot2::theme(plot.background = ggplot2::element_rect(fill="white", color = NA))
  
  png(here::here(dir_name, paste0("plot_marginal_effect_significant_", model_name, "_same_yscale.png")), width = 900, height = 900)
  plot(combined_plot)
  dev.off()

  
  
  #------------- model diagnostics -------------------
  print("*********plot model diagnostics***********")
  plot1 <- sjPlot::plot_model(model, show.values = TRUE, type = "diag")[[1]]
  plot2 <- sjPlot::plot_model(model, show.values = TRUE, type = "diag")[[2]]$indiv_id
  plot3 <- sjPlot::plot_model(model, show.values = TRUE, type = "diag")[[3]]
  plot4 <- sjPlot::plot_model(model, show.values = TRUE, type = "diag")[[4]]
  
  png(here::here(dir_name, paste0("plot_diagnostics_", model_name, ".png")), width = 900, height = 900)
  combined_plot <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4)
  dev.off()
  
  
  #------------- diagnostics with DHARMa -------------------
  #The interpretation of conventional residuals for generalized linear (mixed) and other hierarchical statistical models is often problematic.
  #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
  #The ‘DHARMa’ package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted (generalized) linear mixed models.
  #residuals are standardized to values between 0 and 1, and can be interpreted as intuitively as residuals for the linear model. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap, that transforms the residuals to a standardized scale.
  
  #plotQQunif (left panel) creates a qq-plot to detect overall deviations from the expected distribution, by default with added tests for correct distribution (KS test), dispersion and outliers. Note that outliers in DHARMa are values that are by default defined as values outside the simulation envelope, 
  #not in terms of a particular quantile. Thus, which values will appear as outliers will depend on the number of simulations. If you want outliers in terms of a particuar quantile, you can use the outliers() function.
  #To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional default) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line),
  #and provides a p-value for the deviation from the expected quantile. The significance of the deviation to the expected quantiles is tested and displayed visually, and can be additionally extracted with the testQuantiles function.
  #****testUniformity() or KS - tests if the overall distribution conforms to expectations
  #****testOutliers() - tests if there are more simulation outliers than expected (note that which values will appear as outliers will depend on the number of simulations)
  #****testDispersion() - tests if the simulated dispersion is equal to the observed dispersion
  #Advice: Once an residual effect is statistically significant, look at the magnitude to decide if there is a problem. A residual effect is NOT necessarily problematic.  It is crucial to note that significance is NOT a measures of the
  #strength of the residual pattern, it is a measure of the signal/noise ratio, i.e. whether you are sure there is a pattern at all. Significance in hypothesis tests depends on at least 2 ingredients: strength of the signal, and the number
  #of data points. If you have a lot of data points, residual diagnostics will nearly inevitably become significant, because having a perfectly fitting model is very unlikely
  
  #plotResiduals (right panel) produces a plot of the residuals against the predicted value (or alternatively, other variable). Simulation outliers (data points that are outside the range of simulated values) are highlighted as red stars. These points should be carefully interpreted, because we actually 
  #don’t know “how much” these values deviate from the model expectation. Note also that the probability of an outlier depends on the number of simulations, so whether the existence of outliers is a reason for concern depends also on the number of simulations.
  
  #To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional default) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line),
  #and provides a p-value for the deviation from the expected quantile. The significance of the deviation to the expected quantiles is tested and displayed visually, and can be additionally extracted with the testQuantiles function.
  #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
  
  model_diag <- DHARMa::simulateResiduals(model)
  png(here::here(dir_name, paste0("plot_diagnostics_dharma_", model_name, ".png")), width = 900, height = 900)
  plot(model_diag)
  dev.off()
  
  
  #------------ inference with car::anova --> Wald chisquare test -------------
  #these tables use Wald χ2 statistics for comparisons (neither likelihood ratio tests nor F tests)
  #they apply to the fixed effects of the conditional component of the model only (other components might work, but haven’t been tested)
  #as always, if you want to do type 3 tests, you should probably set sum-to-zero contrasts on factors and center numerical covariates (see contrasts argument above)
  #------Type I ANOVA (Sequential): stats::anova(model)
  #This method evaluates the significance of each predictor sequentially. For example, in a model with two predictors A and B, Type I ANOVA first assesses the effect of A, then the effect of B given A. The order of variables in your model is critical here, as it can alter the results, particularly in unbalanced datasets.
  #------Type II ANOVA (Hierarchical): car::Anova(model, type = "II")
  #Type II looks at each main effect in the model, but unlike Type I, it does not depend on the order of terms. It’s particularly useful for models with main effects only, both in balanced and unbalanced datasets. Here, each main effect is tested after accounting for other main effects, but without considering interactions.
  #------Type III ANOVA (Marginal): car::Anova(model, type = "III")
  #This type tests each main effect after considering all other terms, including interactions. Type III is the go-to method when your model includes interactions, particularly in unbalanced datasets. It’s robust against the order of terms in your model.sink(here::here(dir_name, paste0("anova_", model_name, ".txt")))
  #https://schmidtpaul.github.io/dsfair_quarto/ch/summaryarticles/anovatypes.html
  print("------- Anova type II --------")
  sink(here::here(dir_name, paste0("anova_", model_name, ".txt")))
  print(car::Anova(model)) ## default type II
  sink(NULL)
  
  
  
  #------------ VIF : Variance Inflation Factor ---------
  #vif is used to visualize the level of multicollinearity among predictors
  #https://www.r-bloggers.com/2023/12/exploring-variance-inflation-factor-vif-in-r-a-practical-guide/
  #https://easystats.github.io/blog/posts/performance_check_collinearity/
  print("------- VIF --------")
  sink(here::here(dir_name, paste0("VIF_", model_name, ".txt")))
  print(performance::check_collinearity(model))
  sink(NULL)
  #The variance inflation factor is a measure to analyze the magnitude of multicollinearity of model terms. A VIF less than 5 indicates a low correlation of that predictor with other predictors. A value between 5 and 10 indicates a moderate correlation, while VIF values larger than 10 are a sign for high, 
  #not tolerable correlation of model predictors. The Increased SE column in the output indicates how much larger the standard error is due to the correlation with other predictors.

  
  #--------- step AIC - model selection by AIC ------------
  print("------- stepAIC --------")
  sink(here::here(dir_name, paste0("stepAIC_", model_name, ".txt")))
  step <- MASS::stepAIC(model, direction = "both", trace = TRUE) #If the scope argument is missing the default for direction is "backward"
  print(step$anova)
  sink(NULL)
  
}


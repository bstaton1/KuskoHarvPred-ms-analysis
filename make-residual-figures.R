
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# cat("\nMaking Figures: residuals\n")

# is x_var in the best model for the response variable?
var_in_best_model = function(x_var, response) {
  x_var %in% KuskoHarvPred:::find_variables(KuskoHarvPred:::fit_lists[[response]][1])
}

# is x_var in the model set for the response variable?
var_in_model_set = function(x_var, response) {
  x_var %in% KuskoHarvPred:::find_variables(KuskoHarvPred:::fit_lists[[response]])
}

# function to draw a panel label
panel_title = function(x_var) {
  
  # create the generic name
  xname = get_var_name(x_var)
  
  # if a generic name isn't available, create one by hand
  if (is.na(xname)) {
    xname = switch(
      x_var, 
      "year" = "Year",
      "period" = "Period",
      "fitted" = "Fitted Values",
      "fitted_bin" = "Fitted Values (Binned)",
      "qq" = "Quantile vs. Quantile"
    )
  }
  
  # draw the label
  counter <<- counter + 1  # update the panel counter
  mtext(side = 3, line = 0, adj = 0, cex = 0.9, font = 1, text = paste0("(", letters[counter], ") ", xname))
}

# axis functions
default_axis = function() axis(side = 1, las = 2)
day_axis = function() draw_day_axis(min(fit_data$day), max(fit_data$day), by = 5, las = 2)
percent_axis = function() draw_percent_axis(side = 1, las = 2)

# function to create a residual vs. observed value plot when x_var is continuous
resid_scatterplot = function(x_var, response, axis_fun = default_axis) {
  
  # query information
  fit_list = fit_lists[[response]]
  dat = fit_data
  
  # recode period of the season
  # dat$period = make_period_labels()[dat$period]
  
  # simulate from the best model for this response variable
  resids = DHARMa::simulateResiduals(fit_list[[1]])
  dat$resid = resids$scaledResiduals
  
  # get fitted values
  dat$fitted = KuskoHarvPred:::inverse_transform(fit_list[[1]])(fitted(fit_list[[1]]))
  dat$fitted_bin = cut(dat$fitted, breaks = 4, labels = FALSE)
  
  # build the formula
  formula = as.formula(str_replace("resid ~ x_var", "x_var", x_var))
  
  # create the empty plot
  plot(formula, data = dat, ylim = c(0,1), type = "n",
       xlab = "", ylab = "", xaxt = "n")
  
  # draw reference lines
  abline(h = c(0.25, 0.5, 0.75), lty = 2, col = "royalblue", lwd = c(1,2,1))
  
  # draw residuals as points
  points(formula, data = dat, pch = 21,
         cex = 1.2, xpd = TRUE,
         col = scales::alpha("grey20", 0.5),
         bg = scales::alpha("grey20", 0.2))
  
  # add the x-axis
  axis_fun()
  
  # fit a 50% quantile GAM
  qgam_data = data.frame(x = dat[,x_var], y = dat[,"resid"])
  junk = capture.output({
    fit = tryCatch({
      qgam::qgam(y ~ s(x), qu = 0.5, data = qgam_data)
    },
    error = function(e) {
      qgam::qgam(y ~ x, qu = 0.5, data = qgam_data)
    })
  })
  
  # get the predicted curve and approximate 95% CIs
  pred_data = data.frame(x = seq(min(qgam_data$x), max(qgam_data$x), length = 30))
  preds = predict(fit, pred_data, se.fit = TRUE)
  lwrCI = preds$fit - 1.96 * preds$se.fit
  uprCI = preds$fit + 1.96 * preds$se.fit
  
  # draw fitted GAM and uncertainty
  lines(preds$fit ~ pred_data$x, col = "salmon", lwd = 2)
  polygon(x = c(pred_data$x, rev(pred_data$x)),
          y = c(lwrCI, rev(uprCI)), col = scales::alpha("salmon", 0.2), border = NA)
  
  # remove default boundary box
  box(col = "white")
  
  # draw the boundary box denoting whether the variable is in the model or not
  box(
    col = ifelse(var_in_model_set(x_var, response), "black", "grey"),
    lwd = ifelse(var_in_best_model(x_var, response), 2, 1)
  )
  
  # add a title
  panel_title(x_var)
}

# function to create a residual vs. observed value plot when x_var is categorical
resid_boxplot = function(x_var, response) {
  
  # query information
  fit_list = fit_lists[[response]]
  dat = fit_data
  
  # recode period of the season
  # dat$period = make_period_labels()[dat$period]
  
  # simulate from the best model for this response variable
  resids = DHARMa::simulateResiduals(fit_list[[1]])
  dat$resid = resids$scaledResiduals
  
  # get fitted values
  dat$fitted = KuskoHarvPred:::inverse_transform(fit_list[[1]])(fitted(fit_list[[1]]))
  dat$fitted_bin = cut(dat$fitted, breaks = 4, labels = FALSE)
  
  # build the formula
  formula = as.formula(str_replace("resid ~ x_var", "x_var", x_var))
  
  # format logical variables as Y/N
  if (is.logical(dat[,x_var])) {
    dat[,x_var] = ifelse(dat[,x_var], "Yes", "No")
  }
  
  # calculate the boxplot statistics
  # edit the wiskers to be the 95% interval
  bp_out = boxplot(formula, data = dat, plot = FALSE)
  bp_out$stats[1,] = aggregate(formula, data = dat, FUN = quantile, prob = 0.025)$resid
  bp_out$stats[5,] = aggregate(formula, data = dat, FUN = quantile, prob = 0.975)$resid
  
  # draw the boxplot
  bxp(bp_out, xlab = "", ylab = "", las = 2, yaxt = "n", ylim = c(0,1.1),
      boxfill = "grey80", boxcol = "grey30", medcol = "grey30",
      whiskcol = "grey30", whisklty = 1, staplewex = 0, outline = FALSE,
      xpd = TRUE)
  
  # draw y-axis
  usr = par("usr"); xdiff = diff(usr[1:2])
  axis(side = 2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2))
  
  # draw sample sizes
  n = aggregate(formula, data = dat, FUN = length)
  text(x = (1:nrow(n)) - xdiff * 0.035, y = 1.03, labels = prettyNum(n$resid, big.mark = ","), pos = 4, srt = 90, cex = 0.7, col = "grey30")
  
  # draw reference lines
  abline(h = c(0.25, 0.5, 0.75), lty = 2, col = "royalblue", lwd = c(1,2,1))
  
  # remove default boundary box
  box(col = "white")
  
  # draw the boundary box denoting whether the variable is in the model or not
  box(
    col = ifelse(var_in_model_set(x_var, response), "black", "grey"),
    lwd = ifelse(var_in_best_model(x_var, response), 2, 1)
  )
  
  # add a title
  panel_title(x_var)
}

# function to create a residual qqplot
resid_qqplot = function(response) {
  
  # get quantile-standardized residuals using DHARMa package
  fit_list = fit_lists[[response]]
  resid_out = DHARMa::simulateResiduals(fit_list[[1]])
  
  # extract just the QSRs
  resids = resid_out$scaledResiduals
  
  # how many observations?
  n = length(resids)
  
  # calculate theoretical uniform quantiles
  expected = (1:n)/(n + 1)
  
  # simulate 1e5 replicates of 40 values from an actual uniform dist
  # summarize the 95% range of realized quantiles for a theoretical quantile
  sim_resids = replicate(1e3, sort(runif(n)))
  sim_resids_q = apply(sim_resids, 1, function(x) quantile(x, c(0.025, 0.975)))
  
  # sort the observed residuals
  observed = sort(resids)
  
  # plot them against their expected quantiles
  plot(observed ~ expected, type = "n", xlab = "Theoretical", ylab = "Observed")
  
  # add 1:1 line
  abline(0, 1, lwd = 2, lty = 2, col = "royalblue")
  
  # add the envelope of expected QQ curves under assumptions/sample size
  lines(sim_resids_q["2.5%",] ~ expected, type = "l", col = "royalblue", lty = 2)
  lines(sim_resids_q["97.5%",] ~ expected, type = "l", col = "royalblue", lty = 2)
  
  # draw points
  points(observed ~ expected, pch = 21,
         cex = 1.2, xpd = TRUE,
         col = scales::alpha("grey20", 0.5),
         bg = scales::alpha("grey20", 0.2))
  
  # remove default boundary box
  box(col = "white")
  
  # draw the boundary box denoting whether the variable is in the model or not
  box(
    col = ifelse(FALSE, "black", "grey"),
    lwd = ifelse(FALSE, 2, 1)
  )
  
  # add a title
  panel_title("qq")
}

# wrapper to create the plot file for a given response variable
resid_plots = function(response) {
  
  # initialize a panel counter
  counter <<- 0
  
  # build the file name
  file = str_replace("RESPONSE-residuals", "RESPONSE", str_replace(response, "_", "-"))
  
  # open a graphics device
  dev.on(base = file, ext = fig_type, dir = figure_dir, width = 6.5, height = 7)
  
  # graphical parameters
  par(
    mfrow = c(4,3), mgp = c(1,0.15,0), tcl = 0.0,
    oma = c(0,2,2,1),
    mar = c(2,2.5,1.5,0),
    cex.axis = 0.8, lend = "square", ljoin = "mitre"
  )
  
  # residuals vs. each predictor variable (only those in the model set for this response)
  if (var_in_model_set("day", response)) resid_scatterplot("day", response, day_axis)
  if (var_in_model_set("hours_open", response)) resid_scatterplot("hours_open", response)
  if (var_in_model_set("p_before_noon", response)) resid_scatterplot("p_before_noon", response, percent_axis)
  if (var_in_model_set("total_btf_cpue", response)) resid_scatterplot("total_btf_cpue", response)
  if (var_in_model_set("chinook_btf_comp", response)) resid_scatterplot("chinook_btf_comp", response, percent_axis)
  if (var_in_model_set("chum_btf_comp", response)) resid_scatterplot("chum_btf_comp", response, percent_axis)
  if (var_in_model_set("sockeye_btf_comp", response)) resid_scatterplot("sockeye_btf_comp", response, percent_axis)
  if (var_in_model_set("fished_yesterday", response)) resid_boxplot("fished_yesterday", response)
  if (var_in_model_set("weekend", response)) resid_boxplot("weekend", response)
  
  # other variables of interest (draw regardless)
  resid_boxplot("year", response)
  resid_boxplot("period", response)
  resid_scatterplot("fitted", response)
  resid_boxplot("fitted_bin", response)
  resid_qqplot(response)
  
  # y-axis label and plot title
  mtext(side = 2, line = 0.25, text = "Quantile Standardized Residual", outer = TRUE, cex = 1.2)
  mtext(side = 3, line = 0, outer = TRUE, cex = 1.2,
        text = paste0("Response Variable: ", get_var_name(response)), adj = 0, font = 2)
  
  # close the device
  dev.off()
}

# set random seed: for reproducibility of plot
set.seed(12345)

# make all plot files
junk = sapply(names(fit_lists), resid_plots)

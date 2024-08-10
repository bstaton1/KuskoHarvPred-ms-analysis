
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

# decide whether to use darkmode for figures
darkmode = FALSE

# set the default colors for plot bg and text (fg)
plot_bg_col = ifelse(darkmode, "#263238", "white")
plot_text_col = ifelse(darkmode, "#c5c8c6", "black")

# in scatterplots, use these for points
pt_col = ifelse(darkmode, scales::alpha("grey80", 0.7), scales::alpha("grey30", 0.5))
pt_bg = ifelse(darkmode, scales::alpha("grey80", 0.7), scales::alpha("grey30", 0.5))
poly_col = scales::alpha("black", 0.25)
pt_cex = 1.75

# function to help out with displaying superscripts properly in plots
parse_expression = function(x) {
  out = switch(x,
               "Day^2" = expression(Day^2),
               "BTF % Chinook^2" = expression(BTF~"%"~Chinook^2),
               "BTF % Chum^2" = expression(BTF~"%"~Chum^2),
               "BTF % Sockeye^2" = expression(BTF~"%"~Sockeye^2),
               x
  )
  
  # returns warning about using is.na() on an expression
  # safe to ignore
  suppressWarnings({if (is.na(out)) out = " "})
  return(out)
}

##### MAKE FIGURE: effect-sizes #####

# cat("\nMaking Figure: effect-sizes\n")

# function to calculate standardized model averaged effect sizes
standardize_effects = function(fit_list) {
  
  # perform model averaging
  mod_avg = MuMIn::model.avg(fit_list)
  
  # extract model-averaged coefficients
  pt_ests = MuMIn::coefTable(mod_avg, full = TRUE)
  
  # get model-averaged confidence intervals
  ci_ests = MuMIn:::confint.averaging(mod_avg, full = TRUE)
  
  # combine them, the -1 drops the intercept term
  ests = cbind(pt_ests[,-which(colnames(pt_ests) == "df")], ci_ests)[-1,]
  
  # extract the standard error of each coefficient
  se_ests = ests[,"Std. Error"]
  
  # divide all quantities by standard error to standardize
  eff_sizes = ests[,-which(colnames(ests) == "Std. Error")]/se_ests
  
  # improve predictor variable names
  rownames(eff_sizes) = sapply(rownames(eff_sizes), get_var_name)
  
  # sort the estimates
  eff_sizes = data.frame(eff_sizes[order(eff_sizes[,"Estimate"], decreasing = FALSE),])
  
  # add the predictor variable name as a column
  eff_sizes = cbind(predictor = rownames(eff_sizes), eff_sizes); rownames(eff_sizes) = NULL
  eff_sizes
}

# get standardized effect sizes for each response variable
ests = lapply(fit_lists, standardize_effects)

# add the response variable name as a column
# also, add an empty row for spacing in plot
ests = lapply(1:length(ests), function(i) {
  out = cbind(response = names(ests)[i], ests[[i]])
  rbind(data.frame(response = NA, predictor = NA, "Estimate" = NA, "X2.5.." = NA, "X97.5.." = NA), out)
})
ests = do.call(rbind, ests[5:1])

# remove the empty space from the last response variable
ests = ests[-1,]

# set the symbol codes for each response
pch_codes = c(effort = 21, total_cpt = 22, chinook_comp = 23, chum_comp = 24, sockeye_comp = 25)

# set the grey color
grey_col = "grey50"

# open the graphics device
dev.on(base = "effect-sizes", ext = fig_type, dir = figure_dir, width = 3.5, height = 4.5)

# graphical parameters
par(mar = c(2,5.25,0.5,0.5), mgp = c(200, 0.0, 0), tcl = -0.15, xaxs = "i", yaxs = "i",
    lend = "square", ljoin = "mitre", cex.axis = 0.75, bg = plot_bg_col, col.axis = plot_text_col)

# xlimit
xlim = max(abs(ests[,c("X2.5..", "X97.5..")]), na.rm = TRUE) * c(-1,1)

# empty plot with correct dimensions
mp = barplot(ests[,"Estimate"], horiz = TRUE, xlim = xlim, col = plot_bg_col, border = plot_bg_col, xaxt = "n")

# draw 95% CIs and point estimates
segments(ests[,"X2.5.."], mp, ests[,"X97.5.."], mp, col = plot_text_col)
points(x = ests[,"Estimate"], y = mp, pch = pch_codes[ests$response], col = plot_bg_col, bg = plot_text_col, cex = 1.1, lwd = 1.2)

# reference line
abline(v = 0, lty = 1, col = plot_text_col)
axis(side = 1, at = seq(-8, 8, 2), col.ticks = plot_text_col)

# predictor variable labels
par(tcl = 0, mgp = c(200, 0.1, 0))
axis(side = 2, at = mp, labels = sapply(ests$predictor, parse_expression), las = 2)

# response variable separators
abline(h = mp[is.na(ests$response)], lty = 2, col = grey_col)

# xaxis label
mtext(side = 1, line = 1, "Model-Averaged Effect Size", cex = 0.9, col = plot_text_col)

# connect axes
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE, col = plot_text_col)
segments(usr[1], usr[3], usr[1], usr[4], xpd = TRUE, col = plot_text_col)

# draw legend
legend("topleft", 
       title = "Response", pch = pch_codes, cex = 0.7, x.intersp = 0.5,
       col = grey_col, pt.bg = grey_col, text.col = grey_col, title.col = grey_col, bty = "n",
       legend = c("Trips/Day", "Catch/Trip", "% Chinook", "% Chum", "% Sockeye"),
)

# close the device
dev.off()

##### MAKE FIGURE: relationships #####


# cat("\nMaking Figure: relationships\n")

# create a function, similar to KuskoHarvPred::relationship_plot()
# but specific features only for manuscript
# * grey scale
# * three lines displayed to show effect of a continuous variable on predicted relationship
# * labels for these lines

relationship_plot2 = function(response, settings = list(), line_col = "grey50", line_lab = "text", 
                              line_lab_day = 27, line_lab_angle = 0, dat = fit_data, add = FALSE) {
  
  # create the y-axis label
  ylab = get_var_name(response)
  
  # set y-axis limit
  if (str_detect(response, "comp")) {
    ylim = c(0,1)
  } else {
    ylim = c(0, max(ifelse(response == "total_cpt", 100, 700), max(dat[,response])))
  }
  
  # if not adding to existing plot, create a plot with correct dimensions, labels, etc.
  if (!add) {
    plot(dat[,response] ~ day, data = dat, type = "n", ylim = ylim, xaxt = "n", yaxt = "n",
         ylab = ylab, xlab = "Date", axes = FALSE)
  }
  
  # create the plot with two lines: one for fished_yesterday and one for !fished_yesterday
  if ("fished_yesterday" %in% colnames(pred_data[[response]])) {
    
    # extract only the data for this response variable and specific covariate settings
    settings$fished_yesterday = c(TRUE, FALSE)
    sub_pred_data = KuskoHarvPred:::subset_pred_data(response, settings = settings)
    
    # draw the fitted curves
    lines(pred_response ~ day, data = subset(sub_pred_data, !fished_yesterday), col = line_col, lwd = 2)
    lines(pred_response ~ day, data = subset(sub_pred_data, fished_yesterday), lty = 2, col = line_col, lwd = 2)
    
    # draw points
    if (!add) {
      points(dat[,response] ~ day, data = dat, pch = ifelse(fished_yesterday, 24, 21),
             bg = pt_bg, col = plot_bg_col, cex = pt_cex)
    }
    
    # draw line label
    lines(pred_response ~ c(line_lab_day + c(-1,0,1)), data = subset(sub_pred_data, day %in% c(line_lab_day + c(-1,0,1)) & !fished_yesterday), col = plot_bg_col, lwd = 3)
    text(pred_response ~ line_lab_day, data = subset(sub_pred_data, day == line_lab_day & !fished_yesterday), labels = line_lab, col = line_col, srt = line_lab_angle, cex = 0.9)
    
    # add a legend
    if (response == "total_cpt" & !add) {
      legend("topleft", title = "Fished Yesterday", legend = c("No", "Yes"),
             lty = c(1, 2), col = line_col, lwd = 2, bty = "n", text.col = plot_bg_col,
             x.intersp = 1.5, seg.len = 2, cex = 1)
      legend("topleft", title = "Fished Yesterday", legend = c("No", "Yes"),
             pch = c(21, 24), col = pt_col, pt.bg = pt_bg, pt.cex = pt_cex * 1.1, bty = "n",
             x.intersp = 1.5, text.col = par("col.axis"), cex = 1)
    }
  } else {
    
    # extract only the data for this response variable and specific covariate settings
    sub_pred_data = KuskoHarvPred:::subset_pred_data(response, settings = settings)
    
    # draw points
    if (!add) points(dat[,response] ~ day, data = dat, pch = 21, col = plot_bg_col, bg = pt_bg, cex = pt_cex)
    
    # draw the fitted curve
    lines(pred_response ~ day, data = sub_pred_data, col = line_col, lwd = 2)
    
    # draw line label
    lines(pred_response ~ c(line_lab_day + c(-1,0,1)), data = subset(sub_pred_data, day %in% c(line_lab_day + c(-1,0,1))), col = plot_bg_col, lwd = 3)
    text(pred_response ~ line_lab_day, data = subset(sub_pred_data, day == line_lab_day), labels = line_lab, col = line_col, srt = line_lab_angle, cex = 0.9)
  }
  
  # add a panel label
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  letter = c("a", "b", "c", "d", "e")[which(c("effort", "total_cpt", "chinook_comp", "chum_comp", "sockeye_comp") == response)]
  text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, label = paste0("(", letter, ")"), pos = 2, cex = 1.2, col = plot_text_col)
  
  if (!add) {
    # add x-axis
    draw_day_axis(min(dat$day), max(dat$day), by = 5, side = 1, col = plot_text_col)
    
    # add y-axis
    if (str_detect(response, "comp")) {
      draw_percent_axis(side = 2, las = 1, col = plot_text_col)
    } else {
      draw_regular_axis(side = 2, col = plot_text_col, las = 1)
    }
  }
}

# set colors for each of the three lines
avg_col = "grey50"
lwrq_col = "grey80"
uprq_col = ifelse(darkmode, "grey5", "grey20")

# open a graphics device
dev.on(base = "relationships", ext = fig_type, dir = figure_dir, width = 3.5, height = 8)

# graphical parameters
par(mfrow = c(5,1), mar = c(1,4,0.5,0.5), oma = c(1.5,0,0,0), mgp = c(2.5,0.2,0), cex.lab = 1.2, tcl = -0.1, lend = "square", ljoin = "mitre", bg = plot_bg_col, col.axis = plot_text_col, col.lab = plot_text_col)

# panel (a): trips/day
relationship_plot2("effort", line_col = avg_col, line_lab_day = 25, line_lab = "12h", line_lab_angle = 340)
relationship_plot2("effort", add = TRUE, settings = list(hours_open = 6), line_col = lwrq_col, line_lab_day = 20, line_lab = "6h", line_lab_angle = 350)
relationship_plot2("effort", add = TRUE, settings = list(hours_open = 18), line_col = uprq_col, line_lab_day = 30, line_lab = "18h", line_lab_angle = 340)

# panel (b): catch/trip
relationship_plot2("total_cpt", line_col = avg_col, line_lab_day = 30, line_lab = "p50", line_lab_angle = 10)
relationship_plot2("total_cpt", add = TRUE, settings = list(CAT_total_btf_cpue = "q10"), line_col = lwrq_col, line_lab_day = 35, line_lab = "p10", line_lab_angle = 340)
relationship_plot2("total_cpt", add = TRUE, settings = list(CAT_total_btf_cpue = "q90"), line_col = uprq_col, line_lab_day = 26, line_lab = "p90", line_lab_angle = 30)

# panel (c): % chinook
relationship_plot2("chinook_comp", line_col = avg_col, line_lab_day = 19, line_lab = "p50", line_lab_angle = 320)
relationship_plot2("chinook_comp", add = TRUE, settings = list(CAT_chinook_btf_comp = "q10"), line_col = lwrq_col, line_lab_day = 16, line_lab = "p10", line_lab_angle = 335)
relationship_plot2("chinook_comp", add = TRUE, settings = list(CAT_chinook_btf_comp = "q90"), line_col = uprq_col, line_lab_day = 26, line_lab = "p90", line_lab_angle = 320)

# panel (d): % chum
relationship_plot2("chum_comp", line_col = avg_col, line_lab_day = 26, line_lab = "p50", line_lab_angle = 15)
relationship_plot2("chum_comp", add = TRUE, settings = list(CAT_chum_btf_comp = "q10"), line_col = lwrq_col, line_lab_day = 25, line_lab = "p10", line_lab_angle = 5)
relationship_plot2("chum_comp", add = TRUE, settings = list(CAT_chum_btf_comp = "q90"), line_col = uprq_col, line_lab_day = 27, line_lab = "p90", line_lab_angle = 5)

# panel (e): % sockeye
relationship_plot2("sockeye_comp", line_col = avg_col, line_lab_day = 36, line_lab = "p50", line_lab_angle = 5)
relationship_plot2("sockeye_comp", add = TRUE, settings = list(CAT_sockeye_btf_comp = "q10"), line_col = lwrq_col, line_lab_day = 38, line_lab = "p10", line_lab_angle = 0)
relationship_plot2("sockeye_comp", add = TRUE, settings = list(CAT_sockeye_btf_comp = "q90"), line_col = uprq_col, line_lab_day = 34, line_lab = "p90", line_lab_angle = 15)

# x-axis label
mtext(side = 1, outer = TRUE, line = 0.35, "Date", cex = 0.8, col = plot_text_col)

# close the device
dev.off()

##### MAKE FIGURE: obs-v-pred-seasons #####

# cat("\nMaking Figure: obs-v-pred-seasons\n")

# variables used in id'ing records
id_vars = c("year", "day", "period")

# convert harvest to 1000s scale
harv_vars = c("chinook_harv", "chum_harv", "sockeye_harv")
obs_vals = cbind(fit_data[,c(id_vars, names(fit_lists))], fit_data[,harv_vars]/1000)
pred_vals = cbind(fit_data[,id_vars], loo_output$loo_preds[,names(fit_lists)], loo_output$loo_preds[,harv_vars]/1000)

# day axis settings
fday = 12; lday = max(obs_vals$day); by = 10

# function to draw one response variable in one year
# points: openers; lines: LOO predictions; polygons: LOO predictions +/- 1MAPE
# year_label/date_label: logical for whether to include these for this panel

f = function(year_keep, resp, year_label = FALSE, date_label = FALSE) {
  
  # update the number counter (column within row)
  number_counter <<- number_counter + 1
  
  # subset observed and predicted values
  dat_sub = subset(obs_vals, year == year_keep)
  pred_sub = subset(pred_vals, obs_vals$year == year_keep)
  
  # get the MAPE for this response and period, calculate intervals on predictions
  mape = sapply(dat_sub$period, KuskoHarvPred:::get_mape, response = resp)
  upr = pred_sub[,resp] * (1 + mape)
  lwr = pred_sub[,resp] * (1 - mape)
  
  # empty plot w/proper dimensions and labels
  plot(dat_sub[,resp] ~ dat_sub[,"day"], type = "n",
       ylim = c(0, max(obs_vals[,resp], pred_vals[,resp], upr)) * c(1,1.05),
       axes = FALSE, xlab = "", ylab = "", xlim = range(obs_vals$day) + c(-1,0))
  
  # draw information: LOO uncertainty region
  polygon(x = c(dat_sub[,"day"], rev(dat_sub[,"day"])),
          y = c(lwr, rev(upr)), col = "grey80", border = NA)
  
  # draw information: LOO predictions
  lines(pred_sub[,resp] ~ dat_sub[,"day"], lty = 1, lwd = 1.5)
  
  # draw information: actual outcomes
  points(dat_sub[,resp] ~ dat_sub[,"day"], bg = pt_bg, col = "white", pch = 21, cex = pt_cex, xpd = TRUE)
  
  # handle x-axis
  if (date_label) {
    draw_day_axis(side = 1, fday = fday, lday = lday, by = 10, las = 2)
  } else {
    draw_regular_axis(side = 1, at = seq(fday, lday, by = by), labels = FALSE)
  }
  
  # if not the last year, draw a line separating panels
  if (year_keep != max(obs_vals$year)) draw_axis_line(4)
  
  # handle y-axis and y-axis label
  # only perform if this is the first year
  if (year_keep == unique(obs_vals$year)[1]) {
    
    # select axis type and draw
    axis_type = choose_axis_type(resp)
    if (axis_type == "percent") draw_percent_axis(side = 2, las = 1, at = seq(0, 1, 0.2))
    if (axis_type == "regular") {
      if (resp == "effort") {
        draw_regular_axis(side = 2, at = seq(0, 800, 200), las = 1)
      } else {
        draw_regular_axis(side = 2, las = 1)
      }
    }
    
    # create the yaxis label
    yname = get_var_name(resp); yline = 2.5
    if (str_detect(yname, "Harvest")) {
      yname = str_replace(yname, "Harvest", "\nHarvest")
      yline = yline - 1
    }
    mtext(side = 2, line = yline, yname, cex = 1)
  }
  
  # draw the year label if requested
  if (year_label) mtext(side = 3, line = 0.25, cex = 1.1, text = year_keep, font = 2)
  
  # add the panel label
  label = paste0("(", letters[letter_counter], number_counter, ")")
  mtext(side = 3, line = -0.5, adj = 0.95, text = label, cex = 0.65)
}

# open a graphics device
dev.on(base = "obs-v-pred-seasons", ext = fig_type, dir = figure_dir, width = 7.2, height = 7.5)

# set graphics parameters
par(mfrow = c(8,8), mar = c(1,0,0,0), tcl = -0.15, mgp = c(200,0.25,0), oma = c(1,4.5,2,0.25), lend = "square", ljoin = "mitre")

# create the plots for each row
letter_counter = 1; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "effort", year_label = TRUE)
letter_counter = 2; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "total_cpt")
letter_counter = 3; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "chinook_comp")
letter_counter = 4; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "chum_comp")
letter_counter = 5; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "sockeye_comp")
letter_counter = 6; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "chinook_harv")
letter_counter = 7; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "chum_harv")
letter_counter = 8; number_counter <<- 0; junk = sapply(unique(obs_vals$year), f, resp = "sockeye_harv", date_label = TRUE)

# close the device
dev.off()

##### MAKE FIGURE: loo #####


# cat("\nMaking Figure: loo\n")

# function to create one scatter plot
# similar to KuskoHarvPred::vars_biplot()
# but with different aesthetics
f = function(response) {
  
  # extract the info for this response variable
  obs = fit_data[,response]
  pred = loo_output$loo_preds[,response]
  period = fit_data[,"period"]
  
  # set the symbol types for each period
  period_pch = c(21, 22, 24)
  
  # determine variable type
  is_comp = str_detect(response, "comp")
  is_harv = str_detect(response, "harv")
  
  # if harvest variable, divide by 1000
  if (is_harv) {
    obs = obs/1000
    pred = pred/1000
  }
  
  # set axis limits 
  if (is_comp) {
    lim = c(0,1)
  } else {
    lim = range(obs, pred, 0)
  }
  
  # create the panel title text
  counter <<- counter + 1
  title = get_var_name(response)
  title = paste0("(", letters[counter], ") ", title)
  
  # if (is.na(title)) title = paste0(KuskoHarvUtils::capitalize(str_extract(response, "^[:alpha:]+")), " Harvest")
  
  # empty plot with proper dimensions/labeling
  plot(pred ~ obs, xlim = lim, ylim = lim, axes = FALSE, type = "n")
  
  # draw 1:1 line
  abline(0,1, lty = 2)
  
  # draw the points
  points(pred ~ obs, col = plot_bg_col, bg = pt_bg, pch = period_pch[period], cex = 1.7)
  
  # add the title text
  mtext(side = 3, line = 0, adj = 0, text = title, cex = 0.75)
  
  # draw axes
  if (is_comp) {
    par(mgp = c(2,0.0,0))
    draw_percent_axis(side = 1, col = par("fg"), las = 1, gap.axis = -100)
    par(mgp = c(2,0.2,0))
    draw_percent_axis(side = 2, col = par("fg"), las = 1, gap.axis = -100)
  } else {
    par(mgp = c(2,0.0,0))
    draw_regular_axis(side = 1, las = 1, col = par("fg"), gap.axis = -100)
    par(mgp = c(2,0.2,0))
    draw_regular_axis(side = 2, las = 1, col = par("fg"), gap.axis = -100)
  }
  
  # draw legend if this is the effort panel
  if (response == "effort") {
    legend("bottomright", legend = make_period_labels(),
           pch = period_pch, col = plot_bg_col, pt.bg = pt_bg,
           bty = "n", cex = 0.8, pt.cex = 1.6, title = "Period"
    )
  }
}

# initialize the panel counter
counter <<- 0

# open a graphics device
dev.on(base = "loo", ext = fig_type, dir = figure_dir, width = 7.25, height = 3.75)
# png(file.path(figure_dir, "loo.png"), h = 4 * ppi, w = 7.2 * ppi, res = ppi)

# graphical parameters
par(mfrow = c(2,4), mar = c(1,1.5,1.25,0.5), oma = c(1.5,1.75,0,0.25), tcl = -0.1, mgp = c(2,0.2,0), lend = "square", ljoin = "mitre", cex.axis = 0.9,
    bg = plot_bg_col, fg = plot_text_col, col.axis = plot_text_col)

# loop through response variables and harvest, creating the scatter plot for each
junk = sapply(colnames(KuskoHarvPred:::loo_output$loo_preds), f)

# axis labels
mtext(side = 1, outer = TRUE, line = 0.5, "Actual Value", cex = 0.8)
mtext(side = 2, outer = TRUE, line = 0.7, "Predicted Value", cex = 0.8)

# close the device
dev.off()

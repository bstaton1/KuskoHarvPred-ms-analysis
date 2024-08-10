
# set up session
source(file.path(this.path::this.proj(), "session-setup.R"))

##### MAKE FILE: aic.csv #####

# cat("\nMaking Table: aic.csv\n")

# variables in any models
all_vars = c("day", "I(day^2)", "hours_open", "fished_yesterday", "weekend", "p_before_noon", "total_btf_cpue",
             "chinook_btf_comp", "I(chinook_btf_comp^2)",
             "chum_btf_comp", "I(chum_btf_comp^2)",
             "sockeye_btf_comp", "I(sockeye_btf_comp^2)"
)

# function to prepare AIC output into a table
# includes columns for all predictor variables, and a TRUE if in model, FALSE if not
# only for n_keep models
prep_aic = function(response, n_keep) {
  # build the AIC table for this response variable
  tab = KuskoHarvPred:::AIC_table(fit_list = fit_lists[[response]], digits = 6)
  
  # count all fitted models
  n_all = nrow(tab)
  
  # set the number of models to keep
  n_keep = min(n_keep, n_all)
  
  # retain only these models
  tab = tab[1:n_keep,]
  
  # determine which variables could be in each model -- TRUE if was in full model FALSE if not
  could_be = sapply(all_vars, function(v) v %in% KuskoHarvPred:::find_variables(fit_lists[[response]]))
  
  # determine if each variable was in each model
  in_model = lapply(1:nrow(tab), function(m)  {
    
    terms = unlist(stringr::str_split(tab$terms[m], " \\+ "))
    in_mod = all_vars %in% terms
    
    # in_mod = stringr::str_detect(tab$terms[m], stringr::fixed(all_vars))
    ifelse(could_be, in_mod, NA)
  })
  
  # combine into a data frame
  in_model = do.call(rbind, in_model) |> 
    as.data.frame()
  
  # add AIC variables
  in_model$K = tab$K
  in_model$Delta = tab$delta
  in_model$Wt = tab$wt
  
  # format the response variable name
  resp_var = paste0(KuskoHarvUtils::get_var_name(response, escape = TRUE), " (", n_all, ")")
  
  # add response variable as first column
  cbind("resp" = resp_var, in_model)
}

# apply to all response variables, and combine
aic = do.call(rbind, lapply(names(fit_lists), prep_aic, n_keep = 5))

# save aic.csv; to be read in by manuscript source code
write.csv(aic, file.path(table_dir, "aic.csv"), row.names = FALSE)

##### MAKE FILE: loo-corr.csv #####

# cat("\nMaking Table: loo-corr.csv\n")

# function to get the correlations
get_corr = function(response) {
  # extract observed and predicted outcomes for this response variable
  obs = fit_data[,response]
  pred = loo_output$loo_preds[,response]
  
  # extract the period they correspond to
  period = fit_data[,"period"]
  
  # function to calculate and return correlation statistics
  my_corr = function(obs, pred) {out = cor.test(obs,pred); c(rho = unname(out$estimate), p.value = out$p.value, lwr = out$conf.int[1], upr = out$conf.int[2])}
  
  # apply the function to each period separately, and to all records
  out = rbind(t(sapply(unique(period), function(p) my_corr(obs[period == p], pred[period == p]))), my_corr(obs, pred))
  
  # format & return output
  out = data.frame(response = response, period = c(unique(period), "All"), out)
  return(out)
}

# the variables
vars = colnames(loo_output$loo_preds)

# apply to each
corr = lapply(vars, get_corr); corr = do.call(rbind, corr)

# reformat the statistics
rho = dcast(corr, response ~ period, value.var = "rho"); rownames(rho) = rho[,"response"]; rho = rho[vars,-1]
p_vals = dcast(corr, response ~ period, value.var = "p.value"); rownames(p_vals) = p_vals[,"response"]; p_vals = p_vals[vars,-1]
lwr = dcast(corr, response ~ period, value.var = "lwr"); rownames(lwr) = lwr[,"response"]; lwr = lwr[vars,-1]
upr = dcast(corr, response ~ period, value.var = "upr"); rownames(upr) = upr[,"response"]; upr = upr[vars,-1]

# apply bonferonni correction to alpha value
alpha = 0.05
alpha_bonferonni = alpha/prod(dim(rho))

# which correlations are significant accoring to this?
p_sig = p_vals < alpha_bonferonni

# what fraction of period-specific correlations met a criterion?
mean(rho[,1:3] > 0.5); mean(rho[,1:3] > 0.75); mean(p_sig[,1:3])

# format the table for saving
rho = round(rho, 2)                   # round
rho[p_sig] = paste0(rho[p_sig], "*")  # add "*" if signficiant
rho = cbind(resp = rownames(rho), rho)# add variable names
rownames(rho) = NULL                  # drop rownames
colnames(rho)[2:5] = paste0("rho_", colnames(rho)[2:5])

# save the .csv file
write.csv(rho, file.path(table_dir, "loo-corr.csv"), row.names = FALSE)

##### MAKE FILE: loo-errors.csv #####

# cat("\nMaking Table: loo-corr.csv\n")

# names of response variables
vars = loo_output$error_summary$ME$response

# function to get error summaries for a single response variable
# by time period and scale (error vs. percent error)
summarize_errors = function(resp) {
  
  # select which rows and columns to keep
  keep_response = which(vars == resp)
  keep_cols = str_which(colnames(loo_output$error_summary$ME), "_[:digit:]|_all")
  
  # extract the error summaries
  me = unlist(loo_output$error_summary$ME[keep_response,keep_cols])
  mae = unlist(loo_output$error_summary$MAE[keep_response,keep_cols])
  mpe = unlist(loo_output$error_summary$MPE[keep_response,keep_cols])
  mape = unlist(loo_output$error_summary$MAPE[keep_response,keep_cols])
  
  # handle rounding for non-percent scale values
  if (resp == "effort") {me = round(me, 0); mae = round(mae, 0)} 
  if (resp == "total_cpt") {me = round(me, 1); mae = round(mae, 1)} 
  if (stringr::str_detect(resp, "comp")) {me = round(me * 100, 0); mae = round(mae * 100, 0)} 
  if (stringr::str_detect(resp, "harv")) {me = prettyNum(round(me, 0), big.mark = ","); mae = prettyNum(round(mae, 0), big.mark = ",")} 
  
  # convert percent-scale values to percents
  mpe = KuskoHarvUtils::percentize(mpe, escape = TRUE)
  mape = KuskoHarvUtils::percentize(mape, escape = TRUE)
  
  # combine errors and absolute errors into cells of their own
  errors = paste0(me, "\n(", mpe, ")")
  p_errors = paste0(mae, "\n(", mape, ")")
  names(errors) = paste0("err_", c(1:3,"all"))
  names(p_errors) = paste0("perr_", c(1:3,"all"))
  
  # build the output
  cbind(response = resp, as.data.frame(t(c(errors, p_errors))))
  
}

# apply to all response variables and combine
errors = do.call(rbind, lapply(vars, summarize_errors))

# save the output, to be read in by manuscript source code
write.csv(errors, file.path(table_dir, "loo-errors.csv"), row.names = FALSE)

##### MAKE FILE: loo-sensitivity.csv #####

# cat("\nMaking Table: loo-sensitivity.csv\n")

# which variables improve harvest predictions the most if they were themselves perfectly predicted?

vars = c("effort", "total_cpt", "chinook_comp", "chum_comp", "sockeye_comp")
comp_vars = vars[str_which(vars, "_comp$")]

# get the "true" (i.e., observed) values of the response variables
true_vals = fit_data[,vars]

# get the loo predicted values
pred_vals_orig = loo_output$loo_preds[,vars]

# build three new sets of predicted value
# each one has the true values swapped in
pred_vals_perfect_effort = pred_vals_orig; pred_vals_perfect_effort$effort = true_vals$effort
pred_vals_perfect_total_cpt = pred_vals_orig; pred_vals_perfect_total_cpt$total_cpt = true_vals$total_cpt
pred_vals_perfect_comp = pred_vals_orig; pred_vals_perfect_comp[,comp_vars] = true_vals[,comp_vars]

# function to predict harvest given a set of predicted response variables
pred_harv = function(pred_vals) {
  data.frame(
    chinook_harv = with(pred_vals, effort * total_cpt * chinook_comp),
    chum_harv = with(pred_vals, effort * total_cpt * chum_comp),
    sockeye_harv = with(pred_vals, effort * total_cpt * sockeye_comp)
  )
}

# apply this function to each data set
harv_orig = pred_harv(pred_vals_orig)
harv_perfect_effort = pred_harv(pred_vals_perfect_effort)
harv_perfect_total_cpt = pred_harv(pred_vals_perfect_total_cpt)
harv_perfect_comp = pred_harv(pred_vals_perfect_comp)

# extract the "true" (i.e., observed) harvest values
true_harv = fit_data[,c("chinook_harv", "chum_harv", "sockeye_harv")]

# function to calculate/summarize errors in harvest predictions
summarize_harv_errors = function(harv_pred, keep) {
  # calculate error
  errors = harv_pred[keep,] - true_harv[keep,]
  
  # calculate proportional error
  p_errors = errors/true_harv[keep,]
  
  # summarize median error, median abs. error, median prop. error, and median abs. prop. error
  list(
    ME = apply(errors, 2, median),
    MAE = apply(abs(errors), 2, median),
    MPE = apply(p_errors, 2, median),
    MAPE = apply(abs(p_errors), 2, median)
  )
}

# function to summarize all data sets for a given time period and return one of the error summary stats
summarize_all_harv_errors = function(keep_period, summary_var = "MAPE") {
  
  # if supplied "All" as the period to keep, set it to keep all
  if (keep_period == "All") keep_period = 1:3
  keep = fit_data[,"period"] %in% keep_period
  
  # summarize errors and combine across data sets for this period
  x = rbind(
    "Real_Predictions" = summarize_harv_errors(harv_orig, keep)[[summary_var]],
    "Perfect_Trips/Day" = summarize_harv_errors(harv_perfect_effort, keep)[[summary_var]],
    "Perfect_Catch/Trip" = summarize_harv_errors(harv_perfect_total_cpt, keep)[[summary_var]],
    "Perfect_Species_Comp." = summarize_harv_errors(harv_perfect_comp, keep)[[summary_var]]
  )
  
  # convert to percentages for printing if warranted
  if (summary_var %in% c("MPE", "MAPE")) x = KuskoHarvUtils::percentize(x, escape = TRUE)
  
  # format as a data frame with the relevant info
  x = data.frame(scenario = rownames(x), period = ifelse(length(keep_period) == 1, keep_period, "All"), x)
  
  # reset rownames
  rownames(x) = NULL
  
  # return the table
  return(x)
}

# apply to each and all periods separately, only extract MAPE
tab = rbind(
  summarize_all_harv_errors(1, "MAPE"),
  summarize_all_harv_errors(2, "MAPE"),
  summarize_all_harv_errors(3, "MAPE"),
  summarize_all_harv_errors("All", "MAPE")
)
colnames(tab) = str_remove(colnames(tab), "_harv")

# assign period factor levels for proper column ordering after reshaping
tab$period = factor(tab$period, levels = c("1", "2", "3", "All"))

# reshape for proper table printing
tab = melt(tab, id.vars = c("scenario", "period"), variable.name = "species") |> 
  dcast(species + scenario ~ period)

# improve text to be printed
tab$scenario = str_replace_all(tab$scenario, "_", " ")
tab$species = str_to_title(tab$species)

# set order of the species & scenarios to be printed in the table rows
tab$species = factor(tab$species, levels = c("Chinook", "Chum", "Sockeye"))
tab$scenario = factor(tab$scenario, levels = c("Real Predictions", "Perfect Trips/Day", "Perfect Catch/Trip", "Perfect Species Comp."))

# sort the table and reset row names
tab = tab[order(tab$species, tab$scenario),]
rownames(tab) = NULL

# save the table
write.csv(tab, file.path(table_dir, "loo-sensitivity.csv"), row.names = FALSE)

##### MAKE FILE: data-and-predictions.csv #####

# cat("\nMaking Table: data-and-predictions.csv\n")

# extract the date information for each opener
dates = fit_data[,c("year", "date", "period")]

# get the month (character) for each opener
months = lubridate::month(dates$date, label = TRUE, abbr = TRUE) |> as.character()

# get the numeric day of the month
days = lubridate::day(dates$date)

# build a "nice date"
dates$date_nice = paste0(days, " ", months)

# add a "total" placeholder to the bottom of each year & combine
dates_split = split(dates, dates$year)
dates_split = lapply(dates_split, function(y) rbind(y, "Total" = data.frame(year = unique(y$year), date = NA, date_nice = "Total", period = NA)))
dates_w_total = do.call(rbind, dates_split)
rownames(dates_w_total) = NULL

# extract the predicted and real outcomes
dat_pred = loo_output$loo_preds
dat_real = fit_data[,colnames(dat_pred)]

# function to extract and properly format the output from one table
f = function(df) {
  
  # handle rounding/summing
  df$effort = round(df$effort, 0)
  df$total_cpt = round(df$total_cpt, 1)
  df$total_harv = df$chinook_harv + df$chum_harv + df$sockeye_harv
  
  # calculate the total by year
  out = split(df, dates$year) |> 
    lapply(function(y) rbind(y, "Total" = colSums(y)))
  out = do.call(rbind, out)
  
  # format the composition variables
  comps = c("chinook_comp", "chum_comp", "sockeye_comp")
  out[,comps] = apply(out[,comps], 1, function(x) KuskoHarvUtils:::smart_round(x, 2) |> KuskoHarvUtils::percentize(escape = TRUE)) |> t() |> as.data.frame()
  
  # append these to the date info
  out = cbind(dates_w_total, out)
  
  # force the total comp variables to be NA
  out[is.na(out$date),comps] = NA
  
  # drop the standard "date" column
  out = out[,-which(colnames(out) == "date")]
  
  # format numbers
  out$effort = prettyNum(out$effort, big.mark = ",", scientific = FALSE)
  out$chinook_harv = prettyNum(round(out$chinook_harv), big.mark = ",", scientific = FALSE)
  out$chum_harv = prettyNum(round(out$chum_harv), big.mark = ",", scientific = FALSE)
  out$sockeye_harv = prettyNum(round(out$sockeye_harv), big.mark = ",", scientific = FALSE)
  out$total_harv = prettyNum(round(out$total_harv), big.mark = ",", scientific = FALSE)
  
  # return output
  rownames(out) = NULL
  return(out)
}

# format the predicted and real outcomes
real = f(dat_real)
pred = f(dat_pred)

# function to combine the real values with predicted values cell by cell
combine = function(real, pred) {
  paste0(real, " (", pred, ")") #|> 
  # kableExtra::linebreak(align = "c")
}

# placeholder object
tab = real

# format the columns for each variable
tab$effort = combine(real$effort, pred$effort)
tab$total_cpt = combine(real$total_cpt, pred$total_cpt)
tab$chinook_comp = combine(real$chinook_comp, pred$chinook_comp)
tab$chum_comp = combine(real$chum_comp, pred$chum_comp)
tab$sockeye_comp = combine(real$sockeye_comp, pred$sockeye_comp)
tab$chinook_harv = combine(real$chinook_harv, pred$chinook_harv)
tab$chum_harv = combine(real$chum_harv, pred$chum_harv)
tab$sockeye_harv = combine(real$sockeye_harv, pred$sockeye_harv)
tab$total_harv = combine(real$total_harv, pred$total_harv)

# remove unneeded columns
tab = tab[,-which(colnames(tab) == "period")]
tab = tab[,-which(colnames(tab) == "total_harv")]

# save the output
write.csv(tab, file.path(table_dir, "data-and-predictions.csv"), row.names = FALSE)

##### MAKE FILE: predictors.csv #####

# cat("\nMaking Table: predictors.csv\n")

# extract values of the predictor variables
tab = fit_data[,c("year", "date", "day", "hours_open", "p_before_noon", "fished_yesterday", "weekend", "total_btf_cpue", "chinook_btf_comp", "chum_btf_comp", "sockeye_btf_comp")]

# create a "nice" date
months = lubridate::month(tab$date, label = TRUE, abbr = TRUE) |> as.character()
days = lubridate::day(tab$date)
tab$date_nice = paste0(days, " ", months)

# format the composition variables
comps = c("chinook_btf_comp", "chum_btf_comp", "sockeye_btf_comp")
tab[,comps] = apply(tab[,comps], 1, function(x) smart_round(x, 2) |> percentize(escape = TRUE)) |> t() |> as.data.frame()

# format all other variables
tab$fished_yesterday = ifelse(tab$fished_yesterday, "Yes", "No")
tab$weekend = ifelse(tab$weekend, "Yes", "No")
tab$p_before_noon = percentize(tab$p_before_noon, escape = TRUE)
tab$total_btf_cpue = round(tab$total_btf_cpue, 1)

# re-order the variables
tab = tab[,c("year", "date_nice", "day", "hours_open", "p_before_noon", "fished_yesterday", "weekend", "total_btf_cpue", "chinook_btf_comp", "chum_btf_comp", "sockeye_btf_comp")]

# save the output
write.csv(tab, file.path(table_dir, "predictors.csv"), row.names = FALSE)


# load in packages
library(knitr)
library(kableExtra)
library(KuskoHarvUtils)
library(KuskoHarvPred)
library(texPreview)
library(stringr)

tex_opts$set(
  usrPackages = build_usepackage(pkg = c("threeparttable", "amssymb", "makecell")),
  fileDir = "tables-with-sonar",  imgFormat = "png", cleanup = c(".aux", ".log", ".pdf", ".tex"),
  density = 300, returnType = "tex")

options(knitr.kable.NA = " ")

# standardized kable function
ms_kbl = function(x, latex = TRUE, non_latex_format = "pandoc",
                  full_width = FALSE, booktabs = TRUE, longtable = FALSE,
                  latex_options = NULL, bold_headers = TRUE, bold_first_column = TRUE,
                  allow_float = FALSE, font_size = NULL, ...) {
  
  # set the format
  if (latex) format = "latex" else format = non_latex_format
  
  # if using longtable, also use the "repeat_header" latex_option
  if (longtable) latex_options = c(latex_options, "repeat_header")
  
  # if not allowing float, also use the "HOLD_position" latex_option
  if (!allow_float) latex_options = c(latex_options, "HOLD_position")
  
  # build the initial kable
  out = kableExtra::kbl(
    x = x, format = ifelse(latex, "latex", non_latex_format), linesep = "",
    longtable = longtable, booktabs = TRUE, escape = FALSE, ...
  )
  
  # if this is for latex output, pass through remainder of chain
  if (latex) {
    out = out|> 
      kableExtra::kable_styling(full_width = full_width, latex_options = latex_options, font_size = font_size)
    
    if (bold_headers) out = kableExtra::row_spec(out, 0, bold = TRUE)
    if (bold_first_column) out = kableExtra::column_spec(out, 1, bold = TRUE)
  } 
  
  return(out)
} 


##### TABLE 1: PRED VARS TABLE #####

# load the source code for the table cells
tab = read.csv("tables-with-sonar/pred-vars-source-revised.csv", allowEscapes = TRUE)

# build a lookup key for the group column
group_key = c(
  "timing" = "Fishery Timing",
  "sonar" = "Run Indices\\textsuperscript{a}",
  "weather" = "Weather\\textsuperscript{b}",
  "water" = "Water\\textsuperscript{c}"
)

# replace any instance of ^2 with the TeX version
tab$variable = stringr::str_replace(tab$variable, "\\^2", "\\\\textsuperscript{2}")

# apply the lookup key
tab$group = group_key[as.character(tab$group)]

# change any instances of in/out to the appropriate symbol
tab[tab == "in"] = "$\\blacksquare$"
tab[tab == "out"] = "$\\square$"

# function to append the specified sign to a cell
add_sign = function(x, sign) {
  
  # apply a switch to each element
  sign = sapply(sign, function(i) {
    switch(i, "pos" = " (+)", "neg" = " (-)", "ques" = " (?)", NA)
  })
  
  # combine
  out = paste0(x, sign)
  
  # force NA cells
  out[out == "NANA"] = NA
  
  # return
  out
}

# format the data frame to supply to the kable pipeline
tab = data.frame(
  group = tab$group, variable = tab$variable, 
  trips = add_sign(tab$trips, tab$trips_sign),
  catch_rate = add_sign(tab$catch_rate, tab$catch_rate_sign),
  p_chinook = add_sign(tab$p_chinook, tab$p_chinook_sign),
  p_chum = add_sign(tab$p_chum, tab$p_chum_sign),
  p_sockeye = add_sign(tab$p_sockeye, tab$p_sockeye_sign)
)

# get the response variable names
vars = sapply(c("effort", "total_cpt", "chinook_comp", "chum_comp", "sockeye_comp"), KuskoHarvUtils::get_var_name, escape = TRUE)

# build the basic kable
kbl_out =  ms_kbl(tab, row.names = FALSE, caption = "",
                  col.names = c(" ", "Predictor Variable", vars))

kbl_out |> 
  column_spec(1, width = "45px") |>
  add_header_above(c(" " = 4, "Harvest Species Composition" = 3), bold = TRUE) |> 
  add_header_above(c(" " = 2, "Response Variables" = 5), bold = TRUE) |> 
  collapse_rows(1, valign = "bottom", latex_hline = "major") |> 
  footnote(
    alphabet = c(
      "Sonar project -- a daily fishery-independent index of in-river salmon abundance and species composition.",
      "Measured at the Bethel Airport. All variables represent daily averages, except precipitation (daily total) and gust wind speed (daily maximum).",
      "Historically measured daily during Bethel Test Fishery sampling."
    )#, threeparttable = TRUE, fixed_small_size = TRUE
  ) |> tex_preview(stem = "pred-vars")

file.show("tables-with-sonar/pred-vars.png")

##### TABLE 2: aic #####

# load the pre-processed AIC table
aic = read.csv("tables-with-sonar/aic.csv")

# handle column names (those with I(^2) get distorted)
colnames(aic) = colnames(aic) |>
  str_replace("\\.", "(") |> 
  str_replace("\\.", "^") |> 
  str_replace("\\.", ")") 

# variables in any models
all_vars = colnames(aic)[!colnames(aic) %in% c("resp", "K", "Delta", "Wt")]

# insert filled square anywhere the variable was in the model
tmp = as.matrix(aic[,all_vars])
tmp[tmp] = "$\\blacksquare$"
tmp[tmp == "FALSE"] = "$\\square$"
tmp = as.data.frame(tmp)
aic[,all_vars] = tmp

# improve variable names
colnames(aic)[1] = " "
colnames(aic)[colnames(aic) %in% all_vars] = sapply(all_vars, get_var_name, escape = TRUE)
colnames(aic)[colnames(aic) == "K"] = "\\textbf{K}"
colnames(aic)[colnames(aic) == "Delta"] = "$\\boldsymbol{\\Delta}$\\textbf{AIC\\textsubscript{c}}"
colnames(aic)[colnames(aic) == "Wt"] = "\\textbf{Wt.}"

# rotate column names
colnames(aic)[2:(length(all_vars)+1)] = cell_spec(colnames(aic)[2:(length(all_vars)+1)], "latex", angle = 90, bold = TRUE, escape = FALSE)

# build the basic kable
kbl_out = ms_kbl(aic, digits = 2, caption = "")

kbl_out = kbl_out |> 
  collapse_rows(1, latex_hline = "major", valign = "bottom") |> 
  tex_preview(stem = "aic")

file.show("tables-with-sonar/aic.png")

##### TABLE 3: loo-corr #####

# load the pre-calculated correlation table
rho = read.csv("tables-with-sonar/loo-corr.csv")

# determine which are significant (stored with a * in the cells that are sig)
rho_sig = cbind(resp = rho$resp, as.data.frame(apply(rho[,2:5], 2, function(x) str_detect(x, "\\*"))))

# convert back to pure numbers
rho = cbind(resp = rho$resp, as.data.frame(apply(rho[,2:5], 2, function(x) as.numeric(str_remove(x, "\\*")))))

# make the cells bold that are significant
tab = sapply(2:ncol(rho), function(i) kableExtra::cell_spec(rho[,i], "latex", bold = rho_sig[,i], italic = rho_sig[,i]))

# make colum labels (period)
colnames(tab) = c(make_period_labels(), "All")

# make row labels (variable)
rownames(tab) = unname(sapply(rho$resp, get_var_name, escape = TRUE))

# build the basic kable
kbl_out = ms_kbl(tab, caption = " ", align = "cccc")

# add details if latex
kbl_out |> 
  add_header_above(c(" " = 1, "Period in Season" = 4), bold = TRUE) |> 
  tex_preview(stem = "loo-corr")
file.show("tables-with-sonar/loo-corr.png")

##### TABLE 4: loo-errors #####

# load the pre-processed error summary output table
errors = read.csv("tables-with-sonar/loo-errors.csv")

# insert latex line breaks
errors[,2:ncol(errors)] = sapply(2:ncol(errors), function(i) kableExtra::linebreak(errors[,i], align = "c"))

# make nice variable names
errors$response = sapply(errors$response, get_var_name, escape = TRUE)

# make period lables
period_labels = c(make_period_labels(), "All")

# build the basic kable
kbl_out = ms_kbl(errors, col.names = c("Variable", rep(period_labels, 2)),
                 caption = NULL)

# add details if latex
kbl_out |> 
  add_header_above(c(" " = 1, "Median Error (MPE)" = 4, "Median Absolute Error (MAPE)" = 4), bold = TRUE) |>
  collapse_rows(1, latex_hline = "major", valign = "bottom") |> 
  tex_preview(stem = "loo-errors")
file.show("tables-with-sonar/loo-errors.png")

##### TABLE 5: loo-sensitivity #####

# load the pre-processed sensitivity analysis output
tab = read.csv("tables-with-sonar/loo-sensitivity.csv")

# build the basic kable
kbl_out = ms_kbl(
  tab, align = "llcccc",
  col.names = c("Species", "Scenario", make_period_labels(), "All"),
  caption = NULL
)

# add details if latex
kbl_out |> 
    add_header_above(c(" " = 2, "Period in Season" = 4), bold = TRUE) |>
    collapse_rows(1, valign = "bottom", latex_hline = "major") |>
    column_spec(2, bold = TRUE) |> 
  tex_preview(stem = "loo-sensitivity")
file.show("tables-with-sonar/loo-sensitivity.png")

##### TABLE A1: data-and-predictions #####

# load the pre-processed data table
tab = read.csv("tables-with-sonar/data-and-predictions.csv")

# rotate the year 90 degrees
colnames(tab) = c(" ", "Date", "Trips", "Catch/Trip", "Chinook", "Chum", "Sockeye", "Chinook", "Chum", "Sockeye")
tab[,1] = cell_spec(tab[,1], angle = 90, bold = TRUE, format = "latex")
tab[tab == "NA (NA)"] = " "

# knitr::kable(tab, format = ifelse(is_latex(), "latex", "pandoc"), caption = captions$tables$`data-and-predictions`, escape = FALSE, booktabs = TRUE, longtable = TRUE, linesep = "", align = "r") |>
#   kableExtra::kable_styling(full_width = FALSE, latex_options = c("HOLD_position", "repeat_header")) |>
#   collapse_rows(1, latex_hline = "major") |>
#   row_spec(0, bold = TRUE) |>
#   row_spec(which(tab$Date == "Total"), bold = TRUE) |>
#   add_header_above(c(" " = 4, "\\\\% Composition" = 3, " " = 3), bold = TRUE, escape = FALSE) |>
#   add_header_above(c(" " = 2, "Regression Response Variables (LOO Predictions)" = 5, "Harvest (LOO Predictions)" = 3), bold = TRUE, escape = FALSE)# |>

# build the basic kable
kbl_out = ms_kbl(tab, caption = "", longtable = FALSE, align = "r", font_size = 11)

kbl_out |> 
  collapse_rows(1, latex_hline = "major") |>
  row_spec(which(tab$Date == "Total"), bold = TRUE) |>
  add_header_above(c(" " = 4, "\\\\% Composition" = 3, " " = 3), bold = TRUE, escape = FALSE) |>
  add_header_above(c(" " = 2, "Regression Response Variables (LOO Predictions)" = 5, "Harvest (LOO Predictions)" = 3), bold = TRUE) |> 
  tex_preview(stem = "data-and-predictions")
file.show("tables-with-sonar/data-and-predictions.png")

##### TABLE A2: predictors #####

# load in the pre-processed output
tab = read.csv("tables-with-sonar/predictors.csv")

# format the column names
colnames(tab) = c(" ", "Date", "Day", "Hours Open", "\\% Before Noon", "Weekend", "Total Count", "Chinook", "Chum", "Sockeye")

# rotate the year 90 degrees
tab[,1] = cell_spec(tab[,1], angle = 90, bold = TRUE, format = "latex")

# format total sonar count variable
tab[,"Total Count"] = prettyNum(tab[,"Total Count"], big.mark = ",")

# build the basic kable
kbl_out = ms_kbl(tab, caption = NULL, longtable = FALSE, align = "r")

kbl_out |> 
  collapse_rows(1, latex_hline = "major") |>
  add_header_above(c(" " = 7, "\\\\% Composition" = 3), bold = TRUE, escape = FALSE) |>
  add_header_above(c(" " = 1, "Fishery Timing Variables" = 5, "Sonar Indices" = 4), bold = TRUE) |> 
  tex_preview(stem = "predictors")
file.show("tables-with-sonar/predictors.png")


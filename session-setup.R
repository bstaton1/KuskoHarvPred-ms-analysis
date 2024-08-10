
# set the figure type to create if doesn't already exist
if (!exists("fig_type")) fig_type = "png"

# function to flexibly create a plot file
dev.on = function(base = "fig", ext = "png", dir = ".", width, height) {
  # ensure file extension is lower case
  ext = tolower(ext)
  
  # build path
  path = file.path(dir, paste0(base, ".", ext))
  
  # stop if unaccepted ext type
  if (!ext %in% c("pdf", "png")) stop ("ext must be one of 'pdf' or 'png'")
  
  # open the appropriate device
  if (ext == "pdf") pdf(path, width = width, height = height)
  if (ext == "png") png(path, width = width * 600, height = height * 600, res = 600)
}

# load frequently used packages
library(KuskoHarvUtils)
library(stringr)
library(reshape2)

# location of the project: allows scripts to be sourced from anywhere
proj_dir = this.path::this.proj()

# location of data files for producing post-season estimates
data_dir = file.path(proj_dir, "validation/data")

# location of figure file output
figure_dir = file.path(proj_dir, "figures")

# location of table file output
table_dir = file.path(proj_dir, "tables")

# create figure and table output directories if non-existent
if (!dir.exists(figure_dir)) dir.create(figure_dir)
if (!dir.exists(table_dir)) dir.create(table_dir)

# resolution for figures
ppi = 600

# Load data sets
data(openers_all, package = "KuskoHarvData")
data(harv_est_all, package = "KuskoHarvData")
data(effort_est_all, package = "KuskoHarvData")
data(interview_data_all, package = "KuskoHarvData")
data(flight_data_all, package = "KuskoHarvData")

# which dates will be reported: those not in August and that had non-zero flights
include_dates = lubridate::date(openers_all$start)[lubridate::month(openers_all$start) != 8 & openers_all$flights_flown > 0]

# filter out the non-reporting dates from each info set
H_ests = subset(harv_est_all, date %in% include_dates)
E_ests = subset(effort_est_all, date %in% include_dates)
I_data = subset(interview_data_all, lubridate::date(trip_start) %in% include_dates)
F_data = subset(flight_data_all, lubridate::date(start_time) %in% include_dates)
M_info = subset(openers_all, lubridate::date(start) %in% include_dates)

# attach KuskoHarvPred objects
fit_data = KuskoHarvPred:::fit_data       # data set for regression modeling, output of KuskoHarvData::prepare_regression_data()
fit_lists = KuskoHarvPred:::fit_lists     # all fitted regression models
pred_data = KuskoHarvPred:::pred_data     # model-averaged predictions at pre-defined covariate levels
loo_output = KuskoHarvPred:::loo_output   # model-averaged leave-one-out predictions

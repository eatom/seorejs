#description--------------------------------------------------
#file name: gsc_prophet
#author: Performance Media [Emil Tomczyński  & Gabriela Bugaj]
#start date: 16.10.2018
#description: downloading data from GSC and using Facebook's Prophet model to get forecast for future clicks


# libraries ---------------------------------------------------------------
library(Rcpp)
library(prophet)
library(dplyr)
library(purrr)
library(googleAuthR)
library(searchConsoleR)
library(googleAnalyticsR)
library(dygraphs)
library(xts)


# authenticate -----------------------------------------------------------

# set the scopes required

# options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
#                                         "https://www.googleapis.com/auth/analytics.readonly",
#                                         "https://www.googleapis.com/auth/webmasters"))
# 
# # you may also set the client id and secret here as well
# options(googleAuthR.client_id = "XXXXXX.apps.googleusercontent.com",
#         googleAuthR.client_secret = "ZZZZZ")
# 
# 


# set the scopes required
scopes = c("https://www.googleapis.com/auth/analytics",
           "https://www.googleapis.com/auth/analytics.readonly",
           "https://www.googleapis.com/auth/webmasters")



# set the client
gar_set_client("/home/emil/client-id-other.json", scopes = scopes)


# authenticate and go through the OAuth2 flow first time - specify a filename to save to by passing it in

options(httr_oob_default=TRUE) ## potrzebne TRUE gdy Rstudio jest na serwerze, żeby wklieć kod w konsoli


gar_auth(token = "sc_ga.httr-oauth", new_user = F)

# can run Google Analytics API calls:
ga_account_list() %>% View()

# and run Search Console API calls:
list_websites() %>% View()

# set variables -----------------------------------------------------------
# GSC parameters
website <- "https://queropassagem.com.br/"
# dim_filters <- c("query!~quero passagem") ## OPTIONAL: BRAND EXLUDED FROM RESULTS
searchType <- "web"
# dates
start_date_hist <- Sys.Date()-486
end_date_hist <- Sys.Date()-3
# period of the forecast since last historic observation in days
forecast_period <- 30   


# download data from GSC -----------------------------------------------
clicks <- search_analytics(website,
                           start = start_date_hist, 
                           end = end_date_hist,
                           dimensions = "date",
                           # dimensionFilterExp = dim_filters,
                           searchType = searchType, 
                           walk_data  = c("byBatch")) %>% 
  select(date, clicks)

# draw a time series ------------------------------------------------------
clicks_xts <- xts(x = clicks$clicks, order.by = clicks$date) 
dygraph(clicks_xts, main = "Number of Clicks") %>% 
  dyRangeSelector()

# forecasting -------------------------------------------------------------
clicks_pred <- clicks
colnames(clicks_pred) <- c("ds", "y") #change names of the columns for the purposes of prophet predictions

model <- prophet(clicks_pred, 
                 daily.seasonality = FALSE, 
                 yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE)

forecast_dates <- make_future_dataframe(model,
                                        periods = forecast_period,
                                        freq = "day")

forecast_df <- predict(model, forecast_dates)

# visualize forecasts -----------------------------------------------------
dyplot.prophet(model, forecast_df)
# plot(model, forecast_df, xlabel = "date", ylabel = "number of sessions")

# Prophet detects changepoints by first specifying a large number of potential changepoints 
# at which the rate is allowed to change
# The locations of the signification changepoints can be visualized with:
plot(model, forecast_df) + add_changepoints_to_plot(model)


# decomposition
prophet_plot_components(model, forecast_df) 


# Get the forecasted value
sum(tail(forecast_df[,c('ds','yhat')], n = forecast_period)$yhat) 

### Diagnosis ###########

df.cv <- cross_validation(model, initial = 400, period = 7, horizon = 60, units = 'days')
head(df.cv)

df.p <- performance_metrics(df.cv)
head(df.p)


## zapis predykcji do csv 

write.csv(tail(forecast_df[,c('ds','yhat')], n = forecast_period), "forecast_prophet_gsc.csv") ## tylko estymacja
write.csv(forecast_df[,c('ds','yhat')], "forecast_prophet_gsc.csv") ## całość 


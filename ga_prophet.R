#description--------------------------------------------------
#file name: ga_prophet
#author: Performance Media [Emil Tomczyński & Gabriela Bugaj]
#start date: 16.10.2018
#description: downloading data from Google Analytics and using Facebook's Prophet model to get forecast for future clicks


# libraries ---------------------------------------------------------------
library(Rcpp)
library(prophet)
library(dplyr)
library(purrr)
library(googleAuthR)
library(googleAnalyticsR)
library(dygraphs)
library(xts)
library(tibbletime)
library(highcharter)




# authenticate ------------------------------------------------------------

# set the scopes required
scopes = c("https://www.googleapis.com/auth/analytics",
           "https://www.googleapis.com/auth/analytics.readonly",
           "https://www.googleapis.com/auth/webmasters")

# set the client
gar_set_client("/home/emil/client-id-other.json", scopes = scopes)


# authenticate and go through the OAuth2 flow first time - specify a filename to save to by passing it in
options(httr_oob_default=TRUE) ## potrzebne gdy Rstudio jest na serwerze, żeby wklieć kod w konsoli


gar_auth(token = "sc_ga.httr-oauth", new_user = T)

# can run Google Analytics API calls:
ga_account_list() %>% View()


# set variables -----------------------------------------------------------


ga_id <- 46372209
# dates
# start_date_hist <- Sys.Date()-800
start_date_hist <- "2016-01-02"
# end_date_hist <- Sys.Date() -1 
end_date_hist <- "2019-08-31"
# period of the forecast since last historic observation in days
forecast_period <- 365  

seo_traffic  <- googleAnalyticsR::filter_clause_ga4(list(dim_filter("medium", "EXACT", "organic")))



# download data from GA -----------------------------------------------
sessions <- google_analytics(viewId = ga_id, 
                             date_range = c(start_date_hist, end_date_hist),
                             metrics = "sessions",
                             dim_filters = seo_traffic,
                             dimensions = "date", 
                             anti_sample = T)

# draw a time series ------------------------------------------------------
sessions_xts <- xts(x = sessions$sessions, order.by = sessions$date) 
dygraph(sessions_xts, main = "Number of Sessions") %>% 
  dyRangeSelector()

# forecasting -------------------------------------------------------------
sessions_pred <- sessions
colnames(sessions_pred) <- c("ds", "y") #change names of the columns for the purposes of prophet predictions

model <- prophet(sessions_pred, 
                 daily.seasonality = FALSE, 
                 yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE)

forecast_dates <- make_future_dataframe(model,
                                        periods = forecast_period,
                                        freq = "day")

forecast_df <- predict(model, forecast_dates)

sum(tail(forecast_df[,c('ds','yhat')], n = forecast_period)$yhat) 

# visualize forecasts -----------------------------------------------------
dyplot.prophet(model, forecast_df)
plot(model, forecast_df)

# plot(model, forecast_df, xlabel = "date", ylabel = "number of sessions")
prophet_plot_components(model, forecast_df) # decomposition

write.csv(forecast_df[,c('ds','yhat')], "forecast_prophet_ga.csv") 


## nice chart group by months

# group by month
monthly_sessions <- sessions %>% 
  tibbletime::as_tbl_time(date) %>% 
  tibbletime::collapse_by("monthly") %>%
  dplyr::arrange(date) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(sessions = sum(sessions))

monthly_sessions$date <- as.Date(monthly_sessions$date)

# add seo factor to trend

seo_factor <- seq(from = 1.00, to = 1.20, length.out = 12) ## increase 20% trends throught 12 months 

monthly_forecasts <- tail(forecast_df[,c('ds','yhat')], n = 365) %>% 
  tibbletime::as_tbl_time(ds) %>% 
  tibbletime::collapse_by("monthly") %>%
  dplyr::arrange(ds) %>% 
  dplyr::group_by(ds) %>% 
  dplyr::summarise(trend = sum(yhat)) %>% 
  dplyr::mutate(kpi = round(trend*seo_factor)) %>% 
  dplyr::rename(date = "ds")

monthly_forecasts$date <-  as.Date(monthly_forecasts$date)

## join all three series 
fullData <- monthly_sessions %>% 
  dplyr::full_join(monthly_forecasts, by = "date")

## make nice interactive plot  
highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_title(text = "Oragnic Sessions kpis for next 12 months") %>% 
  hc_xAxis(categories = fullData$date) %>% 
  hc_yAxis(title      = list(text = "Organic sessions")) %>% 
  hc_add_series(fullData$sessions,
                name = "historical data",
                dataLabels = list(
                  enabled = T)) %>% 
  hc_add_series(fullData$trend,
                name = "trend",
                dataLabels = list(
                  enabled = F)) %>% 
  hc_add_series(fullData$kpi,
                name = "trend+seo",
                dataLabels = list(
                  enabled = T)) %>% 
  hc_add_theme(hc_theme_gridlight())

# save data to csv
write.csv(fullData, "forecast_prophet_ga.csv") ## całość zapisana do csv


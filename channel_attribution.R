#description--------------------------------------------------
#file name: channel_attribution
#author: Performance Media [Emil Tomczyński & Gabriela Bugaj]
#start date: 16.10.2018
#description: downloading data from GA and using markov function to calculate importance of each channel in conversion


# libraries ----------------------------------------------------------------------
# library(ChannelAttributionApp)
library(ChannelAttribution)
library(googleAnalyticsR)
library(googleAuthR)
library(tidyr)
library(ggplot2)
library(survival)
library(plotly)
library(dplyr)


# authorization ------------------------------------------------

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



# set the client json
gar_set_client("/home/emil/client-id-other.json", scopes = scopes)

gar_auth(token = "sc_ga.httr-oauth", new_user = T)

# can run Google Analytics API calls:
ga_account_list() %>% View()

# downloading data from GA ------------------------------------------------

account_ga <- 10772734
property_ga <- "UA-10772734-2"  
view_ga <- 184949898 

goal_list <- googleAnalyticsR::ga_goal_list(account_ga, property_ga, view_ga) ## Display goals in GA View
goal_list <- goal_list[, c("id", "name")] ## show only the goal id and name 


# transaction filter
# trans_filter <- "mcf:conversionGoalNumber==002" # the conversion you're interested in
trans_filter <- "mcf:conversionGoalNumber==005" # the conversion you're interested in
visit_filter <- "mcf:conversionGoalNumber==005"   # "visit" conversion 001 for goal nr 1  

# date range
from <- "2019-09-01"
to   <- "2019-09-05"


# what do you want to know the attribution of?
dim = c("basicChannelGroupingPath, campaignPath") # https://developers.google.com/analytics/devguides/reporting/mcf/dimsmets/ 

# transactions
ga_transactions <- google_analytics_3(id = view_ga,
                                        start = from, 
                                        end = to,
                                        metrics = c("totalConversions", "totalConversionValue"),
                                        dimensions = c("basicChannelGroupingPath", "campaignPath"),
                                        filters = trans_filter,
                                        type="mcf",
                                        # max_results = 5000
                                        samplingLevel = "WALK"
)


# clean up and set class
ga_transactions[,1] <- gsub(" / ", "/", ga_transactions[,1])              # remove spacing
ga_transactions[,1] <- gsub(":?(NA|CLICK|NA):?", "", ga_transactions[,1]) # remove CLICK and NA
ga_transactions[,2] <- as.numeric(ga_transactions[,2])                    # conversion column is character :-/
ga_transactions[,3] <- as.numeric(ga_transactions[,3])                    # conversion column is character :-/

# visits (this view has a goal where a visit = a conversion)
ga_visits <- google_analytics_3(id = view_ga,
                                  start = from, end = to,
                                  metrics = c("totalConversions"),
                                  dimensions = dim,
                                  filters = visit_filter,
                                  type="mcf",
                                  samplingLevel = "WALK")

# clean up and set class
ga_visits[,1] <- gsub(" / ", "/", ga_visits[,1])              # remove spacing
ga_visits[,1] <- gsub(":?(NA|CLICK|NA):?", "", ga_visits[,1]) # remove CLICK and NA
ga_visits[,2] <- as.numeric(ga_visits[,2])

# change colnames
colnames(ga_transactions) <- c("path", "transactions", 'transVal')
colnames(ga_visits) <- c("path", "visits")

## group paths
ga_transactions <- ga_transactions %>% 
  dplyr::group_by(path) %>% 
  summarise(transactions = sum(transactions),
            transVal = sum(transVal)
            )

ga_visits <- ga_visits %>% 
  dplyr::group_by(path) %>% 
  summarise(visits = sum(visits))


# merge transactions and visits based on path

ga_transactions_visits <- dplyr::left_join(ga_visits, ga_transactions, by = "path")

# change transaction without paths to 0 
ga_transactions_visits[is.na(ga_transactions_visits$transactions), "transactions"] <- 0

# calculate conversion rate
ga_transactions_visits$rate <- ga_transactions_visits$transactions/ga_transactions_visits$visits
# null column = visits without transaction
ga_transactions_visits$null <- ga_transactions_visits$visits - ga_transactions_visits$transactions


# markov model ------------------------------------------------------------
# ...% customer interactions can be converted withouth specific channel being in place
# Markov chain is used to look at the contribution of various online channels to conversions

# remove rate
ga_transactions_visits <- ga_transactions_visits %>% 
  select(-rate) 

# remove nulls
ga_transactions_visits[is.na(ga_transactions_visits)] <- 0

mm_ga <- ChannelAttribution::markov_model(Data     = ga_transactions_visits,
                                            var_path = "path",
                                            var_conv = "transactions",
                                            var_value = "transVal", ## does not work when there is not enough data 
                                            var_null = "null",
                                            out_more = TRUE,
                                            order    = 1)  ## hyperparameter


# Order 0: Doesn’t know where the user came from or what step the user is on, only the probability of going to any page.
# Order 1: Looks back zero steps. You are currently at Step A (Sequence A). The probability of going anywhere is based on being at that step.
# Order 2: Looks back one step. You came from Step A (Sequence A) and are currently at Step B (Sequence B). The probability of going anywhere is based on where you were and where you are.
# Order 3: Looks back two steps. You came from Step A > B (Sequence A) and are currently at Step C (Sequence B). The probability of going anywhere is based on where you were and where you are.
# Order 4: Looks back three steps. You came from Step A > B > C (Sequence A) and are currently at Step D (Sequence B). The probability of going anywhere is based on where you were and where you are.


mm_ga$channel_name <- iconv(mm_ga$channel_name, from = "UTF-8")

ga_markov <- mm_ga$result


# Removal effect says how many conversions are happening without that channel being in place.
mm_ga$removal_effects %>% View()
mm_ga$transition_matrix %>% View()
mm_ga$result %>% View()



# heuristic model ---------------------------------------------------------

# First Touch Conversion: The conversion happening through the channel when that channel is the first touch point for a customer. 100% credit is given to the first touch point.
# 
# Last Touch Conversion: The conversion happening through the channel when that channel is the last touch point for a customer. 100% credit is given to the last touch point.
# 
# Linear Touch Conversion: All channels/touch points are given equal credit in the conversion.

hm_ga <- ChannelAttribution::heuristic_models(Data     = ga_transactions_visits,
                                                var_path = "path",
                                                var_value =  "transVal",
                                                var_conv = "transactions")

hm_ga$channel_name <- iconv(hm_ga$channel_name, from = "UTF-8")

# merge acquisition sources -----------------------------------------------
# importance of the each channel - the higher the more important

hm_mm_ga <- dplyr::left_join(hm_ga, mm_ga$result, by = "channel_name")

# rename columns
names(hm_mm_ga)[names(hm_mm_ga) == 'total_conversions'] <- 'data driven model'
names(hm_mm_ga)[names(hm_mm_ga) == 'total_conversion_value'] <- 'data driven value'


hm_mm_ga_removal_effect <- cbind(hm_mm_ga, mm_ga$removal_effects)
# each column shows assigned number of conversions according to different models


# total conversion barplot ------------------------------------------------

# select only columns with conversions 
hm_mm_ga_conv <- hm_mm_ga %>% 
                    select("channel_name",
                           "first_touch_conversions",
                           "last_touch_conversions",
                           "linear_touch_conversions",
                           "data driven model")
# transforms the dataset into a data frame that ggplot2 can use to graph the outcomes
ga_total_conversions <- tidyr::gather(data  = hm_mm_ga_conv,
                                        key   = "attribution_model",
                                        value = "conversion",
                                        -channel_name)



plot_ga_conversions <- plotly::ggplotly(ggplot2::ggplot(data = ga_total_conversions, mapping = aes(x = channel_name, y = conversion, fill = attribution_model))+
                                     ggplot2::geom_bar(stat = "identity", position = "dodge")+
                                     ggplot2::ggtitle("Total conversions attributed to each channel")+
                                     ggplot2::ylab("Conversions")+
                                     ggplot2::xlab("Channel")+
                                     ggplot2::theme(
                                       panel.background = element_rect(fill = "white",
                                                                       colour = "white",
                                                                       size = 0.5, linetype = "solid"),
                                       panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                       colour = "lightgrey"),
                                       panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                       colour = "grey")))

plot_ga_conversions

# value conversion barplot ------------------------------------------------

# select only columns with conversions 
hm_mm_ga_val <- hm_mm_ga %>% 
  select("channel_name",
         "first_touch_value",
         "last_touch_value",
         "linear_touch_value",
         "data driven value")

## show value of transactions by channel
ga_total_conversions_val <- tidyr::gather(data  = hm_mm_ga_val,
                                      key   = "attribution_model",
                                      value = "channel_value",
                                      -channel_name)

ga_total_conversions_val$pretty_value <-  prettyNum(ga_total_conversions_val$channel_value, scientific=FALSE, big.mark=" ")

plot_ga_value <- plotly::ggplotly(ggplot2::ggplot(data = ga_total_conversions_val, mapping = aes(x = channel_name, y = channel_value, fill = attribution_model ))+
                                          ggplot2::geom_bar(stat = "identity", position = "dodge")+
                                          ggplot2::ggtitle("Total Value of conversions attributed to each channel")+
                                          ggplot2::ylab("Value of conversions in $")+
                                          ggplot2::xlab("Channel")+
                                          ggplot2::scale_y_continuous(labels = scales::comma)+
                                          ggplot2::theme(
                                            panel.background = element_rect(fill = "white",
                                                                            colour = "white",
                                                                            size = 0.5, linetype = "solid"),
                                            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                            colour = "lightgrey"),
                                            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                            colour = "grey")))

plot_ga_value



# transition matrix heatmap for "real" data -------------------------------


df_plot_trans <- mm_ga$transition_matrix

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")


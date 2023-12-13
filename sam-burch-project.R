

# Preliminary -------------------------------------------------------------


## Libraries
library(tidyverse)
library(glmnet)
library(caret)
library(gt)
library(caTools)
library(car)
library(lmtest)
library(MASS)


## Load in Data
full_df = read_csv("C:/Users/sburc/OneDrive/Desktop/stat-443/project/OnlineNewsPopularity.csv")



## Cleaning the data
df1 = full_df |>
  # Removing article name and other potential share variables
  dplyr::select(-c(url, timedelta)) |> 
  # World Articles
  filter(data_channel_is_world == 1) |> 
  dplyr::select(-c("data_channel_is_lifestyle",
            "data_channel_is_entertainment",
            "data_channel_is_bus",
            "data_channel_is_socmed",
            "data_channel_is_tech",
            "data_channel_is_world")) |> 
  # Rate +/- shouldn't be 0
  filter(rate_positive_words != 0, rate_negative_words != 0) 

df2 = df1 |> 
  # Replace current cols
  mutate(across(n_tokens_title:self_reference_avg_sharess, scale),
         across(n_tokens_title:self_reference_avg_sharess, pnorm),
         across(LDA_00:abs_title_sentiment_polarity, scale),
         across(LDA_00:abs_title_sentiment_polarity, pnorm))




# Linear Regression ----------------------------------------------------------------


# Train-Test Split

set.seed(123)

n = nrow(df2)

train_ind = sample(1:n, size = 0.8*n)




train_x = (df2 |> dplyr::select(-shares))[train_ind, ]
train_y = (df2 |> dplyr::select(shares))[train_ind, ]

test_x = (df2 |> dplyr::select(-shares))[-train_ind, ]
test_y = (df2 |> dplyr::select(shares))[-train_ind, ]


# Full Model


m1 = lm(shares ~ ., data = data.frame(train_x, train_y))

summary(m1)
# NAs: weekend, sunday, rate NW!!!


# Reduce model by step ftn

m2 = step(m1)
summary(m2)


predictions0 = predict(m2, newdata = test_x)


rmse0 = sqrt(mean((predictions0 - (test_y |> pull()))^2))
rmse0


# Best predictors (so far)

predictors = names(m2$coefficients[order(-abs(m2$coefficients))])

top_predictors = as.tibble(m2$coefficients[order(-abs(m2$coefficients))]) |> 
  mutate(names = predictors) |> 
  dplyr::select(names, value)

top_predictors |> print(n = 22)


predictors_table1 = top_predictors |>
  filter(names != "(Intercept)") |> 
  mutate(value = round(value, 2)) |> 
  # filter(abs(value) > 1220) |> 
  gt() |> 
  tab_header(
    title = "Top Predictors (Original)",
    subtitle = "RMSE = 4009"
  ) |> 
  cols_label(
    names = "Name",
    value = "Value"
  ) |> 
  data_color(
    columns = value,
    palette = c("red", "green")
  ) |> 
  tab_footnote(
    footnote = "Color indicates impact (+/-) of the predictor.",
    locations = cells_column_labels(columns = value)
  )
predictors_table1

# gtsave(predictors_table1, filename = "tbl1.png", expand = 50)



# Good but let us check diagnostics


sqrt(vif(m2))
## Not much, but some multicollinearity issues may be present


correlation_tbl = df1 |> 
  # Removes Intercept
  dplyr::select(predictors[ -4]) |> 
  cor()

correlation_tbl[correlation_tbl > .5 & correlation_tbl < 1]
correlation_tbl > .5 & correlation_tbl < 1

predictors_table1



# Keep with highest weight
## kw_min_max, kw_min_avg***
## kw_avg_max, kw_max_max***
## Rate Positive Words, Global Rate Positive Words, ***Global Sentiment polarity



red_train_x = train_x |> dplyr::select(predictors[ -c(4, 13, 15, 18, 19)])
red_train_x


m3 = lm(shares ~ ., data.frame(red_train_x, train_y))
summary(m3)


m4 = step(m3)
## Drops kw_max_max & global_sentiment_polarity
summary(m4)

# RMSE

predictions = predict(m4, newdata = test_x)


rmse = sqrt(mean((predictions - (test_y |> pull()))^2))
rmse


predictors2 = names(m4$coefficients[order(-abs(m4$coefficients))])
predictors2
top_predictors2 = as.tibble(m4$coefficients[order(-abs(m4$coefficients))]) |> 
  mutate(names = predictors2) |> 
  dplyr::select(names, value)

top_predictors
top_predictors2




predictors_table2 = top_predictors2 |>
  filter(names != "(Intercept)") |> 
  mutate(value = round(value, 2)) |>
  # filter(abs(value) > 1550) |> 
  gt() |> 
  tab_header(
    title = "Top Predictors (Final)",
    subtitle = "RMSE = 4007"
  ) |> 
  cols_label(
    names = "Name",
    value = "Value"
  ) |> 
  data_color(
    columns = value,
    palette = c("red", "green")
  ) |> 
  tab_footnote(
    footnote = "Color indicates impact (+/-) of the predictor.",
    locations = cells_column_labels(columns = value)
  )
predictors_table2


# gtsave(predictors_table2, filename = "tbl2.png", expand = 50)


# Diagnostics

par(mfrow = c(2, 2))
plot(m4)


sqrt(vif(m4))
sort(sqrt(vif(m4)), decreasing = TRUE)

## Collinearity issues fixed


sort(cooks.distance(m4), decreasing = TRUE)[1:10]

## No HIPs


bptest(m4)

## NO constant variance

ks.test(m4$residuals, "pnorm", mean = mean(m4$residuals), sd = sd(m4$residuals))

## Not normal







# Fixing Constant Variance


m5 = lm(shares ~ ., data.frame(red_train_x, train_y), weights = 1 / residuals(m4)^2)
summary(m5)


predictions2 = predict(m5, newdata = test_x)
predictions2

rmse2 = sqrt(mean((predictions2 - (test_y |> pull()))^2))
rmse2

bptest(m5)

sort(sqrt(vif(m5)), decreasing = TRUE)

sort(cooks.distance(m5), decreasing = TRUE)[1:10]



ks.test(m5$residuals, "pnorm", mean = mean(m5$residuals), sd = sd(m5$residuals))



# Fixing Normaility

par(mfrow = c(1, 1))

boxcox(m4, lambda = seq(-2,2, length = 400))

m6 = lm(log(shares) ~ ., data.frame(red_train_x, train_y), weights = 1 / residuals(m4)^2)
summary(m6)


ks.test(m6$residuals, "pnorm", mean = mean(m6$residuals), sd = sd(m6$residuals))


## Can't be fixed



# Videos & Images -------------------------------------------------------------


# Video DF
vids_df = df1 |>
  group_by(num_videos) |> 
  summarise(tot_shares = sum(shares),
            avg_shares = mean(shares),
            med_shares = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n(),
            .groups = "drop") |> 
  arrange(-n)
vids_df

# Images DF
imgs_df = df1 |>
  group_by(num_imgs) |> 
  summarise(tot_shares = sum(shares),
            avg_shares = mean(shares),
            med_shares = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n(),
            .groups = "drop") |> 
  arrange(-n)
imgs_df




# Image Y/N
df1 |> 
  mutate(img = if_else(num_imgs >= 1, 1, 0),
         vid = if_else(num_videos >= 1, 1, 0)) |>
  group_by(img) |> 
  summarise(avg = mean(shares),
            med = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n())
# Suggests no images are better than some
# Try only a few versus a lot


# Video Y/N
df1 |> 
  mutate(img = if_else(num_imgs >= 1, 1, 0),
         vid = if_else(num_videos >= 1, 1, 0)) |>
  group_by(vid) |> 
  summarise(avg = mean(shares),
            med = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n())
# Suggests some are better than none
# Compare more in 


# Image mult
img_mult = df1 |> 
  mutate(img = case_when(num_imgs == 0 ~ "0",
                         num_imgs == 1 ~ "1",
                         num_imgs == 2 ~ "2",
                         num_imgs >= 3 ~ "3+"),
         vid = case_when(num_imgs == 0 ~ "0",
                         num_imgs == 1 ~ "1",
                         num_imgs >= 2 ~ "2+")) |>
  group_by(img) |> 
  summarise(avg = mean(shares),
            med = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n())
img_mult
# For 0, higher median and ceiling
# For 1/2, lower median (avoid)
# For 3+, higher median and floor


img_table = img_mult |> 
  mutate(round(across(avg:top_10))) |> 
  gt() |> 
  cols_align("center") |> 
  tab_header(title = "Effect of Images by Categories") |> 
  cols_label(
    img = "Images",
    avg = "Average",
    med = "Median",
    bottom_10 = "10th Pct.",
    bottom_25 = "25th Pct.",
    top_25 = "75th Pct.",
    top_10 = "90th Pct.",
    n = "Sample Size"
  ) |> 
  data_color(
    columns = avg:top_10,
    palette = c("red", "green"))

# gtsave(img_table, filename = "img_tbl.png")





# Video mult
vid_mult = df1 |> 
  mutate(img = case_when(num_imgs == 0 ~ "0",
                         num_imgs == 1 ~ "1",
                         num_imgs == 2 ~ "2",
                         num_imgs >= 3 ~ "3+"),
         vid = case_when(num_videos == 0 ~ "0",
                         num_videos == 1 ~ "1",
                         num_videos >= 2 ~ "2+")) |>
  group_by(vid) |> 
  summarise(avg = mean(shares),
            med = median(shares),
            bottom_10 = quantile(shares, .1),
            bottom_25 = quantile(shares, .25),
            top_25 = quantile(shares, .75),
            top_10 = quantile(shares, .9),
            n = n())
vid_mult
# 2+ is certainly better
## High floor & ceiling lead to highest average & median


vid_table = vid_mult |> 
  mutate(round(across(avg:top_10))) |> 
  gt() |> 
  cols_align("center") |> 
  tab_header(title = "Effect of Videos by Categories") |> 
  cols_label(
    vid = "Videos",
    avg = "Average",
    med = "Median",
    bottom_10 = "10th Pct.",
    bottom_25 = "25th Pct.",
    top_25 = "75th Pct.",
    top_10 = "90th Pct.",
    n = "Sample Size"
  ) |> 
  data_color(
    columns = avg:top_10,
    palette = c("red", "green"))


# gtsave(vid_table, filename = "vid_tbl.png")



# Interaction

m1_img_vid = lm(shares ~ num_imgs * num_videos, data = df1)
summary(m1_img_vid)

summary(step(m1_img_vid))

## No interaction for number


new_df1 = df1 |> 
  mutate(img = if_else(num_imgs >= 1, 1, 0),
         vid = if_else(num_videos >= 1, 1, 0))


m2_img_vid = lm(shares ~ img * vid, data = new_df1)
summary(m2_img_vid)

## Negative interaction for inclduing both
## Skewed because of poor performance by a 1 or 2 imaged articles




# Videos & Images Plots ---------------------------------------------------

# Videos

ggplot(vids_df, aes(x = num_videos, y = avg_shares)) +
  geom_smooth(se = FALSE, color = "gray") +
  labs(
    title = "How Videos affect Shares",
    y = "Average Shares",
    x = "# of Videos"
  ) +
  geom_point(color = "darkorange") +
  # scale_y_continuous(breaks = seq(0, 4000, 500)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Huge outliers


ggplot(vids_df |> filter(n > 25), aes(x = num_videos, y = avg_shares)) +
  geom_smooth(se = FALSE, color = "gray") +
  labs(
    title = "How Videos affect Shares",
    subtitle = "sample size > 25",
    y = "Average Shares",
    x = "# of Videos"
  ) +
  geom_point(color = "darkorange") +
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# ggsave("vids-scatter.png", width = 16, height = 9, units = "cm")





# Images Plot

ggplot(imgs_df, aes(x = num_imgs, y = avg_shares)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE, color = "gray") +
  labs(
    title = "How Images affect Shares",
    y = "Average Shares",
    x = "# of Images"
  ) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Huge outliers

ggplot(imgs_df |> filter(n > 25), aes(x = num_imgs, y = avg_shares)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE, color = "gray") +
  labs(
    title = "How Images affect Shares",
    subtitle = "sample size > 25",
    y = "Average Shares",
    x = "# of Images"
  ) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# ggsave("imgs-scatter.png", width = 16, height = 9, units = "cm")





library(dslabs)
library(HistData)
library(dplyr)
library(caret)

# Q1 
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

summary(dat)
nrow(dat)

total_days <- diff(range(dat$date))
span <- 60/as.numeric(total_days)
fit <- loess(deaths ~ as.numeric(date), degree=1, span=span, data=dat)
#dat %>% mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
#  ggplot(aes(date, deaths)) +
#  geom_point(size=3, alpha = .5, color = "grey") +
#  geom_line(aes(day, smooth), color="red")

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

# Q3
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
summary(mnist_27$train)

class(mnist_27$train$y)
class(mnist_27$train$x_2)

# fit2 <- loess(as.numeric(y) ~ x_2, degree=1, data=mnist_27$train)
# summary(fit2)
# dat %>% 
#   mutate(smooth = fit2$fitted) %>%
#   ggplot(aes(y, x_2)) +
#   geom_point(size=3, alpha = .5, color = "grey") +
#   geom_line(aes(y, smooth), color="red")

# ggplot(mnist_27$train, aes(x_2, y)) + 
#  geom_point() +
#  geom_smooth(method = "loess", se = FALSE)
# summary(mnist_27$train)

span <- 1/3
fit <-loess(as.numeric(y) ~ x_2, data = mnist_27$train, span = span)
mnist_27$train %>%
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(x_2, smooth), color = "red")

# https://google.github.io/CausalImpact/CausalImpact.html

library(CausalImpact)

# Creating an example dataset
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

dim(data)
head(data)
matplot(data, type = "l")


# Running an analysis
pre.period <- c(1, 70)
post.period <- c(71, 100)

impact <- CausalImpact(data, pre.period, post.period)


# Plotting the results
plot(impact)
summary(impact)
summary(impact, "report")


# Working with dates and times
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), time.points)
head(data)

pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)

plot(impact)
summary(impact)
summary(impact, "report")
impact$summary


# Adjusting the model
impact <- CausalImpact(data, pre.period, post.period, 
                       model.args = list(niter = 5000, nseasons = 7))

plot(impact)
summary(impact)


# Adjusting the model
post.period <- c(71, 100)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA

ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)

impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)

plot(impact)
summary(impact)
summary(impact, "report")

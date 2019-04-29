sample <- content_repo%>%
  filter(site_key == 3006)%>%
#  mutate(time = (lead(years)-2003)*12+ lead(months) - 4)%>%
  select1(downloads, cum_uploads)

plot(sample$downloads, sample$cum_uploads, main = "downloads versus cumulated uploads",
     xlab = "cumulated uploads", ylab = "downloads")
abline(lm(sample$downloads ~ sample$cum_uploads))

#### Step 1. Scale variables. Most important step. Scale them such that parameter estimates acquire similar magnitudes
x = sample$cum_uploads/sd(sample$cum_uploads)
y = sample$downloads/sd(sample$downloads)
#t = sample$time

model1=nls(y~SSgompertz(x,Asym,b2,b3))
summary(model1)

plot(x,y)
lines(x, predict(model1), col = "red")



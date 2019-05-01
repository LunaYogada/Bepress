library(broom)
library(ggplot2)
library(tidyverse)

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

plot(x,y, xlab = "standardized total uploads", ylab = "standardized downloads")
lines(x, predict(model1), col = "red")

ggplot()+
  geom_point(aes(x = x, y= y))+
  geom_line(aes(x = x, y= predict(model1)), col = "red")


result_df <- data.frame(matrix(numeric(),ncol = 4, nrow = nrow(sites)))
colnames(result_df) <- c("site_key","parameter","summary","residual SE")

for(i in 1:nrow(sites)){
  key = sites[[i, "site_key"]]
  if(!key %in% c(2,5,8, 1794,2730)){
    tryCatch({
      print(paste0("now is modeling for", key,"-----------"))
      data<-content_repo%>%
        filter(site_key == key)
      x = data$cum_uploads
      y = data$downloads
      models = nls(y~SSgompertz(x,Asym,b2,b3))
      
      #calculate aic aicc bic
      #  nn=ncol(t(zsales))								# sample size
      #  pp=ncol(t(est))								# number of parameters
      # residuals
      #  sig2=sum(err^2) /(nn-pp)						# sigma^2 of error term
      # tvals for inference: abs(tval) > 1. 65 => 90% confidence level; abs(tval) > 1.96 => 95% CI
      #  aic = nn*log(sig2) + 2*pp						# Use when nn is large (i.e., pp/nn < 5%)
      # bic = nn*log(sig2) + pp*log(nn)					# Use when nn is large (i.e., pp/nn < 5%)


      # store results into dataframe
      result_df[[i, "site_key"]] <- key
      result_df[[i, "parameter"]] <-tidy(models)
      result_df[[i, "summary"]] <-summary(models)
      result_df[[i, "residual SE"]] <-summary(models)$sigma
      
      # save plot of model prediction
      plt <- ggplot()+
        geom_point(aes(x = x, y= y))+
        geom_line(aes(x = x, y= predict(models)), col = "red")+
        labs(x = "standardized total uploads", y = "standardized downloads", title ="gompertz model prediction")+
        theme_bw()
      ggsave(paste0( "plots/nonlinear/",key, "model.jpg"), plot = plt)},
      
      error=function(e){
        print(paste0(key,"ERROR :",conditionMessage(e)))
        })
  }else{ print(paste0("not modeling for", key))}
}



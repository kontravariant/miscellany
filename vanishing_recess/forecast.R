library(ggplot2)
library(dplyr)
library(forecast)
indat = read.csv("rgdp.csv")

ggplot(indat,mapping=aes(x=DATE,y=GDPC1)) + geom_line(aes(group=1))

ts = data.frame(rgdp=indat$GDPC1)
rgdp = ts(ts,frequency=4)
plot(rgdp)

q = factor(cycle(rgdp))
trend = time(rgdp)
fit = lm(rgdp ~ 0 + trend + q, na.action=NULL)
plot(rgdp, type="o");lines(fitted(fit), col=2)

rgdp.pre = ts(rgdp[1:101],frequency=4)
plot(rgdp.pre)
q.pre= factor(cycle(rgdp.pre))
trend.pre= time(rgdp.pre)
fit.pre = lm(rgdp.pre ~ 0 + trend.pre + q.pre, na.action = NULL)
plot(rgdp.pre,type='o');lines(fitted(fit.pre),col=2)

mdl = auto.arima(rgdp.pre)

fca = forecast.Arima(mdl, 30)
g = ggplot(fca) + geom_line(gr

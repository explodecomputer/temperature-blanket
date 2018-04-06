library(readxl)
library(lubridate)
library(ggplot2)

a <- read_xlsx("../data/temp.xlsx")
a$dum <- 1
a$day <- 1:nrow(a)
a$bin1 <- cut(as.numeric(a$avg), 8)
ggplot(a, aes(x=day, y=dum)) + geom_tile(aes(fill=as.numeric(bin1))) + scale_fill_brewer(type="qual")
ggplot(a, aes(x=day, y=dum)) + geom_tile(aes(fill=bin1)) + scale_fill_brewer(type="seq")


delivery_date <- ymd("2018-08-25")
start_date <- delivery_date - days(280)


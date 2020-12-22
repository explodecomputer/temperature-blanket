library(tidyverse)
library(lubridate)

fit_cos <- function(firstday, lastday)
{
	y1 <- year(firstday)
	if(firstday > paste0(y1, "/06/21"))
	{
		ss <- paste0(y1+1, "/06/21") %>% ymd()
	} else {
		ss <- paste0(y1, "/06/21") %>% ymd()
	}

	if(firstday > paste0(y1, "/12/21"))
	{
		ws <- paste0(y1+1, "/12/21") %>% ymd()
	} else {
		ws <- paste0(y1, "/12/21") %>% ymd()
	}

	d <- tibble(
		date = seq(firstday, firstday + days(365), by="days"),
		day = 1:length(date),
		x = seq(0, 2 * pi * length(date)/365, length.out=length(date)),
		y = cos(x)
	)
	i <- which(d$date == ss)
	d$y2 <- d$y
	index1 <- i:max(d$day)
	d$y2[index1] <- d$y[1:length(index1)]
	index2 <- 1:i
	d$y2[index2] <- d$y[length(index1):nrow(d)]
	d$y <- (d$y2 + 1) / 2
	d <- select(d, -y2)
	d <- subset(d, date <= lastday)
	return(d)
}

o <- read_csv("../data/Temperature blanket - Sheet1.csv") %>%
	filter(!is.na(Day)) %>%
	mutate(
		year = case_when(
			month %in% c("nov", "dec") ~ 2017,
			TRUE ~ 2018
		),
		d = paste0(year, "/", month, "/", Day),
		date = parse_date_time(d, "%y/%b/%d")
	)
d <- fit_cos(o$date[1], o$date[nrow(o)])
o <- inner_join(d, o, by="date")

ol <- o %>%
	mutate(
		midpoint = (max + min)/2,
		weighted = (max * y + min * (1-y))
	) %>%
	pivot_longer(
		c(min, max, midpoint, weighted)
	) 

ggplot(ol, aes(y=value, x=date)) +
geom_point(aes(colour=name)) +
geom_smooth(aes(colour=name)) +
scale_colour_brewer(type="qual")
ggsave("../docs/sinusoidal_wave.png")

ol %>%
	group_by(name) %>%
	summarise(mean = mean(value), var=var(value))

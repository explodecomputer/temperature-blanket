# e.g http://www.martynhicks.uk/weather/data.php?page=m10y2020

library(tidyverse)
library(rvest)
library(lubridate)
library(ggthemes)


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

scrape_temperatures <- function(startdate)
{
	url <- "http://www.martynhicks.uk/weather/data.php?page="

	# Don't look for the current month it won't be up yet
	to <- min(today() - months(1), startdate + days(280))

	months <- seq(startdate, to, by="month")
	dat <- lapply(months, function(d)
	{
		message(d)
		Sys.sleep(2)
		x <- read_html(paste0(url, "m", formatC(month(d), digits=1, flag="0", format="d"), "y", year(d))) %>%
			html_nodes("table") %>%
			{.[4]} %>%
			html_nodes("tr") %>%
			{.[4]} %>%
			html_nodes("td") %>%
			{
				tibble(
					year = year(d),
					month = month(d),
					max = .[2] %>% html_text() %>% scan(text=., what="") %>% as.numeric(),
					min = .[3] %>% html_text() %>% scan(text=., what="") %>% as.numeric(),
					ave = .[4] %>% html_text() %>% scan(text=., what="") %>% as.numeric(),
				)
			} %>%
			mutate(
				day = 1:n(),
				date = ymd(paste(year, month, day))
			) %>%
			filter(!is.na(max) & date >= startdate & date <= (startdate + days(279))) %>%
			select(date, everything(), -year, -month, -day)
	}) %>% bind_rows()

	sindat <- fit_cos(startdate, startdate + days(280))
	res <- left_join(dat, sindat, by="date") %>%
		mutate(
			temperature = (max * y + min * (1-y)),
			col_sin = case_when(
				temperature < 0 ~ 1,
				temperature <= 4 ~ 2,
				temperature <= 9 ~ 3,
				temperature <= 14 ~ 4,
				temperature <= 19 ~ 5,
				temperature <= 24 ~ 6,
				TRUE ~ 7
			),
			col_ave = case_when(
				ave < 0 ~ 1,
				ave <= 4 ~ 2,
				ave <= 9 ~ 3,
				ave <= 14 ~ 4,
				ave <= 19 ~ 5,
				ave <= 24 ~ 6,
				TRUE ~ 7
			),
			col_min = case_when(
				min < 0 ~ 1,
				min <= 4 ~ 2,
				min <= 9 ~ 3,
				min <= 14 ~ 4,
				min <= 19 ~ 5,
				min <= 24 ~ 6,
				TRUE ~ 7
			),
			col_max = case_when(
				max < 0 ~ 1,
				max <= 4 ~ 2,
				max <= 9 ~ 3,
				max <= 14 ~ 4,
				max <= 19 ~ 5,
				max <= 24 ~ 6,
				TRUE ~ 7
			),		
			day = 1:n(),
			row = ceiling(day / 14),
			col = ((day-1) %% 14) + 1
		) %>%
		select(-x, -y)

	return(res)
}

colours <- tribble(
	~code, ~hex,
	1, "#f4ead7",
	2, "#b5a49c",
	3, "#6a6a6c",
	4, "#424450",
	5, "#616f4d",
	6, "#d4b13d",
	7, "#892c1b"
)
co <- colours$hex
names(co) <- seq_along(co)

res <- scrape_temperatures(ymd("2017/11/18"))
ggplot(res, aes(x = col, y=row)) +
geom_tile(aes(fill=as.factor(col_sin)), colour="white", size=2) +
scale_fill_manual(values=co) +
theme_void() +
theme(legend.position="")
ggsave(file="../docs/asher_squares.png", width=10, height=16)

res2 <- scrape_temperatures(ymd("2020/10/23"))
res2 %>%
	pivot_longer(c(col_sin, col_max, col_min, col_ave)) %>%
	ggplot(aes(x = col, y=row)) +
	geom_tile(aes(fill=as.factor(value)), colour="white", size=2) +
	scale_fill_manual(values=co) +
	facet_grid(name ~ .) +
	theme_void() +
	theme(legend.position="")
ggsave(file="../docs/new_squares.png", width=10, height=10)

write.csv(res2, file="../data/blanket2.csv")

#puerto rico hurricane mortality
#some question wording may be from harvardx staff
library(tidyverse)
library(pdftools)
options(digits = 3) 
#q3 what is x, how many entries in x?
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)
txt
txt[9]
x <- str_split(txt[9], "\n")
x
class(x)
length(x)

#q4 s is first entry of x. what is s?
s <- x[[1]]
s
length(s)

#q5 trim s
s <- str_trim(s)
s
s[[1]]

#q6 find row with header
header_index <- min(str_which(s, "2015"))
header_index

#q7 extract month and col names from header row
header <- s[[2]]
header
header <- str_split(header, "\\s+", simplify = TRUE) #temp file
month <-header[1] #take only the first col
header <- header[-1] #remove the first col
header[3] #3rd value in header

#q8 create "tail index" to hold values of final TOtal row
s
tail_index <- 35

#q9 creat object for count of numbers in each row, how many rows have 1 number
n <- str_count(s, pattern = "\\d+")
length(which(n==1))

#q10 remove entries from rows we don't need
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

#q11 remove anything not a digit or a space
s <- str_remove_all(s, "[^\\d\\s]")
s

#q12 convert s into a data matrix with just the day and count data
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s
#name data cols, convert to numeric, add month col, find mean deaths before & after 'cane
tab <- tibble(day = s[,1], "2015" = s[,2], "2016" = s[,3], "2017" = s[,4],
              month = "SEP")
tab
tab$"2015" <- as.numeric(tab$"2015")
tab$`2016` <- as.numeric(tab$`2016`)
tab$"2017" <- as.numeric(tab$"2017")
class(tab$"2015")
class(tab$`2016`)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

#q13 change to a tidy format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

#q14 plot the data
tab <- tab %>% filter(year != "2018")
tab %>% ggplot(aes(x = day, y = deaths, col = year)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 20)
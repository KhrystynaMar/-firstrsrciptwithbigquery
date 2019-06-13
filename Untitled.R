library(tidyverse)
library(lubridate)
library(bigrquery)
project <- "diesel-bee-242813"
sql <- "SELECT apgar_5min, ever_born, year, month, day, gestation_weeks, apgar_1min, plurality, cigarette_use, alcohol_use, wday, source_year, mother_age, father_race, drinks_per_week, mother_married FROM [publicdata:samples.natality]"
query_exec(sql, project = project)
todo_copies <- query_exec(sql, project = project, useLegacySql = TRUE)
write.csv(
  x = todo_copies,
  file = "Todo_copies.csv",
  row.names = TRUE)
tabble1 <-
  tibble(date = seq.Date(from = ymd(19690101),
                         to   = ymd(20081231),
                         by   = "day"),
         year = year(date),
         day = day(date),
         month = month(date))
x1.df = merge(tabble1, todo_copies)
temp <- group_by(
  .data = x1.df,  
  date)
temp <- summarize(
  .data = temp, 
  Avg.baby3 = sum(ever_born))
ggplot(temp, aes(x=date, y=Avg.baby3)) + 
  geom_bar(stat="identity")
tabble2 <-
  tibble(date = seq.Date(from = ymd(19690101),
                         to   = ymd(20081231),
                         by   = "day"),
         year = year(date),
         day = day(date),
         month = month(date),
         plurality = 1)
x2.df = merge(tabble2, todo_copies)
temp1 <- group_by(
  .data = x2.df,  
  year)
temp1 <- summarize(
  .data = temp1, 
  Avg.baby1 = sum(plurality))
write.csv(
    x = temp1,
    file = "Temp1.csv",
    row.names = TRUE)
ggplot(temp1, aes(x=year, y=Avg.baby1)) + 
  geom_bar(stat="identity")
tabble3 <- select(
  .data = todo_copies,
  apgar_1min,
  mother_age,
  father_race,
  plurality,
)
write.csv(
  x = tabble3,
  file = "tabble3.csv",
  row.names = TRUE)
library(car)
scatterplot( apgar_1min | apgar_5min ~ gestation_weeks | year | alcohol_use | mother_age | cigarette_use, data=todo_copies )
tabble4 <- tabble3 %>% filter( apgar_1min > 1)
tabble5 <- lapply(d, function(x) as.numeric(as.character(x)))
corr <- round(cor(tabble4))
write.csv(
  x = corr,
  file = "corr.csv",
  row.names = TRUE)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of baby", 
           ggtheme=theme_bw)
par(mfrow = c(2,2))
ggplot(temp, aes(x=date, y=Avg.baby)) + 
  geom_bar(stat="identity")
ggplot(temp1, aes(x=date, y=Avg.baby)) + 
  geom_bar(stat="identity")
scatterplot( apgar_1min | apgar_5min ~ gestation_weeks | year | alcohol_use | mother_age | cigarette_use, data=todo_copies )
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of baby", 
           ggtheme=theme_bw)
dates <- seq.Date(from = ymd(19690101),
                  to   = ymd(20081231),
                  by   = "day")
)
dates <- unlist(dates, use.names = FALSE)
dates <- as.Date('dates', tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
                 optional = FALSE)
Avg.baby <- select(
  .data = temp,
  Avg.baby
)
Avg.baby3 = c()
for(atmp in temp['Avg.baby']) { Avg.baby3 <- atmp }
class(Avg.baby3)

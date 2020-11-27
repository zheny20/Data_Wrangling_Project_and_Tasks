library("RODBC")
library("stringr")
Ex51<-sqlQuery(mycon,"select * from Flowsheets")
sub("(cc/kg?)","CC-Kg",Ex51$DISP_NAME)
gsub(pattern = "[[:alnum:]]", " ",Ex51)
Ex53<-sqlQuery(mycon,"select * from Provider")
newcol <- str_split(Ex53$NEW_PROV_NAME, ", ")
grep("^Wa", Ex53$NEW_PROV_NAME, value =TRUE)
table2_rate <- tibble(
  cases = table2_case$cases,
  population = table2_population$population
  year = table2_case$year,
  country = table2_case$country) %>%
  mutate(rate = (table2_case$cases / table2_population) * 10000)

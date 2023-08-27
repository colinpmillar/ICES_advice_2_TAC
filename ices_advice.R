library(dplyr)
library(icesASD)

# read in spreadsheet page
sheet <- read.csv("ices_advice_for_nat.csv") %>% tibble()

# what can I get
sheet_fill <-
  sheet %>%
  select(
    ICES.code, Advice, Year,
    Catches.corresponding.to.advice,
    Landings.corresponding.to.advice
  ) %>%
  mutate(
    Advice = as.numeric(gsub(",","", Advice)),
    Catches.corresponding.to.advice = as.numeric(gsub(",", "", Catches.corresponding.to.advice)),
    Landings.corresponding.to.advice = as.numeric(gsub(",", "", Landings.corresponding.to.advice))
  )

# read ices advice
advice <- getAdviceViewRecord() %>% tibble()
clean_advice <-
  advice %>%
  select(
    stockCode, adviceValue, wantedCatch,
    adviceType, adviceType, assessmentYear
  ) %>%
  mutate(
    adviceValue = as.numeric(adviceValue),
    assessmentYear = assessmentYear + 1
  ) %>%
  rename(
    ICES.code = "stockCode",
    Year = "assessmentYear"
  )



# get ices codes
sheet_fill %>%
  filter(Year > 2019) %>%
  select(ICES.code) %>%
  unique()

clean_advice %>%
  select(ICES.code) %>%
  arrange(ICES.code) %>%
  count(ICES.code)


# work on one stock for now
test <- function(code) {
  ices <-
    clean_advice %>%
    filter(ICES.code %in% strsplit(code, " [+] ")[[1]]) %>%
    group_by(Year) %>%
    summarise(
      adviceValue = sum(adviceValue, na.rm = FALSE),
      wantedCatch = sum(wantedCatch, na.rm = FALSE),
      adviceType = adviceType[1]
    ) %>%
    ungroup() %>%
    mutate(
      ICES.code = code
    ) %>%
    arrange(Year)

  if (nrow(ices) == 0) {
    return(NULL)
  }

  nat <-
    sheet_fill %>%
    filter(ICES.code == code) %>%
    arrange(Year)

  ices %>%
    left_join(nat)
}

code <- "ank.27.8c9a + mon.27.8c9a" # "anf.27.3a46" # "ane.27.8"# "ane.27.9a"
test(code)

codes <-
  sheet_fill %>%
  filter(Year > 2019) %>%
  select(ICES.code) %>%
  unique() %>%
  `[[`("ICES.code")

tests <-
  lapply(codes, test)
names(tests) <- codes

tests[1:5]

names(tests[sapply(tests, is.null)])

   
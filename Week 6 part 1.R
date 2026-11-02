library(readr)

a <- read.csv("owid-co2-data.csv")

raw <- a[a$year == 2022 & !is.na(a$iso_code) &
           a$iso_code != "",
         c("country", "population", "gdp", "co2",
           "co2_per_capita", "coal_co2", "energy_per_capita")]

raw$gdp_pc <- raw$gdp / raw$population

raw$coal_share <- ifelse(
  is.na(raw$coal_co2) | is.na(raw$co2) | raw$co2 <= 0,
  0,
  raw$coal_co2 / raw$co2
)

colSums(is.na(raw[, c("gdp", "population", "co2_per_capita")]))

r <- raw[
  !is.na(raw$gdp) &
    !is.na(raw$population) &
    !is.na(raw$co2_per_capita),
]

cat("Countries before:", nrow(raw), "\n")
cat("Countries after:", nrow(r), "\n")
cat("Countries removed:", nrow(raw) - nrow(r), "\n")

#TASK 2:
m2 <- lm(log(co2_per_capita) ~ log(gdp_pc), data = r)
summary(m2)
round(coef(m2)["log(gdp_pc)"], 3)

# A 1% increase in GDP per capita is associated with approximately a 1.045% increase in CO₂ emissions per capita.

# TASK 3: Add coal_share
m3 <- lm(log(co2_per_capita) ~ log(gdp_pc) + coal_share,
         data = r)

summary(m3)

# Compare GDP elasticities
round(c(
  without_coal = coef(m2)["log(gdp_pc)"],
  with_coal = coef(m3)["log(gdp_pc)"]
), 3)


# After controlling for coal share, the estimated GDP elasticity decreases, 
# suggesting that differences in coal dependence may explain part of the relationship between income and CO₂ emissions.


# TASK 4: Compare observations
c(
  rows_in_data = nrow(r),
  simple_model = nobs(m2),
  extended_model = nobs(m3)
)

cat("Rows excluded by extended model:",
    nrow(r) - nobs(m3), "\n")


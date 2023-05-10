library(tidyverse)
library(fpp3)

# Load data and select data since 1970
HadCRUT <- read_csv("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.csv")
HadCRUT <- HadCRUT %>%
        rename(time = "Time", Temperature = "Anomaly (deg C)") %>%
        select(time, Temperature)
HadCRUT$time <- as.Date(paste(HadCRUT$time, "-01", sep = ""))
HadCRUT <- HadCRUT %>%
        subset(time >= "1958-03-01")

co2 <- read_table("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt",
                  col_names = FALSE, skip = 58)

co2 <- co2 %>%
        select(X1, X2, X4) %>%
        rename(Year = X1, Month = X2, CO2 = X4) %>%
        mutate(time = make_date(year = Year, month = Month, day = 1 )) %>%
        mutate(CO2_rf = 5.35*log(CO2/280)) %>%
        select(time, CO2_rf) %>%
        rename(CO2 = CO2_rf)



# Solar irradiance data

irradiance <- read_table("https://www2.mps.mpg.de/projects/sun-climate/data/SATIRE-T_SATIRE-S_TSI_1850_20220923.txt", 
                         col_names = FALSE, skip = 22) %>%
        rename(time = X1, irradiance = X2) %>%
        select(time, irradiance) %>%
        mutate(daily = as.Date((time - 2396759), origin = as.Date("1850-01-01"))) %>%
        select(daily, irradiance) %>%
        mutate(time = floor_date(daily, "month")) %>%
        group_by(time) %>%
        summarize(irradiance = mean(irradiance, na.rm = TRUE)) %>%
        subset(time >= "1958-03-01") %>%
        rename(Solar =  irradiance)

# ENSO data

ENSO <- read_table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
ENSO$time <- seq.Date(from = as.Date("1950-01-01"), by = "month", length.out = nrow(ENSO))
ENSO <- subset(ENSO, time >= "1957-09-01") %>%
        rename(ENSO = ANOM) %>%
        select(time, ENSO)
ENSO$ENSO_lagged <- lag(ENSO$ENSO, 3)
ENSO <- ENSO %>%
        select(time, ENSO_lagged) %>%
        rename(ENSO = ENSO_lagged)

# Aerosols data

aerosols <- read_table("https://data.giss.nasa.gov/modelforce/strataer/tau.line_2012.12.txt",
                       skip = 3) %>%
        rename(time = "year/mon", Aerosols = global) %>%
        mutate_at(c("time", "Aerosols"), as.numeric) %>%
        select(time, Aerosols)

aerosols$time <- format(date_decimal(aerosols$time), "%Y-%m-01") %>%
        as.Date(aerosols$time, format = "%Y-%m-%d")
aerosols <- subset(aerosols, time >= "1955-10-01")

## Extrapolate out from 2012-09-01
length_out <- subset(ENSO, time >= "2012-10-01")
length_out <- length(length_out$time)

extrapolation <- data.frame(
        time = seq(from = as.Date("2012-10-01"), by = "1 month", length = length_out),
        Aerosols = "NaN"
)

aerosols <- rbind(aerosols, extrapolation)
aerosols$Aerosols <- as.numeric(aerosols$Aerosols)
aerosols <- imputeTS::na_interpolation(aerosols, option = "linear")
aerosols$Aerosols_lagged <- lag(aerosols$Aerosols, 18)
aerosols <- aerosols %>%
        select(time, Aerosols_lagged) %>%
        rename(Aerosols = Aerosols_lagged)
aerosols <- subset(aerosols, time >= "1958-03-01")

# Make data frame

df_list <- list(HadCRUT, co2, irradiance, ENSO, aerosols)
global_temp <- df_list %>%
        reduce(inner_join, by = "time") %>%
        mutate(date = decimal_date(time)) %>%
        mutate(time = yearmonth(time)) %>%
        as_tsibble(index = time)

# Create main model

fit_temp <- global_temp %>%
        model(TSLM(Temperature ~ CO2 + Solar + ENSO + Aerosols))

# Create separate models for each predictor variable in the main

CO2_fit <- global_temp %>%
        model(TSLM(CO2 ~ trend() + I(trend()^2) + season()))

irradiance_fit <- global_temp %>%
        model(TSLM(Solar ~ trend() + season() + fourier(period = 132, K = 66)))

aerosols_fit <- global_temp %>%
        model(TSLM(Aerosols ~ trend() + season()))

ENSO_fit <- global_temp %>%
        model(TSLM(ENSO ~ trend() + season() + fourier(period = 60, K = 30)))
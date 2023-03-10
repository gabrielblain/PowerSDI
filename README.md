# PowerSDI
Calculates the SPI and SPEI from NASA POWER data.

The PowerSDI is an R package capable of calculating the SPI and SPEI from NASA POWER data. This package is based on four friendly-use R-functions designed to calculate these two standardized drought indices (SDI) in scientific and operational modes. 
The functions ScientSDI.R, Accuracy.R and Reference.R may be used to assess, among other features, the ability of the SPI and SPEI frequency distributions to meet the normality assumption, and how well NASAPOWER estimates represent “real-world” data.
The OperatSDI.R function calculates both SPI and SPEI in an operational mode.

The PowerSDI adopts a basic time scale that splits each month into four subperiods: days 1 to 7, days 8 to 14, days 15 to 21, and days 22 to 28, 29, 30, or 31 depending on the month. 
For instance, if TS=4, the time scale corresponds to a moving window with a 1-month length that is calculated four times each month. If TS=48, the time scale corresponds to a moving window with a 12-month length that is calculated four times each month. This time scale is referred as to quart.month.

See Instructions for further details.
# Examples

# Function Scient.R

ScientSDI(lon=-47.01, lat=-22.87, start.date="01-01-1991", end.date="31-12-2022", TS=1)

# Function Reference.R

##Using the Hargreaves & Samani method

ref=Example_ref_HS 

Reference (ref=ref)

##Using the FAO-56 Penman-Monteith method

ref=Example_ref_PM 

Reference(ref=ref)

# Function Accuracy.R

obs_est=Example_obs_est

Accuracy(obs_est)

# Function Operat.R

lonlat=Example_lonlat

DistPar=Example_DistPar

OperatSDI(lonlat=lonlat, start.date="02-01-2013", end.date="31-12-2014", DistPar=DistPar, TS=1)

# PowerSDI
Calculates the Standardised Precipitation (SPI) and Standardised Precipitation-Evapotranspiration (SPEI) indices using NASA POWER data.

# Basic Description
The PowerSDI is an R package capable of calculating the SPI and SPEI using NASA POWER data.
The package is based on five R-functions designed to calculate these two standardised drought indices (SDI) in scientific and operational/routine modes.
The functions `ScientSDI()`, `Accuracy()`, `Reference()` and, `PlotData()` may be used to assess, among other features, the ability of the SPI and SPEI frequency distributions to meet the normality assumption and how well NASA POWER estimates represent “real-world” data.
The `OperatSDI()` function calculates both SPI and SPEI in an operational mode.

The PowerSDI adopts a basic time scale that splits each month into four subperiods: days 1 to 7, days 8 to 14, days 15 to 21, and days 22 to 28, 29, 30, or 31 depending on the month.
For instance, if TS=4, the time scale corresponds to a moving window with a 1-month length that is calculated four times each month.
If TS=48, the time scale corresponds to a moving window with a 12-month length that is calculated four times each month.
This time scale is referred to as "quart.month".

The package depends on R (>= 3.10) and R packages {[nasapower](https://docs.ropensci.org/nasapower/)} and {[lmom](https://cran.r-project.org/package=lmom)}.

# Installation

```r
devtools::install_github("gabrielblain/PowerSDI")
```

# Basic Instructions

## Function ScientSDI()
Helps the users to verify if the SPI and SPEI calculated from NASA POWER data meet the conceptual assumptions expected from standardised drought indices.

## Usage
```r
ScientSDI(
  lon,
  lat,
  start.date,
  end.date,
  distr = "GEV",
  TS = 4,
  Good = "Yes",
  sig.level = 0.95,
  RainUplim = NULL,
  RainLowlim = NULL,
  PEUplim = NULL,
  PELowlim = NULL
)
```

## Arguments

* lon: longitude in decimal degrees: (+) Eastern Hemisphere (-) Western Hemisphere.
* Lat: latitude in decimal degrees: (+) Northern Hemisphere (-) Southern Hemisphere.
* start.date: date at which the indices estimates should start. Format: YYYY-MM-DD".
* end.date: date at which the indices estimates should end. Format: YYYY-MM-DD".
* distr: A character variable ("GEV" or "GLO") defining the distribution to calculate the
* SPEI. Default is "GEV".
* TS: Time scale on the quart.month basis (integer values between 1 and 96). Default is 4.
* Good: A character variable ("Yes" or "No") to calculate or not the goodness-of-fit and  normality tests.
Default is "Yes".
* sig.level: A numeric variable (between 0.90 and 0.95) defining the significance level for parameter Good.
Default is "0.95".
* RainUplim: Optional.
Upper limit in millimetres from which rainfall values larger than it will be removed.
Default is `NULL`.
* RainLowlim: Optional.
Lower limit in millimetres from which rainfall values smaller than it will be removed.
Default is NULL.
* PEUplim: Optional.
Upper limit in millimetres from which evapotranspiration values larger than it will be removed.
Default is `NULL`.
* PELowlim: Optional.
Lower limit in millimetres from which evapotranspiration values smaller than it will be removed.
Default is `NULL`.

## Value

A list with data calculated at the time scale selected by the user. 
If `Good="Yes"`, this list includes:

  * SDI: The NASA-SPI, NASA-SPEI.HS and NASA-SPEI.PM. 
  * DistPar: The parameters of the distributions (gamma and GEV or GLO) used to calculate the indices. 
  * GoodFit: The Lilliefors and Anderson-Darling tests goodness-of-fit tests. 
  * Normality: The outcomes of the two normality checking procedures (Wu et al., 2007 and Stagge et., 2015).

If `Good="No"`, this list includes:
* SDI and 
* DistPar.

This function also presents other data (in millimetres) calculated from the NASA POWER project:

* Rainfall amounts (Rain).
* Potential evapotranspiration values estimated through the Hargreaves and Samani method (PEHS). 
* Potential evapotranspiration values estimated through the FAO-56 Penman-Monteith method (PEPM).
The difference between rainfall and potential evapotranspiration.

## Examples

```r
ScientSDI(
  lon = -47.3,
  lat = -22.67,
  start.date = "1991-01-01",
  end.date = "2022-12-31"
)
```

## Function Accuracy()

Verifies how well NASA-POWER data actually represent real-world/observed data.

## Usage

```r
Accuracy(obs_est, conf.int = "Yes", sig.level = 0.95)
```

## Arguments

* obs_est: A 2-column matrix. The reference or observed and the estimated or predicted data.
See ObsEst as example.
* conf.int: A character variable ("Yes" or "No") defining if the function must calculate confidence intervals.
Default is "Yes".
* sig.level: A numeric variable (between 0.90 and 0.95) defining the significance level for parameter the confidence intervals.
Default is 0.95.

## Value

* Absolute mean error (AME), Square root of the mean squared error (RMSE), Willmott's indices of agreement: original (dorig), Modified (dmod) and refined (dref), Pearson determination coefficient (R2).
If `conf.int="Yes"`, confidence intervals are calculated.

## Examples

```r
data("ObsEst")
Accuracy(obs_est = ObsEst, conf.int = "Yes", sig.level = 0.95)
```

## Function OperatSDI()

Generates routine operational NASA-SPI and NASA-SPEI estimates in several regions and at distinct time scales.

## Usage 

```r
OperatSDI(
  lon,
  lat,
  start.date,
  end.date,
  PEMethod = "HS",
  distr = "GEV",
  parms,
  TS = 4
)
```

## Arguments

* lon: longitude in decimal degrees.
* lat:  latitude in decimal degrees.
* start.date: Date at each the calculation must start (“YYYY-MM-DD").
* end.date: Date at each the calculation must end (“YYYY-MM-DD").
* PEMethod: A character variable ("HS" or "PM") defining the potential evapotranspiration method.
Default is "HS".
* distr: A character variable ("GEV" or "GLO") defining which distribution is used to calculate the SPEI.
Default is "GEV".
* parms: parameters required for calculating the SPI and SPEI. It is provided by the ScientSDI function (DistPar).
* TS: Time scale on the "quart.month" basis (integer values between 1 and 96).

## Value

A data frame with:

* Rainfall,
* potential evapotranspiration (PE),
* difference between rainfall and PE (in millimiters),
* the NASA-SPI and NASA_SPEI,
* and the SDI categories corresponding to each indices estimates.

## Examples

```r
data("DistPar")
OperatSDI(
  lon = -47.3,
  lat = -22.67,
  start.date = "2023-01-31",
  end.date = "2023-07-07",
  parms = DistPar
)
```

## Function PlotData()

Generates scatter plots of rainfall and accumulated potential evapotranspiration.

## Usage

```r
PlotData(lon, lat, start.date, end.date)
```

## Arguments
* lon: longitude in decimal degrees: (+) Eastern Hemisphere (-) Western Hemisphere.
* lat: latitude in decimal degrees: (+) Northern Hemisphere (-) Southern Hemisphere.
* start.date: date at which the indices estimates should start ("YYYY-MM-DD").
* end.date: date at which the indices estimates should end ("YYYY-MM-DD").

## Value

Scatter plots of:
* Rainfall and
* potential evapotranspiration accumulated at the 1-quart.month time scale.

## Examples

```r
PlotData(
  lon = -47.3,
  lat = -22.87,
  start.date = "2021-12-28",
  end.date = "2022-12-31"
)
```

## Function Reference()

Calculates both SPI and SPEI from daily data obtained from a ground weather station or any other reference source.

## Usage

```r
Reference(ref, distr = "GEV", PEMethod = "HS", TS = 4)
```

## Arguments

* ref: A data frame with the variables required for calculating the SDIs.
See `refHS` or `refPM` as examples.
* distr: A character variable ("GEV" or "GLO") defining which distribution is used to calculate the SPEI.
Default is "GEV".
* PEMethod: A character variable ("HS" or "PM") defining the potential evapotranspiration method.
Default is "HS".
* TS: Time scale on the quart.month" basis (integer values between 1 and 96).
Default is 4.

## Value

A data frame with:
* Rain,
*  potential evapotranspiration,
* difference between rainfall and potential evapotranspiration,
* SPI and SPEI calculated at the time scale selected by the user.

## Examples

```r
data("refHS")
Reference(ref = refHS, distr = "GEV, PEMethod = "HS", TS = 4)
```

## DistPar: parameters for calculating the SDIs. Provided by the ScientSDI function.

Contains parameters of the gamma and GEV distributions and the `Pr(Rain=0)`.

## Usage

```r
DistPar
```

## Format
* A 13-column matrix with 48 rows.
* lon: longitude in decimal degrees.
* lat: latitude in decimal degrees.
* quart.month: The quartile of each month.
* alfa.rain: Shape parameter of the gamma distribution.
* beta.rain: Scale parameter of the gamma distribution.
* probzero.rain: Probability of rain=0.
* loc.harg: Location parameter of the GEV distribution, PE calculated by HS method.
* sc.harg: Scale parameter of the GEV distribution, PE calculated by HS method.
* sh.harg: Shape parameter of the GEV distribution, PE calculated by HS method.
* loc.pm: Location parameter of the GEV distribution, PE calculated by PM method.
* sc.pm: Scale parameter of the GEV distribution, PE calculated by PM method.
* sh.pm: Shape parameter of the GEV distribution, PE calculated by PM method.
* TS: Time scale at which the SDIs will be calculated.

## Source

Generated by the `ScientSDI()` function using NASA POWER data.

## Examples

```r
data(DistPar)
```

## ObsEst: Example of the input required by the Accuracy function.

Contains pairs of reference and estimated data.

## Usage 

ObsEst

## Format

* A 2-column matrix with 1434 rows
* PE_obs: PE data from a reference weather station
* PE_est: PE data from the NASA POWER project

## Source

Generated by the PowerSDI package using data from the NASA POWER and Agronomic Institute.

## Examples

```r
data(ObsEst)
```

## refHS: Example of the input required by the Reference function

Contains data for calculating the SPI and SPEI.

## Usage 

```r
refHS
```

## Format
An 8-column matrix with 10950 rows and 8 variables

* YEAR: Year
* MM: Month
* DD: Day
* tmed: Daily average air temperature at 2 metres above the ground (oC)
* tmax: Daily maximum air temperature at 2 metres above the ground (oC)
* tmin: Daily minimum air temperature at 2 metres above the ground (oC)
* Ra: Daily top of the atmosphere radiation (MJ/m2/day)
* Rain: Daily rainfall amounts (mm)

## Source

Agronomic Institute and NASA POWER

## Examples

```r
data(refHS)
```

## refPM: Example of the input required by the Reference function

Contains data for calculating the SPI and SPEI.

## Usage 

```r
refPM
```

## Format
A 11-column matrix with 10958 rows and 11 variables

* YEAR: Year
* MM: Month
* DD: Day
* tmed: Daily average air temperature at 2 metres above the ground (oC)
* tmax: Daily maximum air temperature at 2 metres above the ground (oC)
* tmin: Daily minimum air temperature at 2 metres above the ground (oC)
* Ra: Daily top of the atmosphere radiation (MJ/m2/day)
* Rs: Daily global horizontal irradiance (MJ/m2/day)
* W: Daily average wind speed at 2 metres above the ground (m/s)
* RH: Daily average relative humidity at 2 metres above the ground (in percentage)
* Rain: Daily rainfall amounts (mm)

## Source

Agronomic Institute and NASA POWER

## Examples

```r
data(refPM)
```

## BugReports: 
<https://github.com/gabrielblain/PowerSDI/issues>

## License:

MIT

## Authors: 
Gabriel Constantino Blain, Graciela da Rocha Sobierajski, Leticia Lopes Martins, and Adam H Sparks
Maintainer: Gabriel Constantino Blain, <gabriel.blain@sp.gov.br>

## Acknowledgments:
The package uses data obtained from the NASA Langley Research Center (LaRC) POWER Project funded through the NASA Earth Science/Applied Science Program.
The POWER project provides data for support several activities including agriculture and energy. 
The authors greatly appreciate this initiative.

## References
Allen, R.G.; Pereira, L.S.; Raes, D.; Smith, M. Crop evapotranspiration. In Guidelines for Computing Crop Water Requirements. Irrigation and Drainage Paper 56; FAO: Rome, Italy, 1998; p. 300.

Blain, G. C., 2014. Revisiting the critical values of the Lilliefors test: towards the correct agrometeorological use of the Kolmogorov- Smirnov framework. Bragantia, 73, 192-202. http://dx.doi.org/10.1590/brag.2014.015

Hargreaves, G.H.; Samani, Z.A. 1985.Reference crop evapotranspiration from temperature. Appl. Eng. Agric,1, 96–99.

Mckee, T. B., Doesken, N.J. and Kleist, J., 1993. The relationship of drought frequency and duration to time scales. In: 8th Conference on Applied Climatology. Boston, MA: American Meteorological Society, 179–184.

Stagge, J. H., Tallaksen, L. M., Gudmundsson, L., Van Loon, A. F. and Stahl, K., 2015. Candidate distribution for climatological drought indices (SPI and SPEI). International Journal of Climatology, 35(13), 4027–4040. https://doi.org/10.1002/joc.4267

Package ‘lmom', Version 2.9, Author J. R. M. Hosking. https://CRAN.R-project.org/package=lmom

Package ‘nasapower', Version 4.0.10, Author Adam H. Sparks et al., https://CRAN.R-project.org/package=nasapower

Wu, H., Svoboda, M. D., Hayes, M. J., Wilhite, D. A. and Wen, F., 2007. Appropriate application of the standardised precipitation index in arid locations and dry seasons. International Journal of Climatology, 27(1), 65–79. https://doi.org/10.1002/joc.1371.

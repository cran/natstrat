% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nh0506_3groups}
\alias{nh0506_3groups}
\title{Homocysteine and smoking example data with multiple control groups}
\format{
A data frame with 4457 rows and 11 variables:
\describe{
  \item{SEQN}{NHANES identification number.}
  \item{z}{smoking status treatment factor: 0 = never smoker, 1 = some smoking, 2 = daily smoker.}
  \item{sex}{factor with levels "Male" and "Female".}
  \item{age}{age in years, 20-85, with 85 recorded for everyone >= 85 years.}
  \item{race}{factor with levels "Mexican American",
  "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", and
  "Other Race - Including Multi-Racial".}
  \item{education}{factor with levels "< Grade 9",
  "9-11th grade", "High school grad/GED", "Some college or AA degree",
  "College graduate or above".}
  \item{povertyr}{ratio of family income to the poverty level,
  capped at 5 times poverty, has missing entries.}
  \item{bmi}{BMI (body mass index), has missing entries.}
  \item{cigsperday30}{cigarettes smoked per day, 0 for never smokers.}
  \item{cotinine}{blood cotinine level, a biomarker of recent exposure to tobacco.}
  \item{homocysteine}{homocysteine level.}
}
}
\source{
\url{https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/Default.aspx?BeginYear=2005}
}
\usage{
nh0506_3groups
}
\description{
NHANES 2005-2006 data on smoking and homocysteine levels in adults,
comparing daily smokers to never smokers and occasional smokers.
}
\details{
The code used to generate this data is documented
in the source version of this package under `data-raw/`.
This data is composed of adults aged at least 20 years.
Individuals who have smoked at least 100 cigarettes
but do not now smoke at least 10 cigarettes daily are
excluded. Individuals with missing homocysteine
values, cotinine values, or smoking information are excluded.
After filtering for all these criteria,
five individuals with unknown education remain and are also excluded.
Missing values remain in the poverty ratio and bmi covariates.
}
\examples{
data('nh0506_3groups')
}
\keyword{datasets}

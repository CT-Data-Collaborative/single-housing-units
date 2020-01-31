library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(data.table)
library(reshape2)

##################################################################
#
# Processing Script for Single Housing Units
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

data <- data.table()

for (file in list.files(path_to_raw)) {
    # parse year from file name
    year <- str_extract_all(file, "([0-9]{2})")[[1]][1]
    year <- as.numeric(year) + 2000

    raw <- read.acs(file.path(getwd(), "raw", file), endyear = year, span = 5)

    # raw[1,c(11,13,14)]
    # 11 - Number Total Housing Units
    # 13 - Number 1 unit detached
    # 14 - Percent 1 Unit Detached

    fips <- data.table(Id = geography(raw)$Id)
    fips[,c("Level", "FIPS") := do.call(Map, c(f=c, strsplit(Id, "US", fixed = T)))]

    estimates <- data.table(
        fips$FIPS,
        paste(year-4, year, sep="-"),
        "Housing Units",
        estimate(raw[, 11]),
        estimate(raw[, 13]),
        estimate(raw[, 14])
    )
    setnames(estimates, c("FIPS", "Year", "Variable", "Number:Total", "Number:Detached", "Percent:Detached"))

    moe <- data.table(
        fips$FIPS,
        paste(year-4, year, sep="-"),
        "Margins of Error",
        standard.error(raw[, 11]) * 1.645,
        standard.error(raw[, 13]) * 1.645,
        standard.error(raw[, 14]) * 1.645
    )
    setnames(moe, c("FIPS", "Year", "Variable", "Number:Total", "Number:Detached", "Percent:Detached"))

    data <- rbind(
        data,
        estimates,
        moe
    )
}
remove(moe, estimates, fips, raw, year, file)

data <- melt(
    data,
    id.vars = c("FIPS", "Year", "Variable"),
    variable.name = "Var",
    variable.factor = F,
    value.name = "Value",
    value.factor = F
)

data[,c("Measure Type", "Unit Type") := do.call(Map, c(f=c, strsplit(Var, ":", fixed = T)))]

data[, Var := NULL]

# Get town and county fips in one set, then merge into data
# get towns
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])
towns <- as.data.table(towns)

# new column name -- will match counties and eventually final dataset
setnames(towns, "Town", "Town/County")

#counties
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
counties <- (county_fips_dp$data[[1]])
counties <- as.data.table(counties)

# we don't need two copies of CT fips
counties <- counties[County != "Connecticut"]
# new column name -- will match counties and eventually final dataset
setnames(counties, "County", "Town/County")
#rbind these two together
fips <- rbind(towns, counties)
# set keys
setkey(fips, FIPS)
setkey(data, FIPS)

# Merge!
data <- fips[data]

data <- data[!is.na(data$`Town/County`),]

# cleanup
remove(towns, counties, fips)

# reorder columns
setcolorder(data, c(1, 2, 3, 7, 6, 4, 5))

# reorder data
data <- data[order(`Town/County`, Year, -`Unit Type`, `Measure Type`, Variable)]

write.table(
    data,
    file.path("data", "single-housing-units-2018.csv"),
    sep = ",",
    row.names=F,
    na = "-9999"
)

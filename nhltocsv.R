# Get better dataset from nhl.com
# Don't run this unless you need to

url <- "https://api.nhle.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=true&reportName=teamsummary&sort=[{%22property%22:%22points%22,%22direction%22:%22DESC%22},{%22property%22:%22wins%22,%22direction%22:%22DESC%22}]&cayenneExp=leagueId=133%20and%20gameDate%3E=%222007-08-01%22%20and%20gameDate%3C=%222019-04-07%2023:59:59%22%20and%20gameTypeId=2"
data <- rjson::fromJSON(file = url)[[1]]

x <- as.data.frame(data[[1]])

convert <- lapply(data, as.data.frame)

final <- dplyr::bind_rows(convert)

readr::write_csv(final, "nhlgamedata.csv")

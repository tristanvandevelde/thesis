library(ggplot2)
library(dplyr)
library(tidyr)

energy_df <- read.csv(url("https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv
"))

unique(energy_df$iso_code)

eu_iso_alpha3 <- c(
"AUT",
"BEL",
"BGR",
"HRV",
"CYP",
"CZE",
"DNK",
"EST",
"FIN",
"FRA",
"DEU",
"GRC",
"HUN",
"IRL",
"ITA",
"LVA",
"LTU",
"LUX",
"MLT",
"NLD",
"POL",
"PRT",
"ROU",
"SVK",
"SVN",
"ESP",
"SWE",
"GBR")

energy_eu_df <- subset(energy_df, iso_code %in% eu_iso_alpha3)
energy_eu_df[is.na(energy_eu_df)] <- 0


eu_aggregated <- aggregate(cbind(energy_eu_df$energy_generation, 
                                 energy_eu_df$biofuel_electricity,
                                 energy_eu_df$coal_electricity,
                                 energy_eu_df$fossil_electricity,
                                 energy_eu_df$gas_electricity,
                                 energy_eu_df$hydro_electricity,
                                 energy_eu_df$nuclear_electricity,
                                 energy_eu_df$oil_electricity,
                                 energy_eu_df$other_renewable_electricity,
                                 energy_eu_df$solar_electricity,
                                 energy_eu_df$wind_electricity), 
                           by=list(Category=energy_eu_df$year), FUN=sum)
colnames(eu_aggregated) <- c("energy_generation", 
                             "biofuel_electricity",
                             "coal_electricity",
                             "fossil_electricity",
                             "gas_electricity",
                             "hydro_electricity",
                             "nuclear_electricity",
                             "oil_electricity",
                             "other_renewable_electricity",
                             "solar_electricity",
                             "wind_electricity")

eu_aggregated <- gather(eu_aggregated, "year", "production", 2:11)

colnames(eu_aggregated) <- c("year", "type", "production")

eu_aggregated <- eu_aggregated[eu_aggregated$year>1962,]

ggplot(eu_aggregated, 
       aes(x=year, y=production, fill=type)) +
        geom_area()

# idea: lineplot with absolute production
# 100% chart for relative division
  
1958
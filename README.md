# rbutterflyexplorers

An R package to access butterfly observation data from [Butterfly Explorers](https://www.butterflyexplorers.com) — a curated database of geotagged butterfly observations by Sajan K.C. and Anisha Sapkota.

Data are returned in [Darwin Core](https://dwc.tdwg.org/) format as tibbles or `sf` spatial objects, ready for use in biodiversity analyses, species distribution modeling, and mapping workflows.

## Installation

```r
# From r-universe (recommended)
install.packages("rbutterflyexplorers", repos = "https://sajankc143.r-universe.dev")

# Or from GitHub
devtools::install_github("sajankc143/rbutterflyexplorers")
```

## Usage

```r
library(rbutterflyexplorers)

# Get all observations
obs <- be_observations()

# Filter by species (partial match)
obs <- be_observations(species = "Agathymus indecisa")

# Filter by genus
obs <- be_observations(species = "Megathymus")

# Filter by country
obs <- be_observations(country = "Costa Rica")

# Filter by family
obs <- be_observations(family = "Hesperiidae")

# Filter by state/province
obs <- be_observations(state_province = "Florida")

# Filter by date range
obs <- be_observations(date_from = "2023-01-01", date_to = "2024-12-31")

# Return as sf spatial object
obs_sf <- be_observations(species = "Agathymus", as_sf = TRUE)

# Get full species list with observation counts
spp <- be_species()

# Look up a single observation by ID
obs <- be_observation_id("A3BX9K2M")
```

## Mapping example

```r
library(rbutterflyexplorers)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(sf)

# Fetch Agathymus and Megathymus records
obs_sf <- dplyr::bind_rows(
  be_observations(species = "Agathymus"),
  be_observations(species = "Megathymus")
) |>
  dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) |>
  dplyr::mutate(genus = stringr::str_extract(scientificName, "^\\S+")) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

basemap <- ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(name %in% c("Mexico","Guatemala","Belize","Honduras",
    "El Salvador","Nicaragua","Costa Rica","Panama","United States","Canada"))

ggplot() +
  geom_sf(data = basemap, fill = "grey90", color = "grey60", linewidth = 0.3) +
  geom_sf(data = obs_sf, aes(color = genus), size = 2, alpha = 0.8) +
  coord_sf(xlim = c(-125, -75), ylim = c(6, 50)) +
  scale_color_manual(values = c("Agathymus" = "#e74c3c", "Megathymus" = "#2980b9")) +
  theme_minimal() +
  labs(title = expression(italic("Agathymus") ~ "and" ~ italic("Megathymus")),
       color = "Genus")
```

## Data fields

| Field | Description |
|---|---|
| `occurrenceID` | Unique identifier (BE:ButterflyExplorers:HASH) |
| `scientificName` | Scientific name |
| `vernacularName` | Common name |
| `family` | Butterfly family |
| `eventDate` | Observation date |
| `country` | Country |
| `stateProvince` | State or province |
| `locality` | Full location string |
| `decimalLatitude` | Latitude (WGS84) |
| `decimalLongitude` | Longitude (WGS84) |
| `associatedMedia` | Full image URL |
| `references` | Observation page URL |

## Citation

K.C., Sajan and A. Sapkota (Chief Editors). 2025. Butterfly Explorers, v. 3.3 (www.butterflyexplorers.com).

## License

Data: CC BY-NC 4.0. Package code: MIT.

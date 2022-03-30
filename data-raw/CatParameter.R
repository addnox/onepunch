
param_Occupancy <- data.table::data.table(Occupancy = c("Commercial", "Industrial", "Residential"), p = c(.5, .45, .05))

param_Coverage <- data.table::data.table(
  Coverage = c("Building", "Content", "BI"),
  Commercial = c(.63, .36, .01),
  Industrial = c(.44, .55, .01),
  Residential = c(.9, .1, 0)
)

param_BuildingHeight <- data.table::data.table(
  BuildingHeight = c("1", "2-3", "4-7", "8-11", "12-14", "over 14", "unknown"),
  Commercial = c(0, 0, 0.1, 0.2, 0.3, 0.4, 0),
  Industrial = c(0.3, 0.6, 0.1, 0, 0, 0, 0),
  Residential = c(0, 0, 0, 0.1, 0.4, 0.5, 0)
)

param_ConstructionType <- data.table::data.table(
  ConstructionType = c("Reinforced Concrete","Reinforced Masonry","Unreinforced Masonry","Steel Frame","Wood Frame","Unknown"),
  Commercial = c(0.85, 0.10, 0, 0.05, 0, 0),
  Industrial = c(0.50, 0.25, 0.05, 0.20, 0, 0),
  Residential = c(0.80, 0.20, 0, 0, 0, 0)
)

param_YearBuilt <- data.table::data.table(
  YearBuilt = c("Pre 1970","1970s","1980s","1990s","Post 2000","Unknown"),
  Commercial = c(0, 0, 0.05, 0.05, 0.9, 0),
  Industrial = c(0, 0, 0.05, 0.05, 0.9, 0),
  Residential = c(0, 0, 0.05, 0.05, 0.9, 0)
)

param_LossTerm <- data.table::data.table(
  LossTerm = c("Deductible_Percentage", "Deductible_Amount", "Sublimit_Percentage"),
  Commercial = c(.15, 90000, .8),
  Industrial = c(.15, 90000, .8),
  Residential = c(.15, 90000, .8)
)

param_long <- list(
  Occupancy = param_Occupancy,
  Coverage  = data.table::melt(param_Coverage, id.vars = 1, variable.name = "Occupancy", value.name = "p", variable.factor = FALSE),
  ConstructionType = data.table::melt(param_ConstructionType, id.vars = 1, variable.name = "Occupancy", value.name = "p", variable.factor = FALSE),
  BuildingHeight = data.table::melt(param_BuildingHeight, id.vars = 1, variable.name = "Occupancy", value.name = "p", variable.factor = FALSE),
  YearBuilt = data.table::melt(param_YearBuilt, id.vars = 1, variable.name = "Occupancy", value.name = "p", variable.factor = FALSE),
  LossTerm = param_LossTerm
)

param_wide <- list(
  Occupancy = param_Occupancy,
  Coverage  = param_Coverage,
  ConstructionType = param_ConstructionType,
  BuildingHeight = param_BuildingHeight,
  YearBuilt = param_YearBuilt,
  LossTerm = param_LossTerm
)

usethis::use_data(param_long, param_wide, overwrite = TRUE)


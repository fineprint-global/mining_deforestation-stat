
formulas <- list(
  # "f_base" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine +
  #   elevation + slope +
  #   pop_2000 + area_forest_2000 +
  #   dist_road +
  #   dist_waterway +
  #   distance_protected_area +
  #   distance_cropland_2000 +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_bool" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_bool + distance_mine +
  #   elevation + slope +
  #   pop_2000 + area_forest_2000 +
  #   dist_road_bool + dist_road +
  #   dist_waterway_bool + dist_waterway +
  #   distance_protected_area_bool + distance_protected_area +
  #   distance_cropland_2000_bool + distance_cropland_2000 +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log +
  #   dist_waterway_log +
  #   distance_protected_area_log +
  #   distance_cropland_2000_log +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_bool_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_bool + distance_mine_log +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_bool + dist_road_log +
  #   dist_waterway_bool + dist_waterway_log +
  #   distance_protected_area_bool + distance_protected_area_log +
  #   distance_cropland_2000_bool + distance_cropland_2000_log +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_no_pop_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   elevation + slope +
  #   area_forest_2000_log +
  #   dist_road_log +
  #   dist_waterway_log +
  #   distance_protected_area_log +
  #   distance_cropland_2000_log +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_no_pa_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log +
  #   dist_waterway_log +
  #   distance_cropland_2000_log +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_no_esa_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log +
  #   dist_waterway_log +
  #   distance_protected_area_log +
  #   distance_cropland_2000_log +
  #   soilgrid_grouped,
  # "f_esa_group" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine +
  #   elevation + slope +
  #   pop_2000 + area_forest_2000 +
  #   dist_road +
  #   dist_waterway +
  #   distance_protected_area +
  #   distance_cropland_2000 +
  #   soilgrid_grouped + esa_cci_2000_grouped,
  # "f_base_decay" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_decay +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_decay +
  #   dist_waterway_decay +
  #   distance_protected_area_decay +
  #   distance_cropland_2000_decay +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_log_interactions" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
  #   dist_road_log + I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
  #   dist_waterway_log +
  #   distance_protected_area_log +
  #   distance_cropland_2000_log +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_decay_interactions" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_decay +
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
  #   dist_road_decay + I(dist_road_decay * pop_2000_log) + I(dist_road_decay * distance_cropland_2000_decay) +
  #   dist_waterway_decay +
  #   distance_protected_area_decay +
  #   distance_cropland_2000_decay +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_log_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_bool_log_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_bool + distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_bool + dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
  #   dist_waterway_bool + dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
  #   distance_protected_area_bool + distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
  #   distance_cropland_2000_bool + distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_base_log_interactions_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km10) + I(distance_mine_log * distance_mine_km20)+ I(distance_mine_log * distance_mine_km50) +
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
  #   I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
  #   dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) + I(dist_road_log * dist_road_km20)+ I(dist_road_log * dist_road_km50) +
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) + I(dist_waterway_log * dist_waterway_km20)+ I(dist_waterway_log * dist_waterway_km50) +
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) + I(distance_protected_area_log * distance_protected_area_km20)+ I(distance_protected_area_log * distance_protected_area_km50) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) + I(distance_cropland_2000_log * distance_cropland_2000_km20)+ I(distance_cropland_2000_log * distance_cropland_2000_km50) +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_sub_5_20_log_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20) +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km20)+
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km20)+
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km20)+
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) +  I(distance_cropland_2000_log * distance_cropland_2000_km20)+
  #   soilgrid_grouped + esa_cci_2000,
  # "f_sub_5_20_log_interactions_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20)+
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
  #   I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
  #   dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km20)+
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) +  I(dist_waterway_log * dist_waterway_km20)+
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) +  I(distance_protected_area_log * distance_protected_area_km20)+
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5)  + I(distance_cropland_2000_log * distance_cropland_2000_km20)+
  #   soilgrid_grouped + esa_cci_2000,
  # "f_proposal_log_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20) +
  #   elevation + slope +
  #   pop_2000_log + area_forest_2000_log +
  #   dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km50)+
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km10) +
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km20) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
  #   soilgrid_grouped + esa_cci_2000,
  # "f_proposal_log_interactions_nonlins" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log + I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km20)+
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log + I(pop_2000_log * area_forest_2000_log) +
  #   I(dist_road_log * pop_2000_log) + I(dist_road_log * distance_cropland_2000_log) +
  #   dist_road_log + I(dist_road_log * dist_road_km5) +  I(dist_road_log * dist_road_km50)+
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km10) +
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km20) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km10)  +
    # soilgrid_grouped + esa_cci_2000,
  # "f_vary_no-road_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km25) +
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log +
  #   I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
  #   # Distance + Distance * 5/10/25/50 dummies
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
  #   I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
  #   distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) +
  #   I(distance_protected_area_log * distance_protected_area_km25) + I(distance_protected_area_log * distance_protected_area_km50) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
  #   I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
  #   soilgrid_grouped + esa_cci_2000 + biomes_2017,
  # "f_vary_no-pa_log" = area_accumulated_forest_loss_2019_log ~
  #   distance_mine_log +
  #   I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km25) +
  #   elevation + slope + I(elevation * slope) +
  #   pop_2000_log + area_forest_2000_log +
  #   I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
  #   # Distance + Distance * 5/10/25/50 dummies
  #   dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) +
  #   I(dist_road_log * dist_road_km25) + I(dist_road_log * dist_road_km50) +
  #   dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
  #   I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
  #   distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
  #   I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
  #   soilgrid_grouped + esa_cci_2000 + biomes_2017,
  "f_vary_minesize" = area_accumulated_forest_loss_2019_log ~
    distance_mine_log +
    I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km25) +
    min_area_1degree +
    elevation + slope + I(elevation * slope) +
    pop_2000_log + area_forest_2000_log +
    I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
    # Distance + Distance * 5/10/25/50 dummies
    dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) +
    I(dist_road_log * dist_road_km25) + I(dist_road_log * dist_road_km50) +
    dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
    I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
    distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) +
    I(distance_protected_area_log * distance_protected_area_km25) + I(distance_protected_area_log * distance_protected_area_km50) +
    distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
    I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
    soilgrid_grouped + esa_cci_2000 + biomes_2017,
    "f_vary_log" = area_accumulated_forest_loss_2019_log ~
    distance_mine_log +
    I(distance_mine_log * distance_mine_km5) + I(distance_mine_log * distance_mine_km25) +
    elevation + slope + I(elevation * slope) +
    pop_2000_log + area_forest_2000_log +
    I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
    # Distance + Distance * 5/10/25/50 dummies
    dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) +
    I(dist_road_log * dist_road_km25) + I(dist_road_log * dist_road_km50) +
    dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
    I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
    distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) +
    I(distance_protected_area_log * distance_protected_area_km25) + I(distance_protected_area_log * distance_protected_area_km50) +
    distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
    I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
    soilgrid_grouped + esa_cci_2000 + biomes_2017,
  "f_base_log_minesize" = area_accumulated_forest_loss_2019_log ~
    distance_mine_log +
    min_area_1degree +
    elevation + slope + I(elevation * slope) +
    pop_2000_log + area_forest_2000_log +
    I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
    # Distance + Distance * 5/10/25/50 dummies
    dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) +
    I(dist_road_log * dist_road_km25) + I(dist_road_log * dist_road_km50) +
    dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
    I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
    distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) +
    I(distance_protected_area_log * distance_protected_area_km25) + I(distance_protected_area_log * distance_protected_area_km50) +
    distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
    I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
    soilgrid_grouped + esa_cci_2000 + biomes_2017,
  "f_dummy_minesize" = area_accumulated_forest_loss_2019_log ~
    # > 50 is baseline
    km_inside +
    km1_10 +
    km10_20 +
    km20_30 +
    km30_40 +
    km40_50 +
    min_area_1degree +
    elevation + slope + I(elevation * slope) +
    pop_2000_log + area_forest_2000_log +
    I(pop_2000_log * area_forest_2000_log) + I(pop_2000_log * dist_road_log) +
    # Distance + Distance * 5/10/25/50 dummies
    dist_road_log + I(dist_road_log * dist_road_km5) + I(dist_road_log * dist_road_km10) +
    I(dist_road_log * dist_road_km25) + I(dist_road_log * dist_road_km50) +
    dist_waterway_log + I(dist_waterway_log * dist_waterway_km5) + I(dist_waterway_log * dist_waterway_km10) +
    I(dist_waterway_log * dist_waterway_km25) + I(dist_waterway_log * dist_waterway_km50) +
    distance_protected_area_log + I(distance_protected_area_log * distance_protected_area_km5) + I(distance_protected_area_log * distance_protected_area_km10) +
    I(distance_protected_area_log * distance_protected_area_km25) + I(distance_protected_area_log * distance_protected_area_km50) +
    distance_cropland_2000_log + I(distance_cropland_2000_log * distance_cropland_2000_km5) + I(distance_cropland_2000_log * distance_cropland_2000_km10) +
    I(distance_cropland_2000_log * distance_cropland_2000_km25) + I(distance_cropland_2000_log * distance_cropland_2000_km50) +
    soilgrid_grouped + esa_cci_2000 + biomes_2017
)

gcs_auth_file(file = "testdata/epicsa_token.json")

### mw_workshops
annual_rainfall_summaries("mw_workshops", "Nkhotakota")
annual_rainfall_summaries("mw_workshops", "Kasungu")

annual_temperature_summaries("mw_workshops", "Nkhotakota")
annual_temperature_summaries("mw_workshops", "Kasungu")

monthly_temperature_summaries("mw_workshops", "Nkhotakota")
monthly_temperature_summaries("mw_workshops", "Kasungu")

crop_success_probabilities("mw_workshops", "Nkhotakota")
crop_success_probabilities("mw_workshops", "Kasungu")

season_start_probabilities("mw_workshops", "Nkhotakota")
season_start_probabilities("mw_workshops", "Kasungu")

### zm_workshops
# zambia_eastern is giving all of the zambia stuff! Chipata etc etc.
# chipata is giving Nkhotakota

annual_rainfall_summaries("zm_workshops", "zambia_eastern")
annual_rainfall_summaries("zm_workshops", "chipata")

annual_temperature_summaries("zm_workshops", "zambia_eastern")
annual_temperature_summaries("zm_workshops", "chipata")

monthly_temperature_summaries("zm_workshops", "zambia_eastern")
monthly_temperature_summaries("zm_workshops", "chipata")

crop_success_probabilities("zm_workshops", "zambia_eastern")
crop_success_probabilities("zm_workshops", "chipata")

season_start_probabilities("zm_workshops", "zambia_eastern")
season_start_probabilities("zm_workshops", "chipata")

### zm_test
station_metadata("zm_test")
# all these stations are actually just the zambia stations from before, from what I can tell.
annual_rainfall_summaries("zm_test", "zambia_eastern")
annual_rainfall_summaries("zm_test", "zambia_eastern_4")
annual_rainfall_summaries("zm_test", "zmd_eastern_8")

annual_temperature_summaries("zm_test", "zambia_eastern")
annual_temperature_summaries("zm_test", "zambia_eastern_4")
annual_temperature_summaries("zm_test", "zmd_eastern_8")

monthly_temperature_summaries("zm_test", "zambia_eastern")
monthly_temperature_summaries("zm_test", "zambia_eastern_4")
monthly_temperature_summaries("zm_test", "zmd_eastern_8")

crop_success_probabilities("zm_test", "zambia_eastern")
crop_success_probabilities("zm_test", "zambia_eastern_4")
crop_success_probabilities("zm_test", "zmd_eastern_8")

season_start_probabilities("zm_test", "zambia_eastern")
season_start_probabilities("zm_test", "zambia_eastern_4")
season_start_probabilities("zm_test", "zmd_eastern_8")

### ml_test
station_metadata("ml_test")
annual_rainfall_summaries("ml_test", "Kasungu")
annual_temperature_summaries("ml_test", "Kasungu")
monthly_temperature_summaries("ml_test", "Kasungu")
crop_success_probabilities("ml_test", "Kasungu")
season_start_probabilities("ml_test", "Kasungu")
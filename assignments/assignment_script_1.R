
source("setup.R")

# load the buoys, the coastline and the database of covariate data
buoys = gom_buoys()
coast = read_coastline()
db = brickman_database()

buoys = buoys |> 
  filter(id == "M01")  # not3 the double '=='


db = DB |>
  dplyr::filter(scenario == "RCP45", 
                year == 2055,
                interval == "mon")
x = read_brickman(db)
x
# read the covariates
covars = read_brickman(db)


x = extract_brickman(covars, buoys, form = "wide")



ggplot(data = x,
       mapping = aes(x = month, y = SST)) +
  geom_point() + 
  labs(title = "Temp difference at buoy M01")


x = x |>
  mutate(month = factor(month, levels = month.abb))

# now plot!
ggplot(data = x,
       mapping = aes(x = month, y = SST)) +
  geom_point() + 
  labs(y = "SST (C)", 
       title = "RCP4.5 2055 SST at buoy M01")



# bonus... save the image
ggsave("images/N01_dT.png")


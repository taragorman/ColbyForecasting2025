
read_observations = function(scientificname = "Mola mola",
                             minimum_year = 1970, 
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname, ...)
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  return(x)
}
--
  source("setup.R")
SPECIES = "Thunnus thynnus"
```

# Observations

Next is to read in the observations you have already downloaded for that species.

```{r read_obs}
obs = read_obis(SPECIES)
obs
```

The print out of the table only shows the first 10 rows (so your screen doesn't get filled up), and it tells you how many records you have.  A simple way to keep track of the number of records is to use the `dim()` functions which returns the number of rows and number of columns. I'm going to save the outout so we can compare after all of the filtering.
                                                         
                                                         ```{r dim}
                                                         dim_start = dim(obs)
                                                         dim_start
                                                         ```
                                                         
                                                         ## basisOfRecord
                                                         
                                                         Next we should examine the `basisOfRecord` variable to get an understanding of how these observations were made.
                                                         
                                                         ```{r basisOfRecord}
                                                         obs |> count(basisOfRecord)
                                                         ```
                                                         
                                                         If you are using a different species you may have different values for `basisOfRecord`.  Let's take a closer look at the complete records for one from each group.  

```{r browse}
human = obs |>
  filter(basisOfRecord == "HumanObservation") |>
  slice(1) |>
  browse_obis()

preserved = obs |>
  filter(basisOfRecord == "PreservedSpecimen") |>
  slice(1) |>
  browse_obis()

checklist = obs |>
  filter(basisOfRecord == "NomenclaturalChecklist") |>
  slice(1) |>
  browse_obis()

occurrence = obs |>
  filter(basisOfRecord == "Occurrence") |>
  slice(1) |>
  browse_obis()
```

Next let's think about what our minimum requirements might be in oirder to build a model. To answer that we need to think about our environmental covariates in the Brickman data](https://github.com/BigelowLab/ColbyForecasting2025/wiki/Brickman). That data has dimensions of x (longitude), y (latitude) and month.  In order to match obseravtions with that data, our observations must be complete in those three variables.  Let's take a look at a summary of the observations which will indicate the number of elements missing in each variable.

```{r summary_obs}
summary(obs)
```

## `eventDate`

For *Mola mola* there are some rows where `eventDate` is `NA`.  We need to filter those. The filter function looks for a vector of TRUE/FALSE values - one for each row.  In our case, we test the `eventDate` column to see if it is `NA`, but then we reverse the TRUE/FALSE logical with the preceding `!` (pronounded "bang!"). This we retain only the rows where `eventDate is not `NA`, and then we print the summary again.

```{r obs_filter_date}
obs = obs |>
  filter(!is.na(eventDate))
summary(obs)
```

## `individualCount`

That's better, but we still have  315 `NA` values for `individualCount`.  Let's look at at least one record of those in detail; filter out one, and browse it.

```{r obs_indcount}
obs |>
  filter(is.na(individualCount)) |>
  slice(1) |>
  browse_obis()
```

Eeek!  It's a carcas that washed up on shore!  We checked a number of others, and they are all carcases.  Is that a presence?  Is that what we model are modeling?  If not then we should filer those out.

```{r obs_filter_countless_dead}
obs = obs |>
  filter(!is.na(individualCount))
summary(obs)
```

Well now one has to wonder about a single observation of 25 animals. Let's check that out.

```{r obs_indcount_110}
obs |>
  filter(individualCount == 110) |>
  browse_obis()
```

OK, that seems legitmate. And it is possible, *Mola mola* can congregate for feeding, mating and possibly for karaoke parties.

## `year`

We know that the "current" climate scenario for the Brickman model data define "current" as the 1982-2013 window.  It's just an average, and if you have values from 1970 to the current year, you probably are safe in including them.  But do your observations fall into those years?  Let's make a plot of the counts per year, with dashed lines shown the Brickman "current" cliamtology period.

```{r plot_year}
ggplot(data = obs,
       mapping = aes(x = year)) + 
  geom_bar() + 
  geom_vline(xintercept = c(1990, 2023), linetype = "dashed") + 
  labs(title = "Counts per year")
```
For this species, it seem like it is only the record from 1932 that might be a stretch, so let's filter that out by rejecting records before 1970. This time, instead of asking for a sumamry, we'll print the dimensions (rows, columns) of the table.

```{r filter_earlier}
obs = obs |>
  filter(year >= 1970)
dim(obs)
```

That's still a lot of records.  Now let's check out the distribution across the months of the year.

## `month` 

We will be making models and predictions for each month of the for the 4 future projection climates. Species and observers do show some seasonality, but it that seasonality so extreme that it might be impossible to model some months because of sparse data?  Let's make a plot of the counts per month.

```{r plot_month}
ggplot(data = obs,
       mapping = aes(x = month)) + 
  geom_bar() + 
  labs(title = "Counts per month")
```

Oh, rats!  By default `ggplot` plots in alpha-numeric order, which scrambles our month order.  To fix that we have to convert the `month` in a factor type while specifying the order of the factors, and we'll use the `mutate()` function to help us.

```{r month_ordered}
obs = obs |>
  mutate(month = factor(month, levels = month.abb))

ggplot(data = obs,
       mapping = aes(x = month)) + 
  geom_bar() + 
  labs(title = "Counts per month")
```

That's better! So, it may be the for *Mola mola* we might not be able to successfully model in the cold winter months. That's good to keep in mind.

## `geometry` 

Last, but certainly not least, we should consider the possibility that some observations might be on shore.  It happens!  We already know that some records included fish that were washed up on shore.  It's possible someone mis-keyed the longitude or latitude when entering the vaklues into the database.  It's alos possible that some observations fall just outside the areas where the Brickman data has values.  To look for these points, we'll load the Brickman mask (defines land vs water. Well, really it defines data vs no-data), and use that for further filtering.

We need to load the Brickman database, and then filter it for the static variable called "mask".

```{r mask}
db = brickman_database() |>
  filter(scenario == "STATIC", var == "mask")
mask = read_brickman(db, add_depth = FALSE)
mask
```

Let's see what our mask looks like with the observations drizzled on top. Because the mask only has values of 1 (data) or `NA` (no-data).  You'll note that we only want to plot the locations of the observations, so we strip `obs` of everyhting except its geometery.

```{r plot_mask}
plot(mask, breaks = "equal", axes = TRUE, reset = FALSE)
plot(st_geometry(obs), pch = ".", add = TRUE)
```
Maybe with proper with squinting we can see some that faal into no-data areas.  The sure-fire way to tell is to extract the mask values at the point locations.

```{r mask_extract}
hitOrMiss = extract_brickman(mask, obs)
hitOrMiss
```

OK, let's tally the "value" variable.

```{r tally_masked}
count(hitOrMiss, value)
```
Ooooo, 33 records in `obs` don't line up with values in the mask (or in any Brickman data).  We should filter those out; we'll do so with a `filter()`. Note that we a "reaching" into the `hitOrMiss` table to access the `value` column when we use this `hitOrMiss$value`.  Let's figure out how many records we have dropped with all of this filtering.

```{r filter_the_misses}
obs = obs |>
  filter(!is.na(hitOrMiss$value))
dim_end = dim(obs)

dropped_records = dim_start[1] - dim_end[1]
dropped_records
```


```
source("setup.R")
obs = read_observations()
```
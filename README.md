# climate-can
climate and productivity 

Scratch: [html preview](https://jasonpekos.quarto.pub/climate-and-productivity-ebde/).


# Note on Targets

Targets is a cool package. It allows you to specify a DAG of tasks, and then run them in parallel. It also allows you to cache the results of tasks, so that if you change one task, it will only re-run the tasks that depend on it.

The keynotes for running this package are:

1. When you open your R session, run `library(targets)`.
2. When you want to run the entire pipeline, run `tar_make()`.
3. When you want to create a single object, run `tar_make(object_name)`.
4. To call a targets object, use `tar_read(object_name)`.
5. To see the DAG, run `tar_visnetwork()`.
6. To load the results of a target *into memory*, run `tar_load(object_name)`.

# What's new?
Added ObservableJS cells to the [scratch document](https://jasonpekos.quarto.pub/climate-and-productivity-ebde/), which has been renamed 
and moved to a subdirectory.

This allows you to upload a geojson file (you want the raw file for this), and then examine the census geography at a deeper level. 

But this only works locally for now. I need to go to sleep, I'll deal with it later. 

Added the raw productivity files to the targets script. The files can be loaded more efficiently as they have done [here](https://people.math.carleton.ca/~davecampbell/Case_Study_2023/Dataset-details.html) but I am not yet comfortable using targets. I will look at it again tomorrow. The directory issue is resolved but the scratch.qmd is not running fully in my local PC, still struggling with it. Like when I render the file, I don't get [this](https://jasonpekos.quarto.pub/climate-and-productivity-ebde/). Need to fix it!

The variable information for climate data are available [here](https://climate.weather.gc.ca/glossary_e.html#n).

- Abhiroop

# What's next?

1. Re-write most of the targets other than loading data.
2. Fix the `add_enclosing_polydat()` function so that only calculates the enclosing polygon for any given weather station *once*.
3. Create a little shiny app that allows me to find the station names by clicking on a map.


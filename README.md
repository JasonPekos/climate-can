# climate-can
climate and productivity 

Scratch: [html preview](https://htmlpreview.github.io/?https://github.com/JasonPekos/climate-can/blob/main/Scratch.html).


# Note on Targets

Targets is a cool package. It allows you to specify a DAG of tasks, and then run them in parallel. It also allows you to cache the results of tasks, so that if you change one task, it will only re-run the tasks that depend on it.

The keynotes for running this package are:

1. When you open your R session, run `library(targets)`.
2. When you want to run the entire pipeline, run `tar_make()`.
3. When you want to create a single object, run `tar_make(object_name)`.
4. To call a targets object, use `tar_read(object_name)`.
5. To see the DAG, run `tar_visnetwork()`.
6. To load the results of a target *into memory* , run `tar_load(object_name)`.


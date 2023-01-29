source("init_field_reader.R")
GlobalMarkovParameters <- setRefClass("GlobalMarkovParameters",
                                      fields = c("state_components",
                                                 "action_components",
                                                 "reward_components",
                                                 "num_epochs"),
                                      contains = "InitFieldReader")
# read whatever init fields are included in csv
# if object is created that is not a subclass, all init fields should be included
# object of subclass can calculate other fields (e.g., state components) 
# with subclass-specific methods

GlobalMarkovParameters$methods(
  initialize = function(init_fields_csv = NA, ...) {
    callSuper(...)
    if (!(is.na(init_fields_csv))) read_init_fields_csv(init_fields_csv)
  }
)

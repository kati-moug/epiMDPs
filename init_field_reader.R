# takes a csv with field columns (can be different lengths) and
# initializes fields

# also includes functions to update field lists and data.frames

source("utility_functions.R")
InitFieldReader <- setRefClass("InitFieldReader", 
                               fields = c("init_fields_list",
                                          "unassigned_init_field_list"))
InitFieldReader$methods(
  initialize = function(...){
    unassigned_init_field_list <<- c()
    callSuper(...)
  },
  read_init_fields_csv = function(init_fields_csv){
     init_fields_list <<- read_csv_to_list_and_drop_na(init_fields_csv)
     init_fields_from_list()
     if (length(unassigned_init_field_list)>0){
       print("Warning! The following csv columns are not fields in object:")
       print(unassigned_init_field_list)
     }
  },
  init_fields_from_list = function(){
     field_list_names <- names(init_fields_list)
     for (name in field_list_names) check_if_field_and_assign(name)
  },
  check_if_field_and_assign = function(name){
    field_names <- get_field_names()
    if (name %in% field_names) field(name, init_fields_list[[name]])
    else unassigned_init_field_list <<- append(unassigned_init_field_list, name)
  },
  get_field_names = function(include_init = TRUE){
    field_names <- names(getRefClass()$fields())
    init_names <- c("init_fields_list", "unassigned_init_field_list")
    if (!(include_init)) field_names <- field_names[! field_names %in% init_names]
    field_names
  },
  get_list_fields = function(include_init = FALSE) {
    field_names <- get_field_names(include_init)
    field_list <- list()
    for (name in field_names) field_list[[name]] <- field(name)
    field_list
  },
  update_field_data_frame = function(field_name, row, col, entry) {
    df <- field(field_name)
    df[row, col] <- entry
    field(field_name, df)
  },
  update_field_list = function(field_name, list_name, entry) {
    lst <- field(field_name)
    lst[[list_name]] <- entry
    field(field_name, lst)
  },
  append_vec_in_field_list = function(field_name, vec_name, item) {
    list <- field(field_name)
    list[[vec_name]] <- append(list[[vec_name]], item)
    field(field_name, list)
  }
)

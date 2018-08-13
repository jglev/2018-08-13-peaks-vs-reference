## Load and transform colorant reference data, and find matching peaks
## 2018-07-03

## Load packages -----------------------------------------------------------

library(forcats)
library(ggplot2)
library(dplyr)
library(knitr)
library(magrittr)
library(purrr)
library(pracma)
library(readr)
library(tools)

# Load datasets -----------------------------------------------------------

combine_separate_data_files <- function(
  directory = getwd(),
  filename_pattern = '.*csv',
  recursive_search = FALSE
) {
  
  ## Get the data files in the directory:
  files_in_directory <- list.files(
    path = directory,
    pattern = filename_pattern,
    full.names = TRUE,
    recursive = recursive_search  
    ## Set this to TRUE to go into the example_map_files
    ## directory and load the files from it.
  )
  
  # Get the filenames without extensions (i.e., get the map names):
  data_names <- tools::file_path_sans_ext(
    files_in_directory %>% basename()
  )
  
  # Load the data, and combine it into a single tibble:
  dataset <- files_in_directory %>% 
    purrr::map_df(read.csv, header = FALSE, .id = "dataset_name") %>% 
    dplyr::mutate(
      dataset_name = dataset_name %>% as.integer() %>% sort() %>% factor()
    )
  
  # Check the number of elements per map
  # dataset %>% group_by(dataset_name) %>% summarize(n = n())
  
  # Overwrite the dataset_name column with the original map names:
  dataset$dataset_name %<>% 
    forcats::lvls_revalue(data_names)
  
  # Again, check the number of elements per map
  # dataset %>% group_by(dataset_name) %>% summarize(n = n())
  
  dataset
}

dataset <- combine_separate_data_files(
  directory = 'data_test_set',
  filename_pattern = '*.dpt'
)

dataset %<>% 
  dplyr::rename(
    wavenumber = V1,
    intensity = V2
  ) %>% 
  tibble::as.tibble()

# View(dataset)
# dataset

# Analysis plan -----------------------------------------------------------

# - [ ] Annotate which peaks do and don't match, having looked at the closest match
#   - [ ] For a given dataset, find the closest match using peaks
#     - [ ] For a given dataset, we want one number for each other dataset, representing the number of matching peaks with that dataset.
#     - [ ] Write a command to determine matching peaks
#       - A thing is matching if there is a peak within +/- 10 wavenumbers
#     - [O] Get the top 10 peaks (by intensity) for each dataset
#       - [O] Find all peaks for each dataset

# Find all peaks for each dataset -----------------------------------------

dataset_with_peaks <- dataset %>% 
  dplyr::pull(dataset_name) %>% 
  levels() %>% 
  purrr::map_dfr(
    function(name_of_the_dataset) {
      peaks_for_this_dataset <- dataset %>% 
        dplyr::filter(dataset_name == name_of_the_dataset) %>% 
        dplyr::pull(intensity) %>% 
        ## Threshold in findpeaks() below is the minimum local peak height,
        ## vs. minpeakheight, which is the minimum absolute peak height.
        pracma::findpeaks(threshold = 0.05) %>% 
        tibble::as.tibble() %>% 
        dplyr::rename(
          peak_height = V1,
          peak_index = V2,
          peak_start = V3,
          peak_end = V4
        ) %>% 
        dplyr::mutate(
          # dataset_name = name_of_the_dataset,
          peak_index = as.integer(peak_index),
          peak_start = as.integer(peak_start),
          peak_end = as.integer(peak_end)
        ) # %>% 
        # dplyr::arrange(-peak_height) %>% 
        # dplyr::slice(1:10)
        
        dataset %>% 
          dplyr::filter(dataset_name == name_of_the_dataset) %>% 
          tibble::rowid_to_column('row_number') %>% 
          dplyr::left_join(
            peaks_for_this_dataset,
            by = c('row_number' = 'peak_index')
          )
    }
  ) # %>% 
  # dplyr::select(-dataset_name.x) %>% 
  # dplyr::mutate(dataset_name = dataset_name.y) #  %>% 
  # dplyr::select(-dataset_name.y)

# dataset_with_peaks %>% View()

reference_dataset <- dataset_with_peaks %>% 
  na.omit()

# reference_dataset %>% View()

## Show the selected peaks for a given dataset:
show_dataset_peaks <- function(dataset_names) {
  dataset_with_peaks %>%
    dplyr::filter(`dataset_name` %in% dataset_names) %>%
    ggplot(aes(x = wavenumber, y = intensity, color = dataset_name)) +
      geom_line() +
      geom_point(
        aes(x = wavenumber, y = peak_height, color = 'red', size = 5)
      ) +
      scale_x_reverse() # +
      # theme(legend.position = "none")  ## Remove all legends
}

# show_dataset_peaks('CFD_Chalk40mg_R_FTIR.0')
# show_dataset_peaks('CFD_AlizarineCrimsonLight_K_FTIR.0')
show_dataset_peaks(
  c('a.0', 'b.0')
)

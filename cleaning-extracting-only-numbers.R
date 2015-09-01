# Haruo Kamioka # github: ekamioka

######################### Cleaning the train, test and cookie data ###################################

### The following commands will load and and remove the characters (text words) from the data sets

clean_undescore <- function(x) sapply(x, FUN = function(x) as.numeric(gsub(".*._",'', x)))

# Loading and cleaning the testing set
dev_train_basic <- read.csv('../input/dev_train_basic.csv', header = T)
dev_train_basic <- clean_undescore(dev_train_basic)

# Loading and cleaning the training set
dev_test_basic <- read.csv('../input/dev_test_basic.csv', header = T)
dev_test_basic <- clean_undescore(dev_test_basic)

# Loading and cleaning the cookie set
cookie_all_basic <- read.csv('../input/cookie_all_basic.csv', header = T)
cookie_all_basic <- clean_undescore(cookie_all_basic)

# train_cookie_merge <- merge(dev_train_basic, cookie_all_basic, by = 'drawbridge_handle', all.x = T)

######################################################################################################
# https://www.kaggle.com/benhamner/icdm-2015-drawbridge-cross-device-connections/exploring-the-drawbridge-data

### IP and property .csv files are not good. The following function was provided by the organizers ###

library(readr)
library(xtable)

read_bad_csv <- function(file_name, bad_col=3, n_max=-1) {
  f_in <- file(file_name)
  lines <- readLines(f_in, n=n_max)
  close(f_in)
  temp_csv_1 <- tempfile()
  f_out_1 <- file(temp_csv_1, "w")
  writeLines(gsub("\\{|\\}", '"', lines), f_out_1)
  close(f_out_1)
  data <- read_csv(temp_csv_1, col_names=FALSE)
  temp_csv_2 <- tempfile()
  f_out_2 <- file(temp_csv_2, "w")
  for (i in 1:nrow(data)) {
    bad_lines <- strsplit(substr(data[i,bad_col], 2, nchar(data[i,bad_col])-1), "\\),\\(")[[1]]
    if (bad_col==1) {
      lines <- paste(bad_lines,
                     paste0(as.character(data[i,2:ncol(data)]), collapse=","),
                     sep=",")
    } else if (bad_col<ncol(data)) {
      lines <- paste(paste0(as.character(data[i,1:bad_col-1]), collapse=","),
                     bad_lines,
                     paste0(as.character(data[i,bad_col+1:ncol(data)]), collapse=","),
                     sep=",")
    } else {
      lines <- paste(paste0(as.character(data[i,1:ncol(data)-1]), collapse=","),
                     bad_lines,
                     sep=",")
    }
    writeLines(lines, f_out_2)
  }
  close(f_out_2)
  return(read_csv(temp_csv_2))
}

head.table <- function(table) {
  html <- print(xtable(head(table, n=20)), type="html", print.results=FALSE)
  cat(paste0("<div style='width:800; overflow:auto; border-width: 2'>", html, "</div>"))
}
######################################################################################################

# Below we use the read_bad_function() to read the bad csv files

# Loading and cleaning the id-ip set
id_all_ip <- read_bad_csv('../input/id_all_ip.csv',  bad_col=3, n_max=100)
id_all_ip$device_or_cookie_id <- as.numeric(gsub(".*._",'', id_all_ip$device_or_cookie_id))
id_all_ip$ip <- as.numeric(gsub("ip",'', id_all_ip$ip))

# Loading and cleaning the id-property set
id_all_property <- read_bad_csv('../input/id_all_property.csv',  bad_col=3, n_max=100)
id_all_property <- clean_undescore(id_all_property)

# Loading and cleaning the ip-aggregated set
ipagg_all <- read_csv('../input/ipagg_all.csv', n_max=100)
ipagg_all$ip_address <- as.numeric(gsub("ip",'', ipagg_all$ip_address))

# Loading and cleaning the property_categories set
property_category <- read_bad_csv('../input/property_category.csv',  bad_col=2)
property_category <- clean_undescore(property_category)

library(cyphr)
library(openssl)

data_dir <- file.path("./data")


#Claudia
#only run this part once
key_path <- file.path("~/.ssh")

cyphr::data_request_access(data_dir, key_path)


#if this doesn't work then you don't have an ssh key set up and we need to create one
#ssh_keygen(path = key_path)


# cyphr::data_admin_init("./data", path_user = key_path)
# req <- cyphr::data_admin_list_requests(data_dir)


#run this before loading the app on your local machine.
key_path <- file.path("~/.ssh")
key <- cyphr::data_key("data", path_user = key_path)



#Dan
key_path <- "C:/Users/Dan/.ssh"
key <- cyphr::data_key("data", path_user = key_path)




#Authorise new users
# cyphr::data_admin_authorise(data_dir, yes = TRUE, path_user = "C:/Users/Dan/.ssh")
# cyphr::data_admin_list_keys(data_dir)


#only need to run this encrypt files when updating survey data after updating
cyphr::encrypt(saveRDS(
    cyphr::decrypt(
      readRDS("./data/youth2019.rds"),
      cyphr::key_sodium(sha256(charToRaw(askpass("Please enter password:"))))),
  "./data/youth2019.rds"),
  key)
# 
# 
cyphr::encrypt(saveRDS(
  readRDS(
    "./data/sample_weights2019.rds"),
  "./data/sample_weights2019.rds"),
  key)


cyphr::encrypt(saveRDS(
  readRDS(
    "./data/ECschoolData2019.rds"),
  "./data/ECschoolData2019.rds"),
  key)


cyphr::encrypt(saveRDS(
  cyphr::decrypt(
    readRDS("./data/youth2019.rds"),
    cyphr::key_sodium(sha256(charToRaw(askpass("Please enter password:"))))),
  "./data/youth2019.rds"),
  key)



cyphr::decrypt(readRDS(paste0(dirShinydata, "sample_weights2019.rds")), cyphr::data_key(dirShinydata))



cyphr::encrypt(saveRDS(PASS, "data/password_analysis.rds"), cyphr::key_sodium(openssl::sha256(keyring::key_get_raw("youth19_secret"))))

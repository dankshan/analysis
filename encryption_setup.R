library(cyphr)

data_dir <- file.path("./data")


#Claudia
#only run this part once
key_path <- "~/.ssh"
ssh_keygen(path = key_path)
cyphr::data_admin_init("./data", path_user = key_path)
req <- cyphr::data_admin_list_requests(data_dir)


#run this before loading the app on your local machine.
key <- cyphr::data_key("data", path_user = key_path)



#Dan
key_path <- "C:/Users/Dan/.ssh"
key <- cyphr::data_key("data", path_user = key_path)




#Authorise new users
# cyphr::data_admin_authorise(data_dir, yes = TRUE, path_user = "C:/Users/Dan/.ssh")
# cyphr::data_admin_list_keys(data_dir)


#only need to run this encrypt files when updating survey data after updating
# cyphr::encrypt(saveRDS(
#     cyphr::decrypt(
#       readRDS("./data/youth2019.rds"),
#       cyphr::key_sodium(sha256(charToRaw(askpass("Please enter password:"))))),
#   "./data/youth2019.rds"),
#   key)
# 
# 
# cyphr::encrypt(saveRDS(
#   readRDS(
#     "./data/sample_weights2019.rds"),
#   "./data/sample_weights2019.rds"),
#   key)
# 
# 
# cyphr::encrypt(saveRDS(
#   readRDS(
#     "./data/ECschoolData2019.rds"),
#   "./data/ECschoolData2019.rds"),
#   key)
# 

#### Loading Packages ####
# install.packages("aws.s3")
library(aws.s3)
library(config)
library(data.table)


#### Loading credentials ####
kp <- config::get(config = "AWS", value = "S3")

Sys.setenv("AWS_ACCESS_KEY_ID" = kp$AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = kp$AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = kp$AWS_DEFAULT_REGION)
# Sys.unsetenv("AWS_ACCESS_KEY_ID",
#              "AWS_SECRET_ACCESS_KEY",
#              "AWS_DEFAULT_REGION")

#### Getting bucket list ####
bucketlist(key = kp$AWS_ACCESS_KEY_ID, secret = kp$AWS_SECRET_ACCESS_KEY)
bucket_list() # After executing Sys.setenv()
# get_bucket(bucket = '1000genomes')
get_bucket(bucket = "algotrading-vicmacbec", 
           key = kp$AWS_ACCESS_KEY_ID, 
           secret = kp$AWS_SECRET_ACCESS_KEY,
           region = kp$AWS_DEFAULT_REGION) %>% rbindlist

get_object(object = "Orders/allOrders_year_20220421.csv",
           bucket = "algotrading-vicmacbec",
           key = kp$AWS_ACCESS_KEY_ID,
           secret = kp$AWS_SECRET_ACCESS_KEY,
           region = kp$AWS_DEFAULT_REGION) -> object2
load(rawConnection(object2))
load(charToRaw(object2))


#### Uploading and deleting file to a bucket ####
put_object(file = "~/Drive/Codigos/AlgoTrading/binancePairsBUSD.R", 
           object = "binancePairsBUSD.R", 
           bucket = "algotrading-vicmacbec",
           key = kp$AWS_ACCESS_KEY_ID, 
           secret = kp$AWS_SECRET_ACCESS_KEY,
           region = kp$AWS_DEFAULT_REGION
           )
# Refresh and check on:
# https://s3.console.aws.amazon.com/s3/buckets/algotrading-vicmacbec?region=us-east-2&tab=objects
delete_object(object = "binancePairsBUSD.R", 
              bucket = "algotrading-vicmacbec",
              key = kp$AWS_ACCESS_KEY_ID, 
              secret = kp$AWS_SECRET_ACCESS_KEY,
              region = kp$AWS_DEFAULT_REGION
              )
# Refresh and check on:
# https://s3.console.aws.amazon.com/s3/buckets/algotrading-vicmacbec?region=us-east-2&tab=objects


#### Reading a file from a bucket ####
system.time(
  s3read_using(FUN = fread,
               object = "s3://algotrading-vicmacbec/Orders/allOrders_year_20220421.csv") -> orders
)

system.time(
  s3read_using(FUN = data.table::fread, 
               object = "s3://algotrading-vicmacbec/Trades/allData_year_4h_20220421.csv") -> trades
)


#### Creating a deleting a bucket ####
b <- put_bucket("myexamplebucket-951",
                key = kp$AWS_ACCESS_KEY_ID, 
                secret = kp$AWS_SECRET_ACCESS_KEY,
                region = kp$AWS_DEFAULT_REGION
                )
delete_bucket("myexamplebucket-951",
              key = kp$AWS_ACCESS_KEY_ID, 
              secret = kp$AWS_SECRET_ACCESS_KEY,
              region = kp$AWS_DEFAULT_REGION
              )

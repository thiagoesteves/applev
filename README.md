[![Build Status](https://secure.travis-ci.org/thiagoesteves/applev.svg?branch=main)](http://travis-ci.org/thiagoesteves/applev)

# This application validates apple receipts with App Store. [How to validate](https://developer.apple.com/documentation/storekit/in-app_purchase/validating_receipts_with_the_app_store)

## Getting started ##
You need to clone the repository and download rebar/rebar3 (if it's not already available in your path).
```
git clone https://github.com/thiagoesteves/applev.git
cd applev
```
To compile and run
```
make
```

### How to Use

The application has two modes of operation: Sync and Async. If you decide to use the sync mode, when validation function is called, the function will be blocked until it finishes (or timeout). If async mode is selected, you must pass the PID of the destination process in order to receive the result. See the examples, where it is used the default sandbox receipt:
```erlang
1> applev:validate().
{ok,#{<<"bid">> => <<"com.es.Purchaser">>,<<"bvrs">> => <<"3">>,
      <<"item_id">> => <<"1061557484">>,
      <<"original_purchase_date">> =>
          <<"2015-11-23 16:32:27 Etc/GMT">>,
      <<"original_purchase_date_ms">> => <<"1448296347138">>,
      <<"original_purchase_date_pst">> =>
          <<"2015-11-23 08:32:27 America/Los_Angeles">>,
      <<"original_transaction_id">> => <<"1000000181492557">>,
      <<"product_id">> => <<"purchaser.consumableFeature">>,
      <<"purchase_date">> => <<"2015-11-23 16:32:27 Etc/GMT">>,
      <<"purchase_date_ms">> => <<"1448296347138">>,
      <<"purchase_date_pst">> =>
          <<"2015-11-23 08:32:27 America/Los_Angeles">>,
      <<"quantity">> => <<"1">>,
      <<"transaction_id">> => <<"1000000181492557">>,
      <<"unique_identifier">> =>
          <<"e47425df7fb6ab8c3c7e5730c3a7eb5b6563823d">>,
      <<"unique_vendor_identifier">> =>
          <<"5C275B5A-3E66-4026-9523-845B225755FC">>}}
2> applev:validate_async().
{ok,<0.247.0>}
3> flush().
Shell got {apple_receipt_return,{ok,#{<<"bid">> => <<"com.es.Purchaser">>,
                                      <<"bvrs">> => <<"3">>,
                                      <<"item_id">> => <<"1061557484">>,
                                      <<"original_purchase_date">> =>
                                          <<"2015-11-23 16:32:27 Etc/GMT">>,
                                      <<"original_purchase_date_ms">> =>
                                          <<"1448296347138">>,
                                      <<"original_purchase_date_pst">> =>
                                          <<"2015-11-23 08:32:27 America/Los_Angeles">>,
                                      <<"original_transaction_id">> =>
                                          <<"1000000181492557">>,
                                      <<"product_id">> =>
                                          <<"purchaser.consumableFeature">>,
                                      <<"purchase_date">> =>
                                          <<"2015-11-23 16:32:27 Etc/GMT">>,
                                      <<"purchase_date_ms">> =>
                                          <<"1448296347138">>,
                                      <<"purchase_date_pst">> =>
                                          <<"2015-11-23 08:32:27 America/Los_Angeles">>,
                                      <<"quantity">> => <<"1">>,
                                      <<"transaction_id">> =>
                                          <<"1000000181492557">>,
                                      <<"unique_identifier">> =>
                                          <<"e47425df7fb6ab8c3c7e5730c3a7eb5b6563823d">>,
                                      <<"unique_vendor_identifier">> =>
                                          <<"5C275B5A-3E66-4026-9523-845B225755FC">>}}}
ok
```

### Unit Test and coverage

```
make test
```
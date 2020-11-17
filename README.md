[![Build Status](https://secure.travis-ci.org/thiagoesteves/applev.svg?branch=main)](http://travis-ci.org/thiagoesteves/applev)
[![Coverage Status](https://coveralls.io/repos/github/thiagoesteves/applev/badge.svg?branch=main)](https://coveralls.io/github/thiagoesteves/applev?branch=main)
[![Erlant/OTP Release](https://img.shields.io/badge/Erlang-OTP--23.0-green.svg)](https://github.com/erlang/otp/releases/tag/OTP-23.0)

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

### How to use the application

The application has two modes of operation: Sync and Async. If you decide to use the sync mode, when validation function is called, the function will be blocked until it finishes (or timeout). If async mode is selected, you must pass the PID (or process name) of the destination process in order to receive the result and any argument to identify the validated receipt. See the examples:
```erlang
1> applev:validate(<<"Your Receipt Here">>).
{ok,#{<<"bid">> => <<"com.es.Purchaser">>,<<"bvrs">> => <<"3">>,
      ...
      <<"unique_vendor_identifier">> =>
          <<"5C275B5A-3E66-4026-9523-845B225755FC">>}}
2> applev:validate_async(self(), <<"Your Receipt Here">>, [arg1, arg2]).
{ok,<0.247.0>}
3> flush().
Shell got {apple_receipt_return,{ok,#{<<"bid">> => <<"com.es.Purchaser">>,
                                      ...
                                      <<"unique_vendor_identifier">> =>
                                          <<"5C275B5A-3E66-4026-9523-845B225755FC">>},
                                          [arg1, arg2]} }
ok
```

### How to test the application using the sandbox receipt example

In order to test the connectivity, there are test functions using sandbox receipt
```erlang
1> applev:validate().
{ok,#{<<"bid">> => <<"com.es.Purchaser">>,<<"bvrs">> => <<"3">>,
      ...
      <<"unique_vendor_identifier">> =>
          <<"5C275B5A-3E66-4026-9523-845B225755FC">>}}
2> applev:validate_async().
{ok,<0.247.0>}
3> flush().
Shell got {apple_receipt_return,{ok,#{<<"bid">> => <<"com.es.Purchaser">>,
                                      ...
                                      <<"unique_vendor_identifier">> =>
                                          <<"5C275B5A-3E66-4026-9523-845B225755FC">>}
                                      none}}
ok
```

### Unit Test and coverage

The following command will invoke common test and coverage.

```
make test
```
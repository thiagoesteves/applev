%%%-------------------------------------------------------------------
%%% Created : 22 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Definitions for the whole application
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(applev).
-define(applev, true).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% See https://developer.apple.com/documentation/storekit/in-app_purchase/validating_receipts_with_the_app_store
-define(APPLE_PRODUCTION,   "https://buy.itunes.apple.com/verifyReceipt").
-define(APPLE_SAND_BOX,     "https://sandbox.itunes.apple.com/verifyReceipt").
-define(APPLE_CONTENT_TYPE, "application/json").

%% See https://developer.apple.com/documentation/appstorereceipts/status
-define(APPLE_STATUS_OK,           0).
-define(APPLE_STATUS_SANDBOX,      21007). % Redirect to sandbox
-define(APPLE_STATUS_ERR_SERVER_1, 21005). % Server error, try again
-define(APPLE_STATUS_ERR_SERVER_2, 21009). % Server error, try again
-define(APPLE_STATUS_ERR_EXPIRED,  21006). % Valid, but expired
-define(APPLE_STATUS_ERR_CORRUPTED,21002). % Receipt corrupted

-type apple_return() ::   undefined            % validation in process
                        | receipt_corrupted    % 21002
                        | receipt_expired      % 21006
                        | max_number_of_tries. % max number of validation tries
                                               % 21005, 21009 or http error

% Validation message to be received when the validation is done
-define(APPLEV_MSG(Status, Receipt, Args), {apple_receipt_return, {Status, Receipt, Args}}).

% file with deafult sand-box receipt
-define(APPLEV_SANDBOX_RECEIPT, "doc/receipt.txt").

%% Http defines
-define(HTTP_OK, {"HTTP/1.1",200,"OK"}).

%% Default timeout to wait for a receipt validation
-define(DEFAULT_TIMEOUT_FOR_VALIDATION, 3000).

%% Sandbox receipt example
-define(APPLE_SANDBOX_RECEIPT,
<<"eyJzaWduYXR1cmUiID0gIkFoaHEwbmFxaG01ci8wSUNuYk9hVThCeVBLNkRja2ZsanRCMDNnZUh
   4dk0ybEVjVkdqK2NVM1lnWGZ0RkZCZ2lFYkR1NGdoYXFVWFRqRzlpc25Zeit6VWFhTXRUZDJ6Yn
   NLbFhIMitzYm1ZaC9tY2M2eWt3dVFkaFZsOWZKYkxNaFVXWHNTUlIxVlVjQlE4TkxjVGg5ZHNXO
   TVTREcxdG9DQk45cWM0L01CZlVBQUFEVnpDQ0ExTXdnZ0k3b0FNQ0FRSUNDQnVwNCtQQWhtL0xN
   QTBHQ1NxR1NJYjNEUUVCQlFVQU1IOHhDekFKQmdOVkJBWVRBbFZUTVJNd0VRWURWUVFLREFwQmN
   IQnNaU0JKYm1NdU1TWXdKQVlEVlFRTERCMUJjSEJzWlNCRFpYSjBhV1pwWTJGMGFXOXVJRUYxZE
   dodmNtbDBlVEV6TURFR0ExVUVBd3dxUVhCd2JHVWdhVlIxYm1WeklGTjBiM0psSUVObGNuUnBab
   WxqWVhScGIyNGdRWFYwYUc5eWFYUjVNQjRYRFRFME1EWXdOekF3TURJeU1Wb1hEVEUyTURVeE9E
   RTRNekV6TUZvd1pERWpNQ0VHQTFVRUF3d2FVSFZ5WTJoaGMyVlNaV05sYVhCMFEyVnlkR2xtYVd
   OaGRHVXhHekFaQmdOVkJBc01Fa0Z3Y0d4bElHbFVkVzVsY3lCVGRHOXlaVEVUTUJFR0ExVUVDZ3
   dLUVhCd2JHVWdTVzVqTGpFTE1Ba0dBMVVFQmhNQ1ZWTXdnWjh3RFFZSktvWklodmNOQVFFQkJRQ
   URnWTBBTUlHSkFvR0JBTW1URXVMZ2ppbUx3Ukp4eTFvRWYwZXNVTkRWRUllNndEc25uYWwxNGhO
   QnQxdjE5NVg2bjkzWU83Z2kzb3JQU3V4OUQ1NTRTa01wK1NheWc4NGxUYzM2MlV0bVlMcFduYjM
   0bnF5R3g5S0JWVHk1T0dWNGxqRTFPd0Mrb1RuUk0rUUxSQ21lTnhNYlBaaFM0N1QrZVp0REVoVk
   I5dXNrMytKTTJDb2dmd283QWdNQkFBR2pjakJ3TUIwR0ExVWREZ1FXQkJTSmFFZU51cTlEZjZaZ
   k42OEZlK0kydTIyc3NEQU1CZ05WSFJNQkFmOEVBakFBTUI4R0ExVWRJd1FZTUJhQUZEWWQ2T0tk
   Z3RJQkdMVXlhdzdYUXd1UldFTTZNQTRHQTFVZER3RUIvd1FFQXdJSGdEQVFCZ29xaGtpRzkyTmt
   CZ1VCQkFJRkFEQU5CZ2txaGtpRzl3MEJBUVVGQUFPQ0FRRUFlYUpWMlU1MXJ4ZmNxQUFlNUMyL2
   ZFVzhLVWw0aU80bE11dGE3TjZYelAxcFpJejFOa2tDdElJd2V5Tmo1VVJZSEsrSGpSS1NVOVJMZ
   3VObDBua2Z4cU9iaU1ja3dSdWRLU3E2OU5JbnJaeUNENjZSNEs3N25iOWxNVEFCU1NZbHNLdDhv
   TnRsaGdSLzFralNTUlFjSGt0c0RjU2lRR0tNZGtTbHA0QXlYZjd2bkhQQmU0eUN3WVYyUHBTTjA
   0a2JvaUozcEJseHNHd1YvWmxMMjZNMnVlWUhLWUN1WGhkcUZ3eFZnbTUyaDNvZUpPT3Qvdlk0RW
   NRcTdlcUhtNm0wM1o5YjdQUnpZTTJLR1hIRG1PTWs3dkRwZU1WbExEUFNHWXoxK1Uzc0R4SnplY
   lNwYmFKbVQ3aW16VUtmZ2dFWTd4eGY0Y3pmSDB5ajV3TnpTR1RPdlE9PSI7ICJwdXJjaGFzZS1p
   bmZvIiA9ICJld29KSW05eWFXZHBibUZzTFhCMWNtTm9ZWE5sTFdSaGRHVXRjSE4wSWlBOUlDSXl
   NREUxTFRFeExUSXpJREE0T2pNeU9qSTNJRUZ0WlhKcFkyRXZURzl6WDBGdVoyVnNaWE1pT3dvSk
   luVnVhWEYxWlMxcFpHVnVkR2xtYVdWeUlpQTlJQ0psTkRjME1qVmtaamRtWWpaaFlqaGpNMk0zW
   lRVM016QmpNMkUzWldJMVlqWTFOak00TWpOa0lqc0tDU0p2Y21sbmFXNWhiQzEwY21GdWMyRmpk
   R2x2YmkxcFpDSWdQU0FpTVRBd01EQXdNREU0TVRRNU1qVTFOeUk3Q2draVluWnljeUlnUFNBaU1
   5STdDZ2tpZEhKaGJuTmhZM1JwYjI0dGFXUWlJRDBnSWpFd01EQXdNREF4T0RFME9USTFOVGNpT3
   dvSkluRjFZVzUwYVhSNUlpQTlJQ0l4SWpzS0NTSnZjbWxuYVc1aGJDMXdkWEpqYUdGelpTMWtZW
   FJsTFcxeklpQTlJQ0l4TkRRNE1qazJNelEzTVRNNElqc0tDU0oxYm1seGRXVXRkbVZ1Wkc5eUxX
   bGtaVzUwYVdacFpYSWlJRDBnSWpWRE1qYzFRalZCTFRORk5qWXROREF5TmkwNU5USXpMVGcwTlV
   JeU1qVTNOVFZHUXlJN0Nna2ljSEp2WkhWamRDMXBaQ0lnUFNBaWNIVnlZMmhoYzJWeUxtTnZibk
   4xYldGaWJHVkdaV0YwZFhKbElqc0tDU0pwZEdWdExXbGtJaUE5SUNJeE1EWXhOVFUzTkRnMElqc
   0tDU0ppYVdRaUlEMGdJbU52YlM1bGN5NVFkWEpqYUdGelpYSWlPd29KSW5CMWNtTm9ZWE5sTFdS
   aGRHVXRiWE1pSUQwZ0lqRTBORGd5T1RZek5EY3hNemdpT3dvSkluQjFjbU5vWVhObExXUmhkR1V
   pSUQwZ0lqSXdNVFV0TVRFdE1qTWdNVFk2TXpJNk1qY2dSWFJqTDBkTlZDSTdDZ2tpY0hWeVkyaG
   hjMlV0WkdGMFpTMXdjM1FpSUQwZ0lqSXdNVFV0TVRFdE1qTWdNRGc2TXpJNk1qY2dRVzFsY21sa
   llTOU1iM05mUVc1blpXeGxjeUk3Q2draWIzSnBaMmx1WVd3dGNIVnlZMmhoYzJVdFpHRjBaU0ln
   UFNBaU1qQXhOUzB4TVMweU15QXhOam96TWpveU55QkZkR012UjAxVUlqc0tmUT09IjsiZW52aXJ
   vbm1lbnQiID0gIlNhbmRib3giOyJwb2QiID0gIjEwMCI7InNpZ25pbmctc3RhdHVzIiA9ICIwIj
   t9">> ).

-endif. %% applev
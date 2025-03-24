#!/bin/bash

V_CODE=$1
URL_VERIFY="http://localhost:3000/api/verify"


RESPONSE_GET_VERIFY=$(curl -s -X GET "$URL_VERIFY"/$V_CODE)
echo "RESPONSE_GET_VERIFY: $RESPONSE_GET_VERIFY"


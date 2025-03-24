#!/bin/bash

URL_REGISTER="http://localhost:3000/api/register"

USERNAME1="test_user_1@mail.md"
PASSWD1="123456Test1"

RESPONSE_REGISTER1=$(curl -s -X POST "$URL_REGISTER" -H "Content-Type: application/json" -d "{\"email\":\"$USERNAME1\", \"password\":\"$PASSWD1\"}")
echo "Register responce: $RESPONSE_REGISTER1"

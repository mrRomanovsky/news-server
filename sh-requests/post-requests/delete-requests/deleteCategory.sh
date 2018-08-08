#!/bin/sh

curl --header "Content-Type: application/json"  --header "Authorization: 1" --request POST  --data '6' http://localhost:3000/categories/delete
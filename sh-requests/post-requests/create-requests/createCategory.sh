#!/bin/sh

curl --header "Content-Type: application/json"  --header "Authorization: 1" --request POST  --data '{"name":"postCategory"}' http://localhost:3000/categories
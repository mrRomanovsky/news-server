#!/bin/sh

curl --header "Content-Type: application/json" --header "Authorization: 1" --request POST  --data '{"categoryId":1,"name":"changedPostCategory"}' http://localhost:3000/categories/update
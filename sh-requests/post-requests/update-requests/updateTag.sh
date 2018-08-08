#!/bin/sh

curl --header "Content-Type: application/json" --header "Authorization: 1" --request POST   --data '{"tagId":1,"tagName":"changedWithPost"}' http://localhost:3000/tags/update
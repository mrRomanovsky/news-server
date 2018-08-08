#!/bin/sh

curl --header "Content-Type: application/json" --header "Authorization: 1" --request POST --data '{"tagName":"postTag"}' http://localhost:3000/tags
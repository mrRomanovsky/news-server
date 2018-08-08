#!/bin/sh

curl --header "Content-Type: application/json" --header "Authorization: 1" --request POST --data '{"authorId":1,"userId":1,"desc":"description updated via post request"}' http://localhost:3000/authors/update
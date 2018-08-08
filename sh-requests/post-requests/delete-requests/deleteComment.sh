#!/bin/sh

curl --header "Content-Type: application/json" --request POST --data '1' http://localhost:3000/posts/1/comments/delete

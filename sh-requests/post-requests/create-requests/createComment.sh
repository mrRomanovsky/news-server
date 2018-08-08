#!/bin/sh

curl --header "Content-Type: application/json" --request POST --data 'POSTED COMMENT!' http://localhost:3000/posts/1/comments

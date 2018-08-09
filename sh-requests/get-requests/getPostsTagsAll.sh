#!/bin/sh

curl -v -L -G -d "tags_all=[2,3]" http://localhost:3000/posts

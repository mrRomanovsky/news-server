#!/bin/sh

curl --header "Content-Type: application/json" --header "Authorization: 1" --request POST --data '{"authorId":1,"postName":"Posted Draft For Article","additionalPhotos":["https://draft_photo202","https://draft_photo303"],"textContent":"Posted draft","mainPhoto":"https://draftPost/photo.jpg","categoryId":2,"postId":1,"tags":[2]}' http://localhost:3000/drafts

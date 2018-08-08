#!/bin/sh

curl --header "Content-Type: application/json"   --request POST   --data '{"name":"Test Post User","surname":"Test Post Surname","avatar":"http://postAvatar.jpg"}'   http://localhost:3000/users
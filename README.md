# news-server

Blog-like backend server.

## Entities:

User : contains name, surname, avatar, creation time and boolean variable "is_admin";

Author : user's id, short description;

Category: name, nested categories (e.g. "Programming Languages" category can contain "Dynamically-Typed Programming Languages" as a nested category);

Tag: name;

Post: name, author, category, tags (one post can have many tags), text content, main photo, additional photos, comments (one post can have many comments);

Draft: post (to which draft is related), name,  creation time, category, tags, text content, main photo, additional photos

## API:

### GET-requests:

/users - get all users
/posts - get all posts
/posts/<post_id>/comments - get comments for post
/tags - get all tags
/authors - get all authors (for admins only)
/drafts - get all drafts (each user can only get their drafts)

### POST-requests:

All posts requests should contain request bodies with necessary information. See examples of request bodies in sh-requests.

/users - create new user

/users/delete - delete user


/tags - create new tag (for admins only)

/tags/update - update tag information (for admins only)

/tags/delete (for admins only)


/authors - create new author (for admins only)

/authors/update - update author information (for admins only)

/authors/delete - delete author (for admins only)


/drafts - create new draft

/drafts/update - update draft information

/drafts/delete - delete draft

/drafts/publish - publish draft as a post (replace current post or create new post)

/posts/<post_id>/comments - create new comment
/posts/<post_id>/comments/delete - delete comment

There is no way of creating or updating posts directly - it should be done via drafts.

## Please Note:

Pagination and records sorting are available for all entities. Listing them all would be tedious, so the common pattern is: "get-request"/?"some of your parameters (or none at all)"&page="page to get"&sort_by="parameter to sort by"(name of the field in the database)

Posts allow filtering by author name, substring in content etc. See /sh-requests/get-requests/getPosts* for examples.

### About Authorization:

Database stores no passwords now, so authorization is performed simply by checking if the user with id sent in the authorization header is admin etc. See examples in /sh-requests/post-requests

## Building app

Create postgresql used called "news-server".

Go to app folder after downloading or cloning from git.

Run command "sudo -u news-server psql -U news-server -h localhost -W -a -f db_schema.sql "

Enter your password

Set environment variable "NEWS_DB_PASSW" to the password for news-server user.

Run command "stack build"

## Launching the app

After building the app and setting environment variables you can launci it with command "stack exec news-server-exe"

## App structure

/app/Main.hs - main function, launching web-server

/src/

  DbRequests.hs - some common requests to the database used in other modules;

  GetRequests.hs - processing GET-requests

  PostRequests.hs - processing POST-requests

  Model.hs - typeclass for entities in the database

  Requests.hs - processing GET and POSTS requests (using GetRequests.hs and PostRequests.hs) 

/src/models/Author.hs, Category.hs, ... etc. - instances of Model typeclass for database entities

## Testing

sh-scripts in sh-requests folder


## Logging

All logs are written to file "news-server.log".

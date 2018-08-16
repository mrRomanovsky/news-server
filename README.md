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

### Environment Variables


DB_NAME="name of your database"

DB_USER="username for database"

DB_PASSW="password to your database"

LOG_LEVEL="choose one of: None, Requests, Debug"(default = None)

LOG_FILE="file to write logs" (default = news-server.log)

APP_PORT="port on which the applicatin will listen for requests" (default = 3000)

IMPORTANT! db_schema creates database called "news-server" for user "news-server" if you want to use other database credentials you have to change not only the environment variables, but also the db_schema.sql script

Run command "stack build"

## Launching the app

After building the app and setting environment variables you can launci it with command "stack exec news-server-exe"

## App structure

/app/Main.hs - main function, launching web-server

/src/Blog/Config/Config.hs, Logging.hs - app's configuration

/src/Blog/Exceptions.hs - exception handling

/src/Blog/Server/NewsServer - connecting to the database, processing requests

/src/Blog/ServerDB/Author.hs - tools for getting authors from the database

/src/Blog/ServerDB/DbRequests.hs - tools for making requests to the database used in other modules


/src/Blog/Models/Model.hs, Author.hs, Category.hs, ... etc. - Model typeclass and instances for corresponding database entities

### Request handlers

/src/Blog/Handlers/AuthorRequests.hs, CategoryRequests.hs, .. - request handlers for models

/src/models_requests/RequestsUtils.hs - helper-functions for request handlers

### Routing

Blog/Routing/Router.hs - router type and helper-functions for routing

Blog/Routing/Routing.hs - routing configuration (setting request handlers for routes)

## Testing

sh-scripts in sh-requests folder


## Logging

None - no logs

Requests - requests + responses statuses

Debug - Requests + errors

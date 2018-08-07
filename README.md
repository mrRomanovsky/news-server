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
/tags - get all tags
/authors - get all authors (for admins only)
/drafts - get all drafts (each user can only get their drafts)

### POST-requests:

All posts requests should contain request bodies with necessary information. See examples of request bodies in requests-examples.txt

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


There is no way of creating or updating posts directly - it should be done via drafts.


## Building app

Go to app folder after downloading or cloning from git. Run command "stack build"

## Preparing the app

Before launching the app you should set the configuration of slack and telegram bots:

## Launching the app

After building the app and setting environment variables you can launci it with command "stack exec news-server-exe"

## App structure

## Testing

Not implemented yet.


## Logging

All logs are written to file "news-server.log".

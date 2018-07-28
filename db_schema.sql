DROP DATABASE IF EXISTS "news-server";
CREATE DATABASE "news-server" with encoding "UTF-8" owner="news-server"; --connection limit=1;

DROP TABLE IF EXISTS users CASCADE;

CREATE TABLE users (
  users_id serial PRIMARY KEY, 
  name varchar(50) NOT NULL,
  avatar varchar(100) NOT NULL,
  creation_time timestamp default current_timestamp,
  is_admin boolean
);

DROP TABLE IF EXISTS authors CASCADE;

CREATE TABLE authors (
  author_id serial PRIMARY KEY,
  users_id integer references users
);

DROP TABLE IF EXISTS categories CASCADE;

CREATE TABLE categories (
  category_id serial PRIMARY KEY,
  name varchar(50) NOT NULL
);

DROP TABLE IF EXISTS tags CASCADE;

CREATE TABLE tags (
  tag_id serial PRIMARY KEY,
  name varchar(50) NOT NULL
);

DROP TABLE IF EXISTS news CASCADE;

CREATE TABLE news (
  news_id serial PRIMARY KEY,
  creation_time timestamp default current_timestamp, --maybe I should use another datatype
  author_id integer references authors,
  category_id integer references authors,
  tags integer[],
  text_content varchar NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[]
);

DROP TABLE IF EXISTS comments CASCADE;

CREATE TABLE comments (
  comment_id serial PRIMARY KEY,
  comment_text text,
  news_id integer references news
);
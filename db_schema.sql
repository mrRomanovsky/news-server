--SELECT pg_terminate_backend(pid)
--FROM pg_stat_activity
--WHERE datname = 'news-server';
DROP DATABASE IF EXISTS "news-server";
CREATE DATABASE "news-server" with encoding "UTF-8" owner="news-server"; --connection limit=1;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS authors CASCADE;
DROP TABLE IF EXISTS categories CASCADE;
DROP TABLE IF EXISTS tags CASCADE;
DROP TABLE IF EXISTS posts CASCADE;
DROP TABLE IF EXISTS drafts CASCADE;
DROP TABLE IF EXISTS comments CASCADE;

CREATE TABLE users (
  users_id serial PRIMARY KEY, 
  users_name varchar(50) NOT NULL,
  users_surname varchar(50) NOT NULL,
  avatar varchar(100) NOT NULL,
  creation_time timestamp default current_timestamp,
  is_admin boolean
);

INSERT INTO users (users_name, users_surname, avatar, creation_time, is_admin) VALUES
  ('Test User 1', 'Test Surname 1', 'http://testcreative.co.uk/wp-content/uploads/2017/10/Test-Logo-Small-Black-transparent-1.png', TIMESTAMP '2017-07-28 19:09:38', TRUE),
  ('Test User 2', 'Test Surname 2', 'https://s3.amazonaws.com/tinycards/image/36125d06520a2f6acdae39d1221e5ca8', TIMESTAMP '2017-07-28 14:14:14', FALSE),
  ('Test User 3', 'Test Surname 3', 'http://oxydy.com/wp-content/uploads/2018/02/test-img-300x194.png', TIMESTAMP '2017-07-28 21:09:40', TRUE),
  ('Test User 4', 'Test Surname 4', 'https://cdn.pixabay.com/photo/2014/06/03/19/38/road-sign-361514_960_720.png', TIMESTAMP '2017-07-28 10:07:33', FALSE),
  ('Test User 5', 'Test Surname 5', 'https://vignette.wikia.nocookie.net/googology/images/b/bd/Test.jpg/revision/latest?cb=20180119233937', TIMESTAMP '2017-07-28 12:13:12', TRUE);

CREATE TABLE authors (
  author_id serial PRIMARY KEY,
  users_id integer references users,
  author_desc TEXT
);

INSERT INTO authors (users_id)
  SELECT u.users_id FROM users as u
    WHERE u.is_admin;

CREATE TABLE categories (
  category_id serial PRIMARY KEY,
  category_name varchar(50) NOT NULL,
  nested_categories integer[]
);

INSERT INTO categories (category_name) VALUES
  ('Test Category 1'),
  ('Test Category 2'),
  ('Test Category 3'),
  ('Test Category 4'),
  ('Test Category 5');


CREATE TABLE tags (
  tag_id serial PRIMARY KEY,
  tag_name varchar(50) NOT NULL
);

INSERT INTO tags (tag_name) VALUES
  ('tag1'),
  ('tag2'),
  ('tag3'),
  ('tag4'),
  ('tag5');

CREATE TABLE posts (
  post_id serial PRIMARY KEY,
  post_name TEXT NOT NULL,
  creation_time timestamp default current_timestamp,
  author_id integer references authors,
  category_id integer references categories,
  tags INTEGER[],
  text_content text NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[],
  post_comments text[]
);

INSERT INTO posts (post_name, creation_time, author_id, category_id, tags, text_content, main_photo, additional_photos, post_comments) VALUES
  ('Tet Post 1', TIMESTAMP '2017-07-28 10:20:15',
  (SELECT a.author_id FROM authors AS a
    WHERE a.users_id = (SELECT u.users_id FROM users AS u WHERE u.users_name = 'Test User 1')
  ), (SELECT c.category_id FROM categories AS c WHERE c.category_name = 'Test Category 1'),
  array[1, 2, 3], 'Very intresting article.', 'https://top/photo.jpg', array['https://photo2', 'https://photo2'],
  array['WOW!!', 'Great article, bro!']);

INSERT INTO posts (post_name, creation_time, author_id, category_id, tags, text_content, main_photo, additional_photos) VALUES
  ('Tet Post 2', TIMESTAMP '2018-08-15 22:10:15',
  (SELECT a.author_id FROM authors AS a
    WHERE a.users_id = (SELECT u.users_id FROM users AS u WHERE u.users_name = 'Test User 3')
  ), (SELECT c.category_id FROM categories AS c WHERE c.category_name = 'Test Category 1'),
  array[1, 2, 3], 'Awful article', 'https://top/photo2.jpg', array['https://photo2', 'https://photo2']);

CREATE TABLE drafts (
  draft_id SERIAL PRIMARY KEY,
  post_id INTEGER REFERENCES posts,
  author_id INTEGER REFERENCES authors,
  post_name TEXT NOT NULL,
  creation_time timestamp default current_timestamp,
  category_id INTEGER REFERENCES categories,
  tags INTEGER[],
  text_content text NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[],
  post_comments text[]
);

INSERT INTO drafts (post_id, author_id, post_name, category_id, tags, text_content, main_photo, additional_photos) VALUES
  (1, 1, 'Draft Post Name', 2, array[1, 2, 3], 'Draft for article', 'https://draft/photo.jpg', array['https://draft_photo2', 'https://draft_photo3']);

CREATE TABLE comments (
  comment_id serial PRIMARY KEY,
  comment_text text,
  post_id integer --posts --it should be referencing posts!
);

INSERT INTO comments (comment_text, post_id) VALUES
  ('WOW, TOP ARTICLE!!!', (SELECT p.post_id FROM posts AS p LIMIT 1));
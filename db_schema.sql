DROP DATABASE IF EXISTS "news-server";
CREATE DATABASE "news-server" with encoding "UTF-8" owner="news-server"; --connection limit=1;
DROP TABLE IF EXISTS users CASCADE;
DROP TABLE IF EXISTS authors CASCADE;
DROP TABLE IF EXISTS categories CASCADE;
DROP TABLE IF EXISTS tags CASCADE;
DROP TABLE IF EXISTS news CASCADE;
DROP TABLE IF EXISTS comments CASCADE;

CREATE TABLE users (
  users_id serial PRIMARY KEY, 
  users_name varchar(50) NOT NULL,
  avatar varchar(100) NOT NULL,
  creation_time timestamp default current_timestamp,
  is_admin boolean
);

INSERT INTO users (users_name, avatar, creation_time, is_admin) VALUES
  ('Test User 1', 'http://testcreative.co.uk/wp-content/uploads/2017/10/Test-Logo-Small-Black-transparent-1.png', TIMESTAMP '2017-07-28 19:09:38', TRUE),
  ('Test User 2', 'https://s3.amazonaws.com/tinycards/image/36125d06520a2f6acdae39d1221e5ca8', TIMESTAMP '2017-07-28 14:14:14', FALSE),
  ('Test User 3', 'http://oxydy.com/wp-content/uploads/2018/02/test-img-300x194.png', TIMESTAMP '2017-07-28 21:09:40', TRUE),
  ('Test User 4', 'https://cdn.pixabay.com/photo/2014/06/03/19/38/road-sign-361514_960_720.png', TIMESTAMP '2017-07-28 10:07:33', FALSE),
  ('Test User 5', 'https://vignette.wikia.nocookie.net/googology/images/b/bd/Test.jpg/revision/latest?cb=20180119233937', TIMESTAMP '2017-07-28 12:13:12', TRUE);

CREATE TABLE authors (
  author_id serial PRIMARY KEY,
  users_id integer references users
);

INSERT INTO authors (users_id)
  SELECT u.users_id FROM users as u
    WHERE u.is_admin;

CREATE TABLE categories (
  category_id serial PRIMARY KEY,
  category_name varchar(50) NOT NULL,
  category_parent integer references categories(category_id)
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

CREATE TABLE news (
  news_id serial PRIMARY KEY,
  creation_time timestamp default current_timestamp,
  author_id integer references authors,
  category_id integer references authors,
  tags integer[],
  text_content varchar NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[]
);

INSERT INTO news (creation_time, author_id, category_id, tags, text_content, main_photo, additional_photos) VALUES
  (TIMESTAMP '2017-07-28 10:20:15',
  (SELECT a.author_id FROM authors AS a
    WHERE a.users_id = (SELECT u.users_id FROM users AS u WHERE u.users_name = 'Test User 1')
  ), (SELECT c.category_id FROM categories AS c WHERE c.category_name = 'Test Category 1'),
  array[1, 2, 3], 'Very intresting article.', 'https://top/photo.jpg', array['https://photo2', 'https://photo2']);

CREATE TABLE comments (
  comment_id serial PRIMARY KEY,
  comment_text text,
  news_id integer references news
);

INSERT INTO comments (comment_text, news_id) VALUES
  ('WOW, TOP ARTICLE!!!', (SELECT n.news_id FROM news AS n LIMIT 1));
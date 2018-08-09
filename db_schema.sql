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
  "user_id" SERIAL PRIMARY KEY, 
  "user_name" VARCHAR(50) NOT NULL,
  "user_surname" VARCHAR(50) NOT NULL,
  "user_avatar" VARCHAR(100) NOT NULL,
  "user_creation_time" TIMESTAMP default current_timestamp,
  "user_is_admin" BOOLEAN
);

INSERT INTO users ("user_name", "user_surname", "user_avatar", "user_creation_time", "user_is_admin") VALUES
  ('Test User 1', 'Test Surname 1', 'http://testcreative.co.uk/wp-content/uploads/2017/10/Test-Logo-Small-Black-transparent-1.png', TIMESTAMP '2017-07-28 19:09:38', TRUE),
  ('Test User 2', 'Test Surname 2', 'https://s3.amazonaws.com/tinycards/image/36125d06520a2f6acdae39d1221e5ca8', TIMESTAMP '2017-07-28 14:14:14', FALSE),
  ('Test User 3', 'Test Surname 3', 'http://oxydy.com/wp-content/uploads/2018/02/test-img-300x194.png', TIMESTAMP '2017-07-28 21:09:40', TRUE),
  ('Test User 4', 'Test Surname 4', 'https://cdn.pixabay.com/photo/2014/06/03/19/38/road-sign-361514_960_720.png', TIMESTAMP '2017-07-28 10:07:33', FALSE),
  ('Test User 5', 'Test Surname 5', 'https://vignette.wikia.nocookie.net/googology/images/b/bd/Test.jpg/revision/latest?cb=20180119233937', TIMESTAMP '2017-07-28 12:13:12', TRUE);

CREATE TABLE authors (
  "author_id" SERIAL PRIMARY KEY,
  "user_id" INTEGER REFERENCES users ON DELETE CASCADE,
  "author_desc" TEXT
);

INSERT INTO authors ("user_id")
  SELECT u.user_id FROM users as u
    WHERE u.user_is_admin;

CREATE TABLE categories (
  "category_id" SERIAL PRIMARY KEY,
  "category_name" VARCHAR(50) NOT NULL,
  "category_nested_categories" INTEGER[]
);

INSERT INTO categories ("category_name") VALUES
  ('Test Category 1'),
  ('Test Category 2'),
  ('Test Category 3'),
  ('Test Category 4'),
  ('Test Category 5');


CREATE TABLE tags (
  "tag_id" SERIAL PRIMARY KEY,
  "tag_name" VARCHAR(50) NOT NULL
);

INSERT INTO tags ("tag_name") VALUES
  ('tag1'),
  ('tag2'),
  ('tag3'),
  ('tag4'),
  ('tag5');

CREATE TABLE posts (
  "post_id" SERIAL PRIMARY KEY,
  "post_name" TEXT NOT NULL,
  "post_creation_time" TIMESTAMP default current_timestamp,
  "author_id" INTEGER REFERENCES authors ON DELETE CASCADE,
  "category_id" INTEGER REFERENCES categories ON DELETE CASCADE,
  "post_tags" INTEGER[],
  "post_text_content" TEXT NOT NULL,
  "post_main_photo" TEXT NOT NULL,
  "post_additional_photos" TEXT[],
  "post_comments" TEXT[]
);

INSERT INTO posts ("post_name", "post_creation_time", "author_id", "category_id", "post_tags", "post_text_content", "post_main_photo", "post_additional_photos", "post_comments") VALUES
  ('Tet Post 1', TIMESTAMP '2017-07-28 10:20:15',
  (SELECT a.author_id FROM authors AS a
    WHERE a.user_id = (SELECT u.user_id FROM users AS u WHERE u.user_name = 'Test User 1')
  ), (SELECT c.category_id FROM categories AS c WHERE c.category_name = 'Test Category 1'),
  array[1, 2, 3], 'Very intresting article.', 'https://top/photo.jpg', array['https://photo2', 'https://photo2'],
  array['WOW!!', 'Great article, bro!']);

INSERT INTO posts ("post_name", "post_creation_time", "author_id", "category_id", "post_tags", "post_text_content", "post_main_photo", "post_additional_photos") VALUES
  ('Tet Post 2', TIMESTAMP '2018-08-15 22:10:15',
  (SELECT a.author_id FROM authors AS a
    WHERE a.user_id = (SELECT u.user_id FROM users AS u WHERE u.user_name = 'Test User 3')
  ), (SELECT c.category_id FROM categories AS c WHERE c.category_name = 'Test Category 1'),
  array[1, 2, 3], 'Awful article', 'https://top/photo2.jpg', array['https://photo2', 'https://photo2']);

CREATE TABLE drafts (
  "draft_id" SERIAL PRIMARY KEY,
  "post_id" INTEGER REFERENCES posts ON DELETE CASCADE,
  "author_id" INTEGER REFERENCES authors ON DELETE CASCADE,
  "draft_name" TEXT NOT NULL,
  "draft_creation_time" TIMESTAMP default current_timestamp,
  "category_id" INTEGER REFERENCES categories ON DELETE CASCADE,
  "draft_tags" INTEGER[],
  "draft_text_content" text NOT NULL,
  "draft_main_photo" text NOT NULL,
  "draft_additional_photos" text[],
  "draft_comments" text[]
);

INSERT INTO drafts ("post_id", "author_id", "draft_name", "category_id", "draft_tags", "draft_text_content", "draft_main_photo", "draft_additional_photos") VALUES
  (1, 1, 'Draft Post Name', 2, array[1, 2, 3], 'Draft for article', 'https://draft/photo.jpg', array['https://draft_photo2', 'https://draft_photo3']);

--UPDATE b
--SET column1 = a.column1,
--  column2 = a.column2,
--  column3 = a.column3
--FROM a
--WHERE a.id = b.id
--AND b.id = 1

CREATE OR REPLACE FUNCTION publish_draft(draftId INTEGER) RETURNS VOID AS $$ 
    DECLARE 
    BEGIN 
          CREATE TEMP TABLE draft AS SELECT * FROM drafts WHERE draft_id = draftId;
          UPDATE posts SET post_creation_time = draft.draft_creation_time,
                           post_name = draft.draft_name,
                           category_id = draft.category_id,
                           post_tags = draft.draft_tags,
                           post_text_content = draft.draft_text_content,
                           post_main_photo = draft.draft_main_photo,
                           post_additional_photos = draft.draft_additional_photos
                       FROM draft WHERE posts.post_id = draft.post_id;  
           IF NOT FOUND THEN INSERT INTO posts( post_creation_time, 
                                                post_name, 
                                                author_id, 
                                                category_id, 
                                                post_tags, 
                                                post_text_content, 
                                                post_main_photo, 
                                                post_additional_photos ) 
               SELECT draft.draft_creation_time,
                      draft.draft_name,
                      draft.author_id,
                      draft.category_id,  
                      draft.draft_tags,
                      draft.draft_text_content,
                      draft.draft_main_photo,
                      draft.draft_additional_photos FROM draft; 
           END IF;
           DROP TABLE draft;
    END;
    $$ LANGUAGE 'plpgsql'; 
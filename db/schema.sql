
SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;


COMMENT ON DATABASE postgres IS 'default administrative connection database';



CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;



COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';



CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;



COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE blog (
    id integer NOT NULL,
    username character varying(255) NOT NULL,
    title character varying(255) NOT NULL,
    password_digest text DEFAULT crypt(md5((random())::text), gen_salt('bf'::text)) NOT NULL,
    theme jsonb
);



CREATE SEQUENCE blog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE blog_id_seq OWNED BY blog.id;



CREATE TABLE comment (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    comment text NOT NULL,
    post_id integer,
    commented_at timestamp with time zone DEFAULT now() NOT NULL,
    is_spam boolean DEFAULT false NOT NULL
);



CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE comment_id_seq OWNED BY comment.id;



CREATE TABLE hostname (
    id integer NOT NULL,
    hostname text NOT NULL,
    blog_id integer
);



CREATE SEQUENCE hostname_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE hostname_id_seq OWNED BY hostname.id;



CREATE TABLE invite (
    invite_code text NOT NULL
);



CREATE TABLE post (
    id integer NOT NULL,
    title character varying(255),
    body text,
    posted_at timestamp with time zone DEFAULT now(),
    slug character varying(32) NOT NULL,
    body_html text NOT NULL,
    summary text NOT NULL,
    blog_id integer NOT NULL,
    tags text[] DEFAULT '{}'::text[] NOT NULL
);



CREATE SEQUENCE post_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;



ALTER SEQUENCE post_id_seq OWNED BY post.id;



CREATE TABLE schema_migrations (
    version character varying(28)
);



ALTER TABLE ONLY blog ALTER COLUMN id SET DEFAULT nextval('blog_id_seq'::regclass);



ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);



ALTER TABLE ONLY hostname ALTER COLUMN id SET DEFAULT nextval('hostname_id_seq'::regclass);



ALTER TABLE ONLY post ALTER COLUMN id SET DEFAULT nextval('post_id_seq'::regclass);



ALTER TABLE ONLY blog
    ADD CONSTRAINT blog_pkey PRIMARY KEY (id);



ALTER TABLE ONLY blog
    ADD CONSTRAINT blog_username_key UNIQUE (username);



ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);



ALTER TABLE ONLY hostname
    ADD CONSTRAINT hostname_hostname_key UNIQUE (hostname);



ALTER TABLE ONLY hostname
    ADD CONSTRAINT hostname_pkey PRIMARY KEY (id);



ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_pkey PRIMARY KEY (invite_code);



ALTER TABLE ONLY post
    ADD CONSTRAINT post_pkey PRIMARY KEY (id);



CREATE UNIQUE INDEX blog_username_idx ON blog USING btree (username);



CREATE UNIQUE INDEX post_stub_idx ON post USING btree (slug);



CREATE INDEX post_tags_idx ON post USING btree (blog_id, tags);



ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_post_id_fkey FOREIGN KEY (post_id) REFERENCES post(id) ON UPDATE CASCADE ON DELETE CASCADE;



ALTER TABLE ONLY hostname
    ADD CONSTRAINT hostname_blog_id_fkey FOREIGN KEY (blog_id) REFERENCES blog(id);



ALTER TABLE ONLY post
    ADD CONSTRAINT post_blog_id_fkey FOREIGN KEY (blog_id) REFERENCES blog(id);



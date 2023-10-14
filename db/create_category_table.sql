CREATE TABLE category (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    slug VARCHAR(255) NOT NULL,
    CONSTRAINT uc_category_name UNIQUE (name),
    CONSTRAINT uc_category_slug UNIQUE (slug)
);
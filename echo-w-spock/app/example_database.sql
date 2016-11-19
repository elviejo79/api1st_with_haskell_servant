-- This file is used to create an example users table

CREATE TABLE user (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       username varchar(50) not null,
       password varchar(255) not null,
       created_at TIMESTAMP not null DEFAULT CURRENT_TIMESTAMP,
       updated_at TIMESTAMP not null DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO user(username,password) VALUES
       ('lotz','$2y$04$UMnRYB26AvreBv0v4efdauIIr3qTM0opEKln26tSy6XXmKV4hS56S'),
       ('alice','$2y$04$kRpVhhxbgerHneJJ4HqmNe8MIB7WbPJPXXI3Zy0hFhWpiaJIz6t3m'),
       ('bob','$2y$04$qbhEesNseMuIpJfFzN7F7uCN6Y5CB0vmMs7eq708CAAx8wnzxvGAm')
;

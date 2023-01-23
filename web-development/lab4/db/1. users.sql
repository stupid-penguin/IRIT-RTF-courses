create table if not exists users
(
	name text,
	pass text
);
insert into users (name, pass) values ('user1', MD5('12345'));
insert into users (name, pass) values ('user2', MD5('56921'));
insert into users (name, pass) values ('user3', MD5('48026'));
insert into users (name, pass) values ('user4', MD5('58923'));
insert into users (name, pass) values ('user5', MD5('70457'));
insert into users (name, pass) values ('user6', MD5('12789'));
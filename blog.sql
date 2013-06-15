-- Blog entry table
CREATE TABLE `blog_entry` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `date` date NOT NULL,
  `blog_url` varchar(64) NOT NULL,
  `blog_title` varchar(64) NOT NULL,
  `blog_post` longtext NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `blog_url` (`blog_url`)
) 

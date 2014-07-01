
-- ----------------------------
-- Table structure for `mysql_test`
-- ----------------------------
DROP TABLE IF EXISTS `mysql_test`;
CREATE TABLE `mysql_test` (
    `key` int(12) NOT NULL DEFAULT '0' COMMENT '主键', 
	`type` int(12) NOT NULL DEFAULT '0' COMMENT '类型1', 
	`term` varchar(12) NULL COMMENT '类型2', 
	`string` varchar(20) NOT NULL COMMENT '类型3', 
	`term2` varchar(12) NULL COMMENT '类型4', 
	`term3` timestamp(6) NULL COMMENT '类型4',
	KEY `type` (`type`)
) ENGINE=innoDB DEFAULT CHARSET='utf8';


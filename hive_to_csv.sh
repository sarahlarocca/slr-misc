#!/bin/bash 
 
beeline --outputformat=csv2 -u "jdbc:hive2://host:port/default;principal=hive/host@name.COM" -n username -f "select * from db_name.$1" > $1.csv

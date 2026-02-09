select  
	t.TABLE_SCHEMA          as SchemaName,
	t.TABLE_NAME            as TableName,
	m.definition            as Definition
from 
	INFORMATION_SCHEMA.TABLES t
        left join SYS.VIEWS v on v.name = t.TABLE_NAME 
        left join SYS.SQL_MODULES m on m.object_id = v.object_id 
where
  	t.TABLE_TYPE = 'VIEW'
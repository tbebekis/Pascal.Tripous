select  
	TABLE_SCHEMA              as SchemaName,
	TABLE_NAME                as TableName
from 
	INFORMATION_SCHEMA.TABLES
where
  	TABLE_TYPE = 'BASE TABLE'
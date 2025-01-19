select 
	TABLE_SCHEMA 			as SchemaName,
	TABLE_NAME				as TableName
from 
    INFORMATION_SCHEMA.TABLES
where   
	TABLE_SCHEMA = '@SCHEMA_NAME'   
	and TABLE_TYPE = 'BASE TABLE'
 order by 
    TABLE_SCHEMA, TABLE_NAME
select 
	TABLE_SCHEMA 			as SchemaName,
	TABLE_NAME				as TableName
from 
    INFORMATION_SCHEMA.TABLES
where 
	TABLE_SCHEMA not in ('sys', 'mysql', 'information_schema', 'performance_schema')        
	and TABLE_TYPE = 'BASE TABLE'
 order by 
    TABLE_SCHEMA, TABLE_NAME
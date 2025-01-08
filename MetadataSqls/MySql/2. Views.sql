select 
	TABLE_SCHEMA 			as SchemaName,
	TABLE_NAME			    as TableName,
	VIEW_DEFINITION         as Definition
from 
    INFORMATION_SCHEMA.VIEWS 
where 
	TABLE_SCHEMA not in ('sys', 'mysql', 'information_schema', 'performance_schema')        
order by 
    TABLE_SCHEMA, TABLE_NAME
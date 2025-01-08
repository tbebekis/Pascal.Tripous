select 
    table_schema                as SchemaName,
    table_name                  as TableName
from 
    information_schema.tables 
where 
    table_type = 'BASE TABLE' 
    and table_schema not in ('pg_catalog', 'information_schema')
order by table_schema, table_name
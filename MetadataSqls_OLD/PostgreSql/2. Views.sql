select
    table_schema                 as SchemaName,
    table_name                   as TableName,
    view_definition              as Definition
from 
    information_schema.views
where 
    table_schema not in ('pg_catalog', 'information_schema')
order by table_schema, table_name
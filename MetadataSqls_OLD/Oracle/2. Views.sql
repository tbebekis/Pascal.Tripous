select
    v.OWNER                  as SchemaName,
    v.VIEW_NAME              as TableName,
    v.TEXT                   as Definition
from 
    ALL_VIEWS  v
        inner join USER_VIEWS uv on uv.VIEW_NAME = v.VIEW_NAME   
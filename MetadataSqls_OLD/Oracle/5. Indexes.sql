select
    c.INDEX_OWNER                       as SchemaName,
    c.TABLE_NAME                        as TableName,
    c.INDEX_NAME                        as IndexName,    
    c.COLUMN_NAME                       as FieldName,
    c.COLUMN_POSITION                   as FieldPosition,
    DECODE(ix.UNIQUENESS,'UNIQUE',1,0)  as IsUnique,
    ix.INDEX_TYPE                       as IndexType 
from 
    ALL_IND_COLUMNS c
        inner join USER_TABLES ut on ut.TABLE_NAME = c.TABLE_NAME   
        inner join ALL_INDEXES ix ON ix.OWNER = c.INDEX_OWNER AND ix.INDEX_NAME = c.INDEX_NAME
order by
    c.TABLE_NAME,
    c.INDEX_NAME,
    c.COLUMN_POSITION         

 
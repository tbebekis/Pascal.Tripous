select 
    TABLE_SCHEMA                    as SchemaName,
    TABLE_NAME                      as TableName,
    INDEX_NAME                      as IndexName,
    COLUMN_NAME                     as FieldName,
    SEQ_IN_INDEX                    as FieldPosition,
    INDEX_TYPE                      as IndexType,
    case NON_UNIQUE when 1 then 0 else 1 end as IsUnique
from 
    INFORMATION_SCHEMA.STATISTICS 
where 
    TABLE_SCHEMA = '@SCHEMA_NAME'
 order by 
    TABLE_NAME, 
    INDEX_NAME, 
    SEQ_IN_INDEX
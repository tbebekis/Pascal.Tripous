select
    SCHEMA_NAME(t.schema_id)        as SchemaName,
    t.name                          as TableName,
    ind.name                        as IndexName,
    col.name                        as FieldName,
    ic.key_ordinal                  as FieldPosition,
    is_unique                       as IsUnique,
    ind.type_desc                   as IndexType,
    is_primary_key                  as IsPrimary
from
    sys.indexes ind
        inner join sys.index_columns ic on  ind.object_id = ic.object_id and ind.index_id = ic.index_id
        inner join sys.columns col on ic.object_id = col.object_id and ic.column_id = col.column_id
        inner join sys.tables t on ind.object_id = t.object_id
where
    t.is_ms_shipped = 0 
    and ic.is_included_column = 0
order by
    t.name,
    ind.name,
    ic.key_ordinal  
select 
    n.nspname                                       as SchemaName,
    t.relname                                       as TableName,
    i.relname                                       as IndexName,
    a.attname                                       as FieldName,
    a.attnum                                        as FieldPosition,
    ''                                              as IndexType,
    ix.indisunique                                  as IsUnique 
from
    pg_catalog.pg_class i 
        inner join pg_catalog.pg_index ix on ix.indexrelid = i.oid 
        inner join pg_catalog.pg_class t on ix.indrelid = t.oid 
        inner join pg_catalog.pg_attribute a on i.oid = a.attrelid 
        left join pg_catalog.pg_namespace n on n.oid = i.relnamespace
where
    i.relkind = 'i'
    and n.nspname not in ('pg_catalog', 'information_schema', 'pg_toast')
    and pg_catalog.pg_table_is_visible(i.oid)
    and t.relkind = 'r'
order by
    n.nspname, 
    t.relname, 
    i.relname, 
    a.attnum
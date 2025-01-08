SELECT 
  tr.TRIGGER_SCHEMA                                      as SchemaName,
  tr.TRIGGER_NAME                                        as TriggerName,
  tr.EVENT_OBJECT_TABLE                                  as TableName,
  concat(tr.ACTION_TIMING, ' ', tr.EVENT_MANIPULATION)   as TriggerType,
  tr.ACTION_STATEMENT                                    as Definition
FROM 
	information_schema.Triggers tr
        inner join information_schema.tables t
            on  tr.EVENT_OBJECT_SCHEMA = t.TABLE_SCHEMA 
            and tr.EVENT_OBJECT_TABLE = t.TABLE_NAME
where 
    t.table_type = 'BASE TABLE' 
    and t.TABLE_SCHEMA not in ('pg_catalog', 'information_schema')
order by 
    tr.TRIGGER_SCHEMA, 
    tr.TRIGGER_NAME
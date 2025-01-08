select 
    tr.TRIGGER_SCHEMA                     as SchemaName,
    tr.TRIGGER_NAME                       as TriggerName,
    tr.EVENT_OBJECT_TABLE                 as TableName,
    concat(tr.ACTION_TIMING, ' ', tr.EVENT_MANIPULATION) as TriggerType,
    0                                     as IsInactive,
    tr.ACTION_STATEMENT 				  as Definition
from 
    INFORMATION_SCHEMA.TRIGGERS tr
    inner join INFORMATION_SCHEMA.TABLES t 
            on  tr.EVENT_OBJECT_SCHEMA = t.TABLE_SCHEMA 
            and tr.EVENT_OBJECT_TABLE = t.TABLE_NAME
where 
    t.TABLE_SCHEMA not in ('sys', 'mysql', 'information_schema', 'performance_schema') 
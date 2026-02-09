select
    tl.[schema]                             as SchemaName,
    t.tbl_name                              as TableName,
    t.name                                  as TriggerName,
    ''                                      as TriggerType,
    t.sql                                   as Definition
from
    sqlite_master t 
        inner join pragma_table_list(t.tbl_name) tl on tl.name = t.tbl_name      
where 
    t.[type] = 'trigger'
order by 
    tl.[schema],
    t.tbl_name,
    t.name
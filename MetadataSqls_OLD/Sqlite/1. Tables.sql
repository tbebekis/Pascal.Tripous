select 
    tl.[schema]        as SchemaName,
    t.name             as TableName,
    t.sql              as Definition
from                                
    sqlite_master t 
        inner join pragma_table_list(t.name) tl    
where 
    t.type = 'table'  
order by 
    tl.[schema],
    t.name 
select 
    tl.[schema]        as SchemaName,
    t.name             as TableName
from                                
    sqlite_master t 
        inner join pragma_table_list(t.name) tl    
where 
   t.type = 'view'    
order by 
    tl.[schema],
    t.name
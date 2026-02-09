select
    tl.[schema]                             as SchemaName,
    i.tbl_name                              as TableName,
    i.name                                  as IndexName,
    ii.name                                 as FieldName,
    ii.seqno                                as FieldPosition,
    il.[unique]                             as IsUnique,
    case il.origin 
    	when 'pk' then 'PRIMARY KEY'
    	when 'u' then 'UNIQUE'
    	else 'INDEX'
    end                                     as IndexType
from
    sqlite_master i
        inner join pragma_index_info(i.name) ii
        inner join pragma_index_list(i.tbl_name) il on il.name = i.name
        inner join pragma_table_list(i.tbl_name) tl on tl.name = i.tbl_name  
where 
    i.type = 'index'    
    and il.origin not in ('pk', 'u')      
order by
    tl.[schema],
	TableName,
	IndexName,
	FieldPosition  
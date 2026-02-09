select 
    tl.[schema]                                 as SchemaName,
    ''                                          as ConstraintName,
    'FOREIGN KEY'                               as ConstraintTypeText,
    2                                           as ConstraintType,
    t.name                                      as TableName,
    fk.[from]                                   as FieldName,
    fk.[table]                                  as ForeignTable,
    fk.[to]                                     as ForeignField,
    fk.on_update                                as UpdateRule,
    fk.on_delete                                as DeleteRule,
    fk.seq                                      as FieldPosition     
from                                
    sqlite_master t 
        inner join pragma_foreign_key_list(t.name) fk  
        inner join pragma_table_list(t.tbl_name) tl on tl.name = t.tbl_name      
where
    t.type = 'table'
order by
    tl.[schema],
	TableName,
	FieldPosition        


 
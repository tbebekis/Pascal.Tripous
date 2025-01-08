select 
    tl.[schema]                                 as SchemaName,
    t.name                                      as TableName,
    ti.name                                     as FieldName,
    ti.type                                     as DataType,
    ''                                          as DataSubType,
    case ti.[notnull] when 0 then 1 else 0 end  as IsNullable,
    0                                           as SizeInChars,
    0                                           as SizeInBytes,
    0                                           as DecimalPrecision,
    0                                           as DecimalScale,
    ti.dflt_value                               as DefaultValue,
    ''                                          as Expression,
    ti.cid                                      as OrdinalPosition     
from                                
    sqlite_master t 
        inner join pragma_table_list(t.name) tl   
        inner join pragma_table_info(t.name) ti
where 
    t.type = 'view'          
order by 
    tl.[schema],
    TableName,
    OrdinalPosition   
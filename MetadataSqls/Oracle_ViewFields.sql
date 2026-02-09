select 
    t.OWNER                                     as SchemaName,        
    t.TABLE_NAME                                as TableName,
    t.COLUMN_NAME                               as FieldName,
    t.DATA_TYPE                                 as DataType, 
    ' '                                         as DataSubType,
    case t.NULLABLE when 'Y' then 1 else 0 end  as IsNullable,  
    coalesce(t.CHAR_LENGTH, 0)                  as SizeInChars,
    coalesce(t.DATA_LENGTH, 0)                  as SizeInBytes,
    coalesce(t.DATA_PRECISION, 0)               as DecimalPrecision,
    coalesce(t.DATA_SCALE, 0)                   as DecimalScale,    
    ' '                                         as DefaultValue,
    ' '                                         as Expression,
    t.COLUMN_ID                                 as OrdinalPosition
from 
    ALL_TAB_COLUMNS t 
        inner join USER_VIEWS uv on uv.VIEW_NAME = t.TABLE_NAME
order by
	t.TABLE_NAME,
	t.COLUMN_ID    
	
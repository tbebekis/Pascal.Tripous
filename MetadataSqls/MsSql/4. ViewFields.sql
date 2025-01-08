select 
    c.TABLE_SCHEMA                                      as SchemaName,           
    c.TABLE_NAME                                        as TableName, 
    c.COLUMN_NAME                                       as FieldName, 
    c.DATA_TYPE                                         as DataType, 
    ''                                                  as DataSubType,
    case c.IS_NULLABLE when 'YES' then 1 else 0 end     as IsNullable,
    coalesce(c.CHARACTER_MAXIMUM_LENGTH, 0)             as SizeInChars, 
    coalesce(c.CHARACTER_OCTET_LENGTH, 0)               as SizeInBytes,
    coalesce(c.NUMERIC_PRECISION, 0)                    as DecimalPrecision, 
    coalesce(c.NUMERIC_SCALE, 0)                        as DecimalScale, 
    coalesce(c.COLUMN_DEFAULT, '')                      as DefaultValue, 
    coalesce(cc.Definition, '')                         as Expression,
    c.ORDINAL_POSITION                                  as OrdinalPosition
from 
    INFORMATION_SCHEMA.COLUMNS c
        left join INFORMATION_SCHEMA.TABLES t  on c.TABLE_SCHEMA = t.TABLE_SCHEMA 
                and c.TABLE_NAME = t.TABLE_NAME
		left join sys.computed_columns cc on cc.name = c.COLUMN_NAME 
where 
    TABLE_TYPE = 'VIEW'
order by 
   c.TABLE_NAME, 
   c.ORDINAL_POSITION
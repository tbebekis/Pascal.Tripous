select
    c.table_schema                                       as SchemaName,
    c.table_name                                         as TableName,
    c.column_name                                        as FieldName,
    c.udt_name                                           as DataType,
    ''                                                   as DataSubType,
    case c.is_nullable when 'YES' then 1 else 0 end      as IsNullable, 
    coalesce(c.character_maximum_length, 0)              as SizeInChars, 
    coalesce(c.character_octet_length, 0)                as SizeInBytes,
    coalesce(c.numeric_precision, 0)                     as DecimalPrecision, 
    coalesce(c.numeric_scale, 0)                         as DecimalScale, 
    coalesce(c.column_default, '')                       as DefaultValue, 
    ''                                                   as Expression,
    c.ordinal_position                                   as OrdinalPosition
from 
    information_schema.columns c
      inner join information_schema.views  t 
            on  c.table_schema = t.table_schema 
            and c.table_name = t.table_name
where 
    t.table_schema not in ('pg_catalog', 'information_schema')
order by 
    t.table_schema, 
    t.table_name, 
    c.ordinal_position  
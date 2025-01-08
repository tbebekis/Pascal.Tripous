select distinct
    Constraints.constraint_schema                     as SchemaName,  
    Constraints.constraint_name                       as ConstraintName,
    coalesce(Constraints.constraint_type, '')         as ConstraintTypeText,
    case coalesce(Constraints.constraint_type, '')
        when 'PRIMARY KEY' then 1
        when 'FOREIGN KEY' then 2
        when 'UNIQUE' then 3
        when 'CHECK' then 4
        when 'NOT NULL' then 5
        else 0
    end                                               as ConstraintType,
    KeyColumns.table_name                             as TableName,
    KeyColumns.column_name                            as FieldName,    
    coalesce(KeyColumns.REFERENCED_TABLE_NAME, '')    as ForeignTable,
    coalesce(KeyColumns.REFERENCED_COLUMN_NAME, '')   as ForeignField,  
    coalesce(KeyColumns.REFERENCED_TABLE_SCHEMA, '')  as ForeignSchema,
    coalesce(Refs.update_rule, '')				      as UpdateRule,
    coalesce(Refs.delete_rule, '')				      as DeleteRule,
    KeyColumns.ordinal_position                       as FieldPosition   
from 
    INFORMATION_SCHEMA.TABLE_CONSTRAINTS Constraints
        inner join INFORMATION_SCHEMA.KEY_COLUMN_USAGE KeyColumns            
            on  (Constraints.constraint_catalog = KeyColumns.constraint_catalog or Constraints.constraint_catalog is null) 
            and Constraints.constraint_schema = KeyColumns.constraint_schema 
            and Constraints.constraint_name = KeyColumns.constraint_name 
            and Constraints.table_name = KeyColumns.table_name
        left join INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS Refs
            on (Constraints.constraint_catalog = Refs.constraint_catalog or Constraints.constraint_catalog is null) 
            and Constraints.constraint_schema = Refs.constraint_schema 
            and Constraints.constraint_name = Refs.constraint_name 
            and Constraints.table_name = Refs.table_name
where
    Constraints.constraint_schema not in ('sys', 'mysql', 'information_schema', 'performance_schema') 
order by
    KeyColumns.table_name, 
    Constraints.constraint_name, 
    KeyColumns.ordinal_position
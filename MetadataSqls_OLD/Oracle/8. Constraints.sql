select 
    c.owner                                            as SchemaName,  
    c.constraint_name                                  as ConstraintName,
    c.CONSTRAINT_TYPE                                  as ConstraintTypeText,
    case coalesce(c.CONSTRAINT_TYPE, '')
        when 'P' then 1
        when 'R' then 2
        when 'U' then 3
        when 'C' then 4
        else 0
    end                                                 as ConstraintType,
    c.table_name                                        as TableName,
    col.column_name                                     as FieldName,
    coalesce(c2.table_name, ' ')                        as ForeignTable,  
    coalesce(col2.column_name, ' ')                     as ForeignField,
    ' '                                                 as UpdateRule,
    coalesce(c.delete_rule, ' ')                        as DeleteRule,
    coalesce(col.position, 1)                           as FieldPosition    
from 
    USER_CONSTRAINTS c
        inner join USER_CONS_COLUMNS col  ON c.constraint_name = col.constraint_name AND c.owner = col.owner
        left join USER_CONSTRAINTS c2 ON c.r_constraint_name = c2.constraint_name AND c.owner = c2.owner 
        left join USER_CONS_COLUMNS col2 on c2.constraint_name = col2.constraint_name AND c2.owner = col2.owner
order by 
    c.table_name, 
    c.constraint_name,    
    col.position
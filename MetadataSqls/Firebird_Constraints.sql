select 
    rel.RDB$OWNER_NAME                      as SchemaName,    
    rc.RDB$CONSTRAINT_NAME                  as ConstraintName,
    coalesce(rc.RDB$CONSTRAINT_TYPE, '')    as ConstraintTypeText,
    case coalesce(rc.RDB$CONSTRAINT_TYPE, '')
        when 'PRIMARY KEY' then 1
        when 'FOREIGN KEY' then 2
        when 'UNIQUE' then 3
        when 'CHECK' then 4
        when 'NOT NULL' then 5
        else 0
    end                                     as ConstraintType,
    rc.RDB$RELATION_NAME                    as TableName,
    s.RDB$FIELD_NAME                        as FieldName,
    coalesce(i2.RDB$RELATION_NAME, '')      as ForeignTable,
    coalesce(s2.RDB$FIELD_NAME, '')         as ForeignField,
    coalesce(refc.RDB$UPDATE_RULE, '')      as UpdateRule,
    coalesce(refc.RDB$DELETE_RULE, '')      as DeleteRule,
    (s.RDB$FIELD_POSITION + 1)              as FieldPosition
from 
    RDB$INDEX_SEGMENTS s
        left join RDB$INDICES i on i.RDB$INDEX_NAME = s.RDB$INDEX_NAME
        left join RDB$RELATION_CONSTRAINTS rc on rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME
        left join RDB$REF_CONSTRAINTS refc on rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME
        left join RDB$RELATION_CONSTRAINTS rc2 on rc2.RDB$CONSTRAINT_NAME = refc.RDB$const_name_uq
        left join RDB$INDICES i2 on i2.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME
        left join RDB$INDEX_SEGMENTS s2 on i2.RDB$INDEX_NAME = s2.RDB$INDEX_NAME
        left join RDB$relations rel on rel.RDB$RELATION_NAME = rc.RDB$RELATION_NAME
where
  (rc.RDB$CONSTRAINT_TYPE IS NOT NULL)
  and (rel.RDB$SYSTEM_FLAG  is null  or rel.RDB$SYSTEM_FLAG  = 0)     
order by
    rc.RDB$RELATION_NAME,
    rc.RDB$CONSTRAINT_NAME,
    s.RDB$FIELD_POSITION  


 
select
    t.RDB$OWNER_NAME                       as SchemaName,
    tr.RDB$TRIGGER_NAME                    as TriggerName,
    tr.RDB$RELATION_NAME                   as TableName,
    case tr.RDB$TRIGGER_TYPE
        when 1      then 'before insert'
        when 2      then 'after insert'
        when 3      then 'before update'
        when 4      then 'after update'
        when 5      then 'before delete'      
        when 6      then 'after delete'
        when 17     then 'before insert or update'
        when 18     then 'after insert or update'
        when 25     then 'before insert or delete'
        when 26     then 'after insert or delete'
        when 27     then 'before update or delete'
        when 28     then 'after update or delete'
        when 113    then 'before insert or update or delete'
        when 114    then 'after insert or update or delete'
        when 8192   then 'on connect'
        when 8193   then 'on disconnect'
        when 8194   then 'on transaction start'
        when 8195   then 'on transaction commit'
        when 8196   then 'on transaction rollback'      
    end                                    as TriggerType,
    coalesce(tr.RDB$TRIGGER_INACTIVE, 0)   as IsInactive,
    tr.RDB$TRIGGER_SOURCE                  as Definition,
    coalesce(tr.RDB$VALID_BLR, 0)          as IsValid
from
    RDB$TRIGGERS tr
       left join RDB$RELATIONS t on tr.RDB$RELATION_NAME = t.RDB$RELATION_NAME        
where
        (tr.RDB$SYSTEM_FLAG is null or tr.RDB$SYSTEM_FLAG  = 0)                
    and (tr.RDB$RELATION_NAME <> '')                                     
order by
    tr.RDB$RELATION_NAME,
    tr.RDB$TRIGGER_NAME   
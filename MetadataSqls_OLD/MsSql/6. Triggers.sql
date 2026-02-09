select
    SCHEMA_NAME(t.schema_id)                                as SchemaName, 
    tr.name                                                 as TriggerName,    
    t.name                                                  as TableName,
    tt.IsAfter + tt.IsInsert + tt.IsUpdate + tt.IsDelete    as TriggerType,
    tr.is_disabled                                          as IsInactive,
    OBJECT_DEFINITION(tr.object_id)                         as Definition
from 
    sys.triggers as tr
        inner join sys.tables t on tr.parent_id = t.object_id
        inner join (
select
    tr.name                                             as TriggerName,
    t.name                                              as TableName,
    case 
        OBJECTPROPERTY(tr.object_id, 'ExecIsAfterTrigger') 
            when 1 then 'after '
            else 'before ' end                           as IsAfter,     
    case 
        OBJECTPROPERTY(tr.object_id, 'ExecIsInsertTrigger') 
            when 1 then 'insert '
            else '' end                                 as IsInsert,    
    case 
        OBJECTPROPERTY(tr.object_id, 'ExecIsUpdateTrigger') 
            when 1 then 'update '
            else '' end                                 as IsUpdate,
    case 
        OBJECTPROPERTY(tr.object_id, 'ExecIsDeleteTrigger') 
            when 1 then 'delete '
            else '' end                                 as IsDelete
from 
    sys.triggers as tr
        inner join sys.tables t on tr.parent_id = t.object_id        
        ) as tt on tt.TableName = t.name and tt.TriggerName = tr.name
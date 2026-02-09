select
    TABLE_OWNER                             as SchemaName,
    TRIGGER_NAME                            as TriggerName,
    TABLE_NAME                              as TableName,    
    TRIGGER_TYPE || ' ' || TRIGGERING_EVENT as TriggerType,
    TRIGGER_BODY                            as Definition
from 
    USER_TRIGGERS
where
	BASE_OBJECT_TYPE = 'TABLE'

 
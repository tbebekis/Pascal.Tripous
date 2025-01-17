select
    ROUTINE_SCHEMA                    as SchemaName,
    ROUTINE_NAME                      as ProcedureName,    
    ROUTINE_TYPE                      as ProcedureType,  
    ROUTINE_DEFINITION                as Definition  
from 
    INFORMATION_SCHEMA.ROUTINES
where 
    ROUTINE_SCHEMA = '@SCHEMA_NAME'
    and  (ROUTINE_TYPE = 'PROCEDURE' or ROUTINE_TYPE = 'FUNCTION')
order by
    ROUTINE_SCHEMA, 
    ROUTINE_NAME
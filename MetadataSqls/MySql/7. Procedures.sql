select
    ROUTINE_SCHEMA                    as SchemaName,
    ROUTINE_NAME                      as ProcedureName,    
    ROUTINE_TYPE                      as ProcedureType,  
    ROUTINE_DEFINITION                as Definition  
from 
    INFORMATION_SCHEMA.ROUTINES
where 
    (ROUTINE_TYPE = 'PROCEDURE' or ROUTINE_TYPE = 'FUNCTION')
    and ROUTINE_SCHEMA not in ('sys', 'mysql', 'information_schema', 'performance_schema') 
order by
    ROUTINE_SCHEMA, 
    ROUTINE_NAME
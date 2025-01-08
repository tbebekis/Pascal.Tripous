select
    RDB$OWNER_NAME                         as SchemaName,
    RDB$PROCEDURE_NAME                     as ProcedureName,
    case RDB$PROCEDURE_TYPE
        when 1 then 'Selectable'
        when 2 then 'Executable'
    end 									 as ProcedureType,
    RDB$PROCEDURE_INPUTS					 as InputCount,
    RDB$PROCEDURE_OUTPUTS					 as OutputCount,  
    RDB$PROCEDURE_SOURCE					 as Definition,
    coalesce(RDB$VALID_BLR, 0)             as IsValid 
from 
    RDB$PROCEDURES    
where
	(RDB$PROCEDURES.RDB$SYSTEM_FLAG  is null  or RDB$PROCEDURES.RDB$SYSTEM_FLAG  = 0)  
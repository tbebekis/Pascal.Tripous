select 
    r.routine_schema                                     as SchemaName,
    r.routine_name                                       as ProcedureName,    
    r.routine_type                                       as ProcedureType,  
    r.routine_definition                                 as Definition
from 
    information_schema.routines r
where 
    r.specific_schema  not in ('pg_catalog', 'information_schema')   
order by 
    r.routine_schema, 
    r.routine_name
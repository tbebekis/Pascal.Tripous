select
  r.SPECIFIC_SCHEMA              as SchemaName,
  r.SPECIFIC_NAME                as ProcedureName,
  r.ROUTINE_TYPE                 as ProcedureType,
  r.ROUTINE_DEFINITION           as Definition
from 
  INFORMATION_SCHEMA.ROUTINES r
  	left join sys.procedures p on p.name  = r.SPECIFIC_NAME and p.type = 'P'
where 
	is_ms_shipped = 0
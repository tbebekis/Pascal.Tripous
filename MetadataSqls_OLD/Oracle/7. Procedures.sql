select 
  ap.OWNER                      as SchemaName,
  p.OBJECT_NAME                 as ProcedureName,
  p.OBJECT_TYPE                 as ProcedureType,  
  ' '                           as Definition
from 
   USER_PROCEDURES p
      inner join ALL_PROCEDURES ap on ap.PROCEDURE_NAME = p.PROCEDURE_NAME 
select
  s2.SEQUENCE_OWNER                as SchemaName,
  s.SEQUENCE_NAME                  as SequenceName,
  s.LAST_NUMBER                    as CurrentValue,
  s.MIN_VALUE                      as InitialValue,
  s.INCREMENT_BY                   as IncrementBy	
from 
  USER_SEQUENCES s
  	inner join ALL_SEQUENCES s2 on s2.SEQUENCE_NAME = s.SEQUENCE_NAME
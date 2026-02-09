select 
   s.schemaname                         as SchemaName,
   s.sequencename 						as SequenceName,
   s.last_value                         as CurrentValue,
   s.start_value                        as InitialValue,
   s.increment_by                       as IncrementBy   
from 
   pg_sequences  s
order by
  s.sequencename 
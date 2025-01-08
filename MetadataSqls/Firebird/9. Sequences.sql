select
    RDB$OWNER_NAME            as SchemaName,
    RDB$GENERATOR_NAME        as SequenceName,
    RDB$GENERATOR_ID          as CurrentValue,
    RDB$INITIAL_VALUE         as InitialValue,
    RDB$GENERATOR_INCREMENT   as IncrementBy	
from
   RDB$GENERATORS
where
   RDB$SYSTEM_FLAG  is null or RDB$SYSTEM_FLAG = 0 
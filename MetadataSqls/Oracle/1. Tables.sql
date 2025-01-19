select
  t.OWNER              as SchemaName,
  t.TABLE_NAME         as TableName
from 
  ALL_TABLES t
    inner join USER_TABLES ut on ut.TABLE_NAME = t.TABLE_NAME
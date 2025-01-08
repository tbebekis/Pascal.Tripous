select distinct
  Constraints.constraint_schema                    as SchemaName,
  Constraints.constraint_name                      as ConstraintName,
  coalesce(Constraints.constraint_type, '')        as ConstraintTypeText,
  case coalesce(Constraints.constraint_type, '')
      when 'PRIMARY KEY' then 1
      when 'FOREIGN KEY' then 2
      when 'UNIQUE' then 3
      when 'CHECK' then 4
      when 'NOT NULL' then 5
      else 0
  end                                              as ConstraintType,
  KeyColumns.table_name                            as TableName,
  KeyColumns.column_name                           as FieldName,
  coalesce(Constraints2.table_name, '')            as ForeignTable,
  coalesce(ForeignInfo.ForeignField, '')           as ForeignField,
  coalesce(Constraints2.table_schema, '')          as ForeignSchema,
  coalesce(Refs.update_rule, '')                   as UpdateRule,
  coalesce(Refs.delete_rule, '')                   as DeleteRule,
  KeyColumns.ordinal_position                      as FieldPosition
from
  INFORMATION_SCHEMA.TABLE_CONSTRAINTS as Constraints
   		inner join INFORMATION_SCHEMA.KEY_COLUMN_USAGE as KeyColumns
   			on (Constraints.constraint_catalog = KeyColumns.constraint_catalog or Constraints.constraint_catalog is null)
   			and Constraints.constraint_schema = KeyColumns.constraint_schema
   			and Constraints.constraint_name = KeyColumns.constraint_name
   			and Constraints.table_name = KeyColumns.table_name
   		left join INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS as Refs
   			on (Constraints.constraint_catalog = Refs.constraint_catalog or Constraints.constraint_catalog is null)
   			and Constraints.constraint_schema = Refs.constraint_schema
   			and Constraints.constraint_name = Refs.constraint_name
   		left join INFORMATION_SCHEMA.TABLE_CONSTRAINTS as Constraints2
   			on (Constraints2.constraint_catalog = Refs.constraint_catalog or Constraints2.constraint_catalog is null)
   			and Constraints2.constraint_schema = Refs.unique_constraint_schema
   			and Constraints2.constraint_name = Refs.unique_constraint_name
   		left join (
   select
     ForeignKeys.name                                  as ConstraintName,
     SCHEMA_NAME(ForeignKeys.schema_id)                as SchemaName,
     OBJECT_NAME(ForeignKeys.parent_object_id)         as TableName,
     ParentColumns.name                                as FieldName,
     OBJECT_NAME(ForeignKeys.referenced_object_id)     as ForeignTable,
     RefColums.name                                    as ForeignField
   FROM
     sys.foreign_keys as ForeignKeys
       inner join sys.foreign_key_columns as ForeignKeyColumns
               on  ForeignKeys.parent_object_id = ForeignKeyColumns.parent_object_id
               and ForeignKeys.object_id = ForeignKeyColumns.constraint_object_id
               and ForeignKeys.referenced_object_id = ForeignKeyColumns.referenced_object_id
       inner join sys.columns as ParentColumns
               on  ForeignKeyColumns.parent_object_id = ParentColumns.object_id
               and ForeignKeyColumns.parent_column_id = ParentColumns.column_id
       inner join sys.columns as RefColums
               on  ForeignKeyColumns.referenced_object_id = RefColums.object_id
               and ForeignKeyColumns.referenced_column_id = RefColums.column_id
   ) ForeignInfo
   	on  ForeignInfo.ConstraintName = Constraints.constraint_name
   	and ForeignInfo.SchemaName = Constraints.constraint_schema
   	and ForeignInfo.TableName = KeyColumns.table_name
   	and ForeignInfo.FieldName = KeyColumns.column_name
   order by
       KeyColumns.table_name,
       Constraints.constraint_name,
       ordinal_position    
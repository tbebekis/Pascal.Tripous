unit Tripous.Data.Constants;

{$mode DELPHI}{$H+}

interface

uses
  Classes
  ,SysUtils
  ;

const
  // ---------------------------------------------------------------------------------------------------------
  // FirebirdSql
  // ---------------------------------------------------------------------------------------------------------


  // https://www.firebirdfaq.org/faq174/
  // https://www.alberton.info/firebird_sql_meta_info.html
  // https://firebirdsql.org/file/documentation/chunk/en/refdocs/fblangref30/fblangref30-appx04-systables.html
  // https://ib-aid.com/download/docs/firebird-language-reference-2.5/fblangref25-appx04-systables.html
  // https://docwiki.embarcadero.com/InterBase/2020/en/System_Tables

  // https://www.firebirdfaq.org/faq174/
  SFirebirdTablesSql =
    'select                                                ' +
    '  RDB$RELATION_NAME as TableName,                     ' +
    '  RDB$OWNER_NAME    as SchemaName                      ' +
    'from                                                  ' +
    '  RDB$RELATIONS                                       ' +
    'where                                                 ' +
    '  (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG = 0)    ' +
    '  and RDB$VIEW_BLR is null                            ' +
    'order by                                              ' +
    '  RDB$RELATION_NAME                                   ' +
    ''
    ;


  SFirebirdViewsSql =
    'select                                                ' +
    '  RDB$OWNER_NAME                  as SchemaName,       ' +
    '  RDB$RELATION_NAME               as TableName,       ' +
    '  coalesce(RDB$VIEW_SOURCE, '''') as Definition       ' +
    'from                                                  ' +
    '  RDB$RELATIONS                                       ' +
    'where                                                 ' +
    '  (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG = 0)    ' +
    '  and RDB$VIEW_BLR is not null                        ' +
    'order by                                              ' +
    '  RDB$RELATION_NAME                                   '
    ;


  SFirebirdFieldsSql =
    'select                                                                                    ' +
    '  t.RDB$OWNER_NAME                                as SchemaName,                           ' +
    '  tf.RDB$RELATION_NAME                            as TableName,                           ' +
    '  coalesce(t.RDB$VIEW_SOURCE, '''')               as Definition                           ' +
    '  tf.RDB$FIELD_NAME                               as FieldName,                           ' +
    '  case f.RDB$FIELD_TYPE                                                                   ' +
    '    when 7 then ''SMALLINT''                                                              ' +
    '    when 8 then ''INTEGER''                                                               ' +
    '    when 9 then ''QUAD''                                                                  ' +
    '    when 10 then ''FLOAT''                                                                ' +
    '    when 11 then ''D_FLOAT''                                                              ' +
    '    when 12 then ''DATE''                                                                 ' +
    '    when 13 then ''TIME''                                                                 ' +
    '    when 14 then ''CHAR''                                                                 ' +
    '    when 16 then ''INT64''                                                                ' +
    '    when 27 then ''DOUBLE''                                                               ' +
    '    when 35 then ''TIMESTAMP''                                                            ' +
    '    when 37 then ''VARCHAR''                                                              ' +
    '    when 40 then ''CSTRING''                                                              ' +
    '    when 261 then ''BLOB''                                                                ' +
    '    else ''''                                                                             ' +
    '  end                                             as DataType,                            ' +
    '  case                                                                                    ' +
    '    when (f.RDB$FIELD_TYPE = 261) and (f.RDB$FIELD_SUB_TYPE = 1)  then ''TEXT''           ' +
    '    when (f.RDB$FIELD_TYPE = 261) and (f.RDB$FIELD_SUB_TYPE = 0)  then ''BINARY''         ' +
    '    else ''''                                                                             ' +
    '  end                                             AS DataSubType,                         ' +
    '  coalesce(tf.RDB$NULL_FLAG, f.RDB$NULL_FLAG, 0)  as IsNullable,                          ' +
    '  coalesce(f.RDB$CHARACTER_LENGTH, 0)             as SizeInChars,                         ' +
    '  f.RDB$FIELD_LENGTH                              as SizeInBytes,                         ' +
    '  coalesce(f.RDB$FIELD_PRECISION, 0)              as DecimalPrecision,                    ' +
    '  coalesce(f.RDB$FIELD_SCALE, 0)                  as DecimalScale,                        ' +
    '  coalesce(tf.RDB$DEFAULT_SOURCE, '''')           as DefaultValue,                        ' +
    '  coalesce(f.RDB$COMPUTED_SOURCE, '''')           as Expression,                          ' +
    '  tf.RDB$FIELD_POSITION                           as OrdinalPosition                      ' +
    'from                                                                                      ' +
    '  RDB$RELATION_FIELDS tf                                                                  ' +
    '    left join RDB$RELATIONS t ON tf.RDB$RELATION_NAME = t.RDB$RELATION_NAME               ' +
    '    left join RDB$FIELDS f ON tf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME                      ' +
    'where                                                                                     ' +
    '      (tf.RDB$SYSTEM_FLAG is null  or tf.RDB$SYSTEM_FLAG = 0)                             ' +
    '  and (t.RDB$SYSTEM_FLAG  is null  or t.RDB$SYSTEM_FLAG  = 0)                             ' +
    '  and t.RDB$VIEW_BLR is null                                                              ' +
    'order by                                                                                  ' +
    '  tf.RDB$RELATION_NAME,                                                                   ' +
    '  tf.RDB$FIELD_POSITION                                                                   '
    ;


  // https://www.firebirdsql.org/file/documentation/chunk/en/refdocs/fblangref40/fblangref-appx04-indices.html

  SFirebirdIndexesSql =
    'select                                                                                  ' +
    '    t.RDB$OWNER_NAME                        as SchemaName,                               ' +
    '    i.RDB$RELATION_NAME                     as TableName,                               ' +
    '    i.RDB$INDEX_NAME                        as IndexName,                               ' +
    '    isg.RDB$FIELD_NAME                      as FieldName,                               ' +
    '    (isg.RDB$FIELD_POSITION + 1)            as FieldPosition,                           ' +
    '    i.RDB$UNIQUE_FLAG                       as IsUnique,                                ' +
    '    coalesce(rc.RDB$CONSTRAINT_TYPE, '''')  as IndexType,                               ' +
    '    i.RDB$SEGMENT_COUNT                     as FieldCount,                              ' +
    '    coalesce(i.RDB$INDEX_INACTIVE, 0)       as IsInactive,                              ' +
    '    coalesce(i.RDB$FOREIGN_KEY, '''')       as ForeignKey                               ' +
    'from                                                                                    ' +
    '    RDB$INDEX_SEGMENTS isg                                                              ' +
    '        left join RDB$INDICES i on i.RDB$INDEX_NAME = isg.RDB$INDEX_NAME                ' +
    '        left join RDB$RELATION_CONSTRAINTS rc on rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
    '        left join RDB$RELATIONS t on i.RDB$RELATION_NAME = t.RDB$RELATION_NAME          ' +
    'where                                                                                   ' +
    '    (i.RDB$SYSTEM_FLAG is null or i.RDB$SYSTEM_FLAG  = 0)                               ' +
    'order by                                                                                ' +
    '    i.RDB$RELATION_NAME,                                                                ' +
    '    i.RDB$INDEX_NAME,                                                                   ' +
    '    isg.RDB$FIELD_POSITION                                                              '
    ;

  SFirebirdTriggersSql =
    'select                                                                ' +
    '    RDB$TRIGGER_NAME                    as TriggerName,               ' +
    '    RDB$RELATION_NAME                   as TableName,                 ' +
    '    case RDB$TRIGGER_TYPE                                             ' +
    '        when 1      then ''on before insert''                         ' +
    '        when 2      then ''on after insert''                          ' +
    '        when 3      then ''on before update''                         ' +
    '        when 4      then ''on after update''                          ' +
    '        when 5      then ''on before delete''                         ' +
    '        when 6      then ''on after delete''                          ' +
    '        when 17     then ''on before insert or update''               ' +
    '        when 18     then ''on after insert or update''                ' +
    '        when 25     then ''on before insert or delete''               ' +
    '        when 26     then ''on after insert or delete''                ' +
    '        when 27     then ''on before update or delete''               ' +
    '        when 28     then ''on after update or delete''                ' +
    '        when 113    then ''on before insert or update or delete''     ' +
    '        when 114    then ''on after insert or update or delete''      ' +
    '        when 8192   then ''on connect''                               ' +
    '        when 8193   then ''on disconnect''                            ' +
    '        when 8194   then ''on transaction start''                     ' +
    '        when 8195   then ''on transaction commit''                    ' +
    '        when 8196   then ''on transaction rollback''                  ' +
    '    end                                 as TriggerType,               ' +
    '    coalesce(RDB$TRIGGER_INACTIVE, 0)   as IsInactive,                ' +
    '    RDB$TRIGGER_SOURCE                  as Definition,                ' +
    '    coalesce(RDB$VALID_BLR, 0)          as IsValid                    ' +
    'from                                                                  ' +
    '    RDB$TRIGGERS                                                      ' +
    'where                                                                 ' +
    '    (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG  = 0)                 ' +
    ' and (RDB$RELATION_NAME <> '''')                                      ' +
    'order by                                                              ' +
    '    RDB$RELATION_NAME,                                                ' +
    '    RDB$TRIGGER_NAME                                                  '
    ;

  SFirebirdConstraintsSql =
    'select                                                                                                   ' +
    '    rel.RDB$OWNER_NAME                        as SchemaName,                                              ' +
    '    rc.RDB$CONSTRAINT_NAME                    as ConstraintName,                                         ' +
    '    coalesce(rc.RDB$CONSTRAINT_TYPE, '''')    as ConstraintType,                                         ' +
    '    rc.RDB$RELATION_NAME                      as TableName,                                              ' +
    '    s.RDB$FIELD_NAME                          as FieldName,                                              ' +
    '    coalesce(i2.RDB$RELATION_NAME, '''')      as ForeignTable,                                           ' +
    '    coalesce(s2.RDB$FIELD_NAME, '''')         as ForeignField,                                           ' +
    '    coalesce(refc.RDB$UPDATE_RULE, '''')      as UpdateRule,                                             ' +
    '    coalesce(refc.RDB$DELETE_RULE, '''')      as DeleteRule,                                             ' +
    '    (s.RDB$FIELD_POSITION + 1)                as FieldPosition                                           ' +
    'from                                                                                                     ' +
    '    RDB$INDEX_SEGMENTS s                                                                                 ' +
    '        left join RDB$INDICES i on i.RDB$INDEX_NAME = s.RDB$INDEX_NAME                                   ' +
    '        left join RDB$RELATION_CONSTRAINTS rc on rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME                    ' +
    '        left join RDB$REF_CONSTRAINTS refc on rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME          ' +
    '        left join RDB$RELATION_CONSTRAINTS rc2 on rc2.RDB$CONSTRAINT_NAME = refc.RDB$const_name_uq       ' +
    '        left join RDB$INDICES i2 on i2.RDB$INDEX_NAME = rc2.RDB$INDEX_NAME                               ' +
    '        left join RDB$INDEX_SEGMENTS s2 on i2.RDB$INDEX_NAME = s2.RDB$INDEX_NAME                         ' +
    '        left join RDB$relations rel on rel.RDB$RELATION_NAME = rc.RDB$RELATION_NAME                      ' +
    'where                                                                                                    ' +
    '  (rc.RDB$CONSTRAINT_TYPE IS NOT NULL)                                                                   ' +
    '  and (rel.RDB$SYSTEM_FLAG  is null  or rel.RDB$SYSTEM_FLAG  = 0)                                        ' +
    'order by                                                                                                 ' +
    '    rc.RDB$RELATION_NAME,                                                                                ' +
    '    rc.RDB$CONSTRAINT_NAME,                                                                              ' +
    '    s.RDB$FIELD_POSITION                                                                                 '
    ;



  SFirebirdProceduresSql =
    'select                                                                                    ' +
    '  RDB$PROCEDURE_NAME                     as ProcedureName,                                ' +
    '  case RDB$PROCEDURE_TYPE                                                                 ' +
    '  	when 1 then ''Selectable''                                                             ' +
    '  	when 2 then ''Executable''                                                             ' +
    '  end                                    as ProcedureType,                                ' +
    '  RDB$PROCEDURE_INPUTS		     as InputCount,                                    ' +
    '  RDB$PROCEDURE_OUTPUTS		     as OutputCount,                                   ' +
    '  RDB$PROCEDURE_SOURCE		     as Definition,                                    ' +
    '  coalesce(RDB$VALID_BLR, 0)             as IsValid                                       ' +
    'from                                                                                      ' +
    '  RDB$PROCEDURES                                                                          ' +
    'where                                                                                     ' +
    '	(RDB$PROCEDURES.RDB$SYSTEM_FLAG  is null  or RDB$PROCEDURES.RDB$SYSTEM_FLAG  = 0)      '
    ;

  SFirebirdSequencesSql =
    'select                                                 ' +
    '  RDB$GENERATOR_NAME        as SequenceName,           ' +
    '  RDB$GENERATOR_ID          as CurrentValue,           ' +
    '  RDB$INITIAL_VALUE         as InitialValue,           ' +
    '  RDB$GENERATOR_INCREMENT   as IncrementBy             ' +
    'from                                                   ' +
    '  RDB$GENERATORS                                       ' +
    'where                                                  ' +
    '  (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG  = 0)    '
    ;


  // ---------------------------------------------------------------------------------------------------------
  // MsSql
  // ---------------------------------------------------------------------------------------------------------

  SMsSqlTablesSql =
   'select                                              ' +
   '	TABLE_SCHEMA 			as SchemaName,   ' +
   '	TABLE_NAME			as TableName    ' +
   'from                                                ' +
   '	INFORMATION_SCHEMA.TABLES                       ' +
   'where                                               ' +
   '  	TABLE_TYPE = ''BASE TABLE''                     '
   ;


  SMsSqlViewsSql =
   'select                                                            ' +
   '	t.TABLE_SCHEMA 			as SchemaName,                 ' +
   '	t.TABLE_NAME			as TableName,                 ' +
   '	m.definition                    as Definition                 ' +
   'from                                                              ' +
   '	INFORMATION_SCHEMA.TABLES t                                   ' +
   '        left join SYS.VIEWS v on v.name = t.TABLE_NAME            ' +
   '        left join SYS.SQL_MODULES m on m.object_id = v.object_id  ' +
   'where                                                             ' +
   '  	t.TABLE_TYPE = ''VIEW''                                       '
   ;



   SMsSqlFieldsSql =
   'select                                                                                  ' +
   '    c.TABLE_SCHEMA                                      as SchemaName,                   ' +
   '    c.TABLE_NAME                                        as TableName,                   ' +
   '    c.COLUMN_NAME                                       as FieldName,                   ' +
   '    c.DATA_TYPE                                         as DataType,                    ' +
   '    ''''                                                as DataSubType,                 ' +
   '    case c.IS_NULLABLE when ''YES'' then 1 else 0 end   as IsNullable,                  ' +
   '    coalesce(c.CHARACTER_MAXIMUM_LENGTH, 0)             as SizeInChars,                 ' +
   '    coalesce(c.CHARACTER_OCTET_LENGTH, 0)               as SizeInBytes,                 ' +
   '    coalesce(c.NUMERIC_PRECISION, 0)                    as DecimalPrecision,            ' +
   '    coalesce(c.NUMERIC_SCALE, 0)                        as DecimalScale,                ' +
   '    coalesce(c.COLUMN_DEFAULT, '''')                    as DefaultValue,                ' +
   '    coalesce(cc.Definition, '''')                       as Expression,                  ' +
   '    c.ORDINAL_POSITION                                  as OrdinalPosition              ' +
   'from                                                                                    ' +
   '    INFORMATION_SCHEMA.COLUMNS c                                                        ' +
   '        left join INFORMATION_SCHEMA.TABLES t  on c.TABLE_SCHEMA = t.TABLE_SCHEMA       ' +
   '                and c.TABLE_NAME = t.TABLE_NAME                                         ' +
   '		left join sys.computed_columns cc on cc.name = c.COLUMN_NAME                ' +
   'where                                                                                   ' +
   '    TABLE_TYPE = ''BASE TABLE''                                                         ' +
   'order by                                                                                ' +
   '   c.TABLE_NAME,                                                                        ' +
   '   c.ORDINAL_POSITION                                                                   '
   ;

   SMsSqlIndexesSql =
   'select                                                                                                    ' +
   '    SCHEMA_NAME(t.schema_id) 		as SchemaName,                                                 ' +
   '    t.name					as TableName,                                                 ' +
   '    ind.name				as IndexName,                                                 ' +
   '    col.name 				as FieldName,                                                 ' +
   '    ic.key_ordinal				as FieldPosition,                                             ' +
   '    is_unique				as IsUnique,                                                  ' +
   '    ind.type_desc				as IndexType,                                                 ' +
   '    is_primary_key				as IsPrimary                                                  ' +
   'from                                                                                                      ' +
   '	sys.indexes ind                                                                                       ' +
   '        inner join sys.index_columns ic on  ind.object_id = ic.object_id and ind.index_id = ic.index_id   ' +
   '        inner join sys.columns col on ic.object_id = col.object_id and ic.column_id = col.column_id       ' +
   '        inner join sys.tables t on ind.object_id = t.object_id                                            ' +
   'where                                                                                                     ' +
   '	 t.is_ms_shipped = 0 AND                                                                              ' +
   '	 ic.is_included_column = 0                                                                            ' +
   'order by                                                                                                  ' +
   '     t.name,                                                                                              ' +
   '     ind.name,                                                                                            ' +
   '     ic.key_ordinal                                                                                       '
   ;

   SMsSqlTriggersSql   =
   'select                                                                         ' +
   '    tr.name                                                 as TriggerName,    ' +
   '    SCHEMA_NAME(t.schema_id)                       	        as SchemaName,      ' +
   '    t.name                                                  as TableName,      ' +
   '    tt.IsAfter + tt.IsInsert + tt.IsUpdate + tt.IsDelete    as TriggerType,    ' +
   '    tr.is_disabled                                          as IsInactive,     ' +
   '    OBJECT_DEFINITION(tr.object_id)                         as Definition      ' +
   'from                                                                           ' +
   '    sys.triggers as tr                                                         ' +
   '        inner join sys.tables t on tr.parent_id = t.object_id                  ' +
   '        inner join (                                                           ' +
   'select                                                                         ' +
   '    tr.name                                             as TriggerName,        ' +
   '    t.name                                              as TableName,          ' +
   '    case                                                                       ' +
   '        OBJECTPROPERTY(tr.object_id, ''ExecIsAfterTrigger'')                   ' +
   '            when 1 then ''after ''                                             ' +
   '            else ''before '' end                           as IsAfter,         ' +
   '    case                                                                       ' +
   '        OBJECTPROPERTY(tr.object_id, ''ExecIsInsertTrigger'')                  ' +
   '            when 1 then ''insert ''                                            ' +
   '            else '''' end                                 as IsInsert,         ' +
   '    case                                                                       ' +
   '        OBJECTPROPERTY(tr.object_id, ''ExecIsUpdateTrigger'')                  ' +
   '            when 1 then ''update ''                                            ' +
   '            else '''' end                                 as IsUpdate,         ' +
   '    case                                                                       ' +
   '        OBJECTPROPERTY(tr.object_id, ''ExecIsDeleteTrigger'')                  ' +
   '            when 1 then ''delete ''                                            ' +
   '            else '''' end                                 as IsDelete          ' +
   'from                                                                           ' +
   '    sys.triggers as tr                                                         ' +
   '        inner join sys.tables t on tr.parent_id = t.object_id                  ' +
   '        ) as tt on tt.TableName = t.name and tt.TriggerName = tr.name          '
   ;

   SMsSqlConstraintsSql =
   'select distinct                                                                                                                           ' +
   '    Constraints.constraint_schema				as SchemaName,                                                                 ' +
   '    Constraints.constraint_name                             as ConstraintName,                                                            ' +
   '    Constraints.constraint_type                             as ConstraintType,                                                            ' +
   '    KeyColumns.table_name					as TableName,                                                                 ' +
   '    KeyColumns.column_name					as FieldName,                                                                 ' +
   '    coalesce(Constraints2.table_name, '''')                 as ForeignTable,                                                              ' +
   '	coalesce(ForeignInfo.ForeignField, '''') 		as ForeignField,                                                              ' +
   '    coalesce(Constraints2.table_schema, '''')               as ForeignSchema,                                                             ' +
   '    coalesce(Refs.update_rule, '''')			as UpdateRule,                                                                ' +
   '    coalesce(Refs.delete_rule, '''')			as DeleteRule,                                                                ' +
   '    KeyColumns.ordinal_position				as FieldPosition                                                              ' +
   'from                                                                                                                                      ' +
   '    INFORMATION_SCHEMA.TABLE_CONSTRAINTS as Constraints                                                                                   ' +
   '		inner join INFORMATION_SCHEMA.KEY_COLUMN_USAGE as KeyColumns                                                                  ' +
   '			on (Constraints.constraint_catalog = KeyColumns.constraint_catalog or Constraints.constraint_catalog is null)         ' +
   '			and Constraints.constraint_schema = KeyColumns.constraint_schema                                                      ' +
   '			and Constraints.constraint_name = KeyColumns.constraint_name                                                          ' +
   '			and Constraints.table_name = KeyColumns.table_name                                                                    ' +
   '		left join INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS as Refs                                                                  ' +
   '			on (Constraints.constraint_catalog = Refs.constraint_catalog or Constraints.constraint_catalog is null)               ' +
   '			and Constraints.constraint_schema = Refs.constraint_schema                                                            ' +
   '			and Constraints.constraint_name = Refs.constraint_name                                                                ' +
   '		left join INFORMATION_SCHEMA.TABLE_CONSTRAINTS as Constraints2                                                                ' +
   '			on (Constraints2.constraint_catalog = Refs.constraint_catalog or Constraints2.constraint_catalog is null)             ' +
   '			and Constraints2.constraint_schema = Refs.unique_constraint_schema                                                    ' +
   '			and Constraints2.constraint_name = Refs.unique_constraint_name                                                        ' +
   '		left join (                                                                                                                   ' +
   'select                                                                                                                                     ' +
   '  ForeignKeys.name                                  as ConstraintName,                                                                     ' +
   '  SCHEMA_NAME(ForeignKeys.schema_id)                as SchemaName,                                                                          ' +
   '  OBJECT_NAME(ForeignKeys.parent_object_id)         as TableName,                                                                          ' +
   '  ParentColumns.name                                as FieldName,                                                                          ' +
   '  OBJECT_NAME(ForeignKeys.referenced_object_id)     as ForeignTable,                                                                       ' +
   '  RefColums.name                                    as ForeignField                                                                        ' +
   'from                                                                                                                                       ' +
   '  sys.foreign_keys as ForeignKeys                                                                                                          ' +
   '    inner join sys.foreign_key_columns as ForeignKeyColumns                                                                                ' +
   '            on  ForeignKeys.parent_object_id = ForeignKeyColumns.parent_object_id                                                          ' +
   '            and ForeignKeys.object_id = ForeignKeyColumns.constraint_object_id                                                             ' +
   '            and ForeignKeys.referenced_object_id = ForeignKeyColumns.referenced_object_id                                                  ' +
   '    inner join sys.columns as ParentColumns                                                                                                ' +
   '            on  ForeignKeyColumns.parent_object_id = ParentColumns.object_id                                                               ' +
   '            and ForeignKeyColumns.parent_column_id = ParentColumns.column_id                                                               ' +
   '    inner join sys.columns as RefColums                                                                                                    ' +
   '            on  ForeignKeyColumns.referenced_object_id = RefColums.object_id                                                               ' +
   '            and ForeignKeyColumns.referenced_column_id = RefColums.column_id                                                               ' +
   ') ForeignInfo                                                                                                                              ' +
   '	on  ForeignInfo.ConstraintName = Constraints.constraint_name                                                                          ' +
   '	and ForeignInfo.SchemaName = Constraints.constraint_schema                                                                      ' +
   '	and ForeignInfo.TableName = KeyColumns.table_name                                                                                     ' +
   '	and ForeignInfo.FieldName = KeyColumns.column_name                                                                                    ' +
   'order by                                                                                                                                   ' +
   '    KeyColumns.table_name,                                                                                                                 ' +
   '    Constraints.constraint_name,                                                                                                           ' +
   '    ordinal_position                                                                                                                       '
   ;

   SMsSqlProceduresSql   =
   'select                                                                          ' +
   '  r.SPECIFIC_SCHEMA              as SchemaName,                                  ' +
   '  r.SPECIFIC_NAME                as ProcedureName,                              ' +
   '  r.ROUTINE_TYPE                 as ProcedureType,                              ' +
   '  r.ROUTINE_DEFINITION           as Definition                                  ' +
   'from                                                                            ' +
   '  INFORMATION_SCHEMA.ROUTINES r                                                 ' +
   '  	left join sys.procedures p on p.name  = r.SPECIFIC_NAME and p.type = ''P''  ' +
   'where                                                                           ' +
   '	is_ms_shipped = 0                                                           '
   ;







implementation

end.


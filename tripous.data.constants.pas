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
    '  RDB$OWNER_NAME    as OwnerName                      ' +
    'from                                                  ' +
    '  RDB$RELATIONS                                       ' +
    'where                                                 ' +
    '  (RDB$SYSTEM_FLAG is null or RDB$SYSTEM_FLAG = 0)    ' +
    '  and RDB$VIEW_BLR is null                            ' +
    'order by                                              ' +
    '  RDB$RELATION_NAME                                   ' +
    ''
    ;

  SFirebirdTablesAndFieldsSql =
    'select                                                                                    ' +
    '  t.RDB$OWNER_NAME                                as OwnerName,                           ' +
    '  tf.RDB$RELATION_NAME                            as TableName,                           ' +
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
    '    t.RDB$OWNER_NAME                        as OwnerName,                               ' +
    '    i.RDB$RELATION_NAME                     as TableName,                               ' +
    '    i.RDB$INDEX_NAME                        as IndexName,                               ' +
    '    isg.RDB$FIELD_NAME                      as FieldName,                               ' +
    '    (isg.RDB$FIELD_POSITION + 1)            as FieldPosition,                           ' +
    '    i.RDB$UNIQUE_FLAG                       as IsUnique,                                ' +
    '    coalesce(rc.RDB$CONSTRAINT_TYPE, '''')  as IndexType,                               ' +
    '    i.RDB$SEGMENT_COUNT                     as FieldCount,                              ' +
    '    coalesce(i.RDB$INDEX_INACTIVE, 0)       as IsInactive,                              ' +
    '    case i.RDB$INDEX_TYPE                                                               ' +
    '        when 1 then 1                                                                   ' +
    '        else 0                                                                          ' +
    '    end                                     as IsDescending,                            ' +
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

   SMsSqlTablesAndFieldsSql =
   'select                                                                                  ' +
   '    c.TABLE_SCHEMA                                      as OwnerName,                   ' +
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



























implementation

end.


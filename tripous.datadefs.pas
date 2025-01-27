unit Tripous.DataDefs;

{$mode DELPHI}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
interface

uses
  Classes
  ,SysUtils
  ,DateUtils
  ,DB
  ,SQLDB

  ,TypInfo

  ,Tripous
  ,Tripous.MemTable
  ,Tripous.Data
  ;

type
  { The data-type of a data field }
  TDataFieldType =  (
    { Unknown }
    dftUnknown = 0,
    { String (nvarchar, varchar) }
    dftString = 1,
    { Integer }
    dftInteger = 2,
    { Float (float, double precision, etc) }
    dftFloat = 4,
    { Decimal (decimal(18, 4))
      Implied Precision and Scale <c>(18, 4)
      Example: @DECIMAL becomes decimal(18, 4)  }
    dftDecimal = 8,
    { Decimal (decimal(?, ?))
      The user provides the Precision and Scale explicitly.
      Example: @DECIMAL_(10, 2) becomes <c>decimal(10, 2)  }
    dftDecimal_ = $10,
    { Date (date) }
    dftDate = $20,
    { DateTime (datetime, timestamp, etc) }
    dftDateTime = $40,
    { Boolean (integer always, 1 = true, else false) }
    dftBoolean = $80,
    { Blob }
    dftBlob = $100,
    { Text Blob }
    dftTextBlob = $200
  );

  { Indicates the kind of the aggregate function to be used }
  TAggregateFunctionType = (
    aftNone = 0,
    aftCount = 1,
    aftAvg = 2,
    aftSum = 4,
    aftMax = 8,
    aftMin = $10
  );

  { The display type of a column. Used with grids. }
  TColumnDisplayType = (
    { Whatever the underlying field is }
    cdtDefault = 0,
    cdtDateTime = 1,
    cdtDate = 2,
    cdtTime = 3,
    cdtCheckBox = 4,
    cdtMemo = 5,
    cdtImage = 6
  );

  { Indicates how to construct a range between two dates.  }
  TDateRange = (
    { Custom range }
    drCustom,

    { From today To today }
    drToday,
    { From yesterday To today }
    drYesterday,
    { From today To Tomorrow }
    drTomorrow,

    // From Today To ...

    drLastWeek,
    drLastTwoWeeks,
    drLastMonth,
    drLastTwoMonths,
    drLastThreeMonths,
    drLastSemester,
    drLastYear,
    drLastTwoYears,

    // From ... To Today

    drNextWeek,
    drNextTwoWeeks,
    drNextMonth,
    drNextTwoMonths,
    drNextThreeMonths,
    drNextSemester,
    drNextYear,
    drNextTwoYears
  );

  { Indicates how the user enters of selects the criterion value }
  TSqlFilterMode = (
  { None }
  sfmNone = 0,
  { Indicates that the user enters the criterion value into an input control.  }
  sfmSimple = 1,
  { Indicates that the user selects the criterion value from a list of values returned by a SELECT statement  }
  sfmEnumQuery = 2,
  { Indicates that the user selects the criterion value from a constant list of values given at design time }
  sfmEnumConst = 4,
  { Indicates that the user selects the criterion value by using a LocatorBox Ui to issue a lookup select }
  sfmLocator = 8
  );

  { DateRanges }
  DateRanges = class
  public const
    PrefixFrom = 'FROM_DATE_RANGE_';
    PrefixTo = 'TO_DATE_RANGE_';
    WhereRanges = [drCustom,
                   drToday,
                   drYesterday,
                   drLastWeek,
                   drLastTwoWeeks,
                   drLastMonth,
                   drLastTwoMonths,
                   drLastThreeMonths,
                   drLastSemester,
                   drLastYear,
                   drLastTwoYears];

  public
    { Converts a TDateRange value to two DateTime values.  }
    class function ToDates(Range: TDateRange; Today: TDateTime; var FromDate: TDateTime; var ToDate: TDateTime): Boolean;
    { True if Range denotes a past time (Today included)   }
    class function IsPast(Range: TDateRange): Boolean;

    { Creates an array of ListerItem items based on WhereRanges. NOTE: For use with ComboBoxes, etc. }
    class function GetWhereRangeItems(): TListerItems;
  end;


  { TSqlFilterDef }
  { Describes a filter of an Sql statement }
  TSqlFilterDef = class(TCollectionItem)
  private
    FAggregateFunc: string;
    FDataType: TDataFieldType;
    FEnumDisplayLabels: string;
    FEnumIncludeAll: Boolean;
    FEnumIsMultiChoise: Boolean;
    FEnumOptionList: TStrings;
    FEnumResultField: string;
    FEnumSql: string;
    FFieldPath: string;
    FInitialValue: string;
    FLocator: string;
    FMode: TSqlFilterMode;
    FPutInHaving: Boolean;

    FTitle: string;
    FTitleKey: string;
    FUseRange: Boolean;
    function GetAggregateFunc: string;
  public
    class var
      ValidAggregateFunctions : TStrings;  // '', 'count', 'avg', 'sum', 'max', 'min'
  public
    constructor Create(ACollection: TCollection = nil); override;
    destructor Destroy(); override;

    { construction }
    class constructor Create();
    class destructor Destroy();

    { Throws exception if this instance is not a valid one. }
    procedure CheckDescriptor();

    { Sets the value of a property of this instance. Returns this instance. }
    function SetUseRange(Value: Boolean = True): TSqlFilterDef;
    { Sets the value of a property of this instance. Returns this instance. }
    function SetPutInHaving(Value: Boolean = True): TSqlFilterDef;
    { Sets the value of a property of this instance. Returns this instance. }
    function SetAggregateFunc(Value: string = 'count'): TSqlFilterDef;
    { Sets the value of a property of this instance. Returns this instance. }
    function SetInitialValue(Value: string): TSqlFilterDef;
  published
    { The full path to the field, i.e. TableAlias.FieldName, or just FieldName }
    property FieldPath: string read FFieldPath write FFieldPath;
    property Title: string read FTitle write FTitle;
    property TitleKey: string read FTitleKey write FTitleKey;

    { Gets or sets the "data type" of the filter. }
    property DataType: TDataFieldType read FDataType write FDataType;
    { Indicates how the user enters of selects the filter value }
    property Mode: TSqlFilterMode read FMode write FMode;

    { Valid ONLY when Mode is Simple and DataType String, Float or Integer.
      Date and Time are ALWAYS used as a range from-to.
      Defaults to false }
    property UseRange: Boolean read FUseRange write FUseRange;
    { Gets or sets the name of the locator descriptor, used when this is of a Locator DataType    }
    property Locator: string read FLocator write FLocator;
    { When true then the produced CommandText goes to the HAVING clause, instead of the WHERE clause }
    property PutInHaving: Boolean read FPutInHaving write FPutInHaving;
    { Gets or sets the aggregation function (sum, count, avg, min, max) to use, when <see cref="PutInHaving"/> is true.
      It could be an empty string. }
    property AggregateFunc: string read GetAggregateFunc write FAggregateFunc;


    { enum properties - Valid ONLY when Mode is Enum and DataType is String or Integer }

    { Gets or sets the SQL SELECT statement of a TSqlFilterMode.EnumQuery criterion.  }
    property EnumSql: string read FEnumSql write FEnumSql;
    { Ges or sets the result field  }
    property EnumResultField: string read FEnumResultField write FEnumResultField;            //  = "Id";
    { For EnumConst and EnumQuery only items. When true the user interface presents
      a multi choise control, otherwise a combo box is presented. }
    property EnumIsMultiChoise: Boolean read FEnumIsMultiChoise write FEnumIsMultiChoise;
    { Gets the list of constant options. Used only when the filter is a <see cref="SqlFilterMode.EnumConst"/>  filter. }
    property EnumOptionList: TStrings read FEnumOptionList write FEnumOptionList;        //   new List<string>();

    { When true, constant options are displayed initially to the user as checked.  }
    property EnumIncludeAll: Boolean read FEnumIncludeAll write FEnumIncludeAll;
    { A list where each line is FIELD_NAME=Title   }
    property EnumDisplayLabels: string read FEnumDisplayLabels write FEnumDisplayLabels;

    property InitialValue: string read FInitialValue write FInitialValue;
  end;


  { TSqlFilterDefsEnumerator }
  TSqlFilterDefsEnumerator = class(TEnumeratorBase)
  private
    function GetCurrent: TSqlFilterDef;
  public
    property Current: TSqlFilterDef read GetCurrent;
  end;

  { TSqlFilterDefs }
  TSqlFilterDefs = class(TPersistent)
  private
    FList: TCollection;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSqlFilterDef;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetMultiChoise(Value: Boolean);
    procedure CheckDescriptors();

    procedure Clear();

    function Add(TableName: string; FieldName: string; TitleKey: string; DataType: TDataFieldType; Mode: TSqlFilterMode = sfmSimple): TSqlFilterDef; overload;
    function Add(FieldPath: string; TitleKey: string; DataType: TDataFieldType; Mode: TSqlFilterMode = sfmSimple): TSqlFilterDef; overload;
    function Add(TableName: string; FieldName: string; TitleKey: string; DataType: TDataFieldType): TSqlFilterDef; overload;
    function AddLocator(FieldPath: string; TitleKey: string; Locator: string; DataType: TDataFieldType): TSqlFilterDef;

    procedure Add(Item: TSqlFilterDef); overload;
    procedure Remove(Item: TSqlFilterDef);

    function  IndexOf(Item: TSqlFilterDef): Integer;
    function  Contains(const FieldPath: string): Boolean;
    function  Find(const FieldPath: string): TSqlFilterDef;

    function GetEnumerator(): TSqlFilterDefsEnumerator;

    property Items[Index: Integer]: TSqlFilterDef read GetItem ; default;
    property Count: Integer read GetCount;
  published
    property List: TCollection read FList write FList;
  end;


  { TSelectSqlColumn }
  TSelectSqlColumn = class(TCollectionItem)
  private
    FAggregate: TAggregateFunctionType;
    FAggregateFormat: string;
    FDecimals: Integer;
    FDisplayIndex: Integer;
    FDisplayType: TColumnDisplayType;
    FFormatString: string;
    FGroupIndex: Integer;
    FName: string;
    FReadOnly: Boolean;
    FTitle: string;
    FTitleKey: string;
    FVisible: Boolean;
    FWidth: Integer;
  public
    constructor Create(ACollection: TCollection = nil); override;
    destructor Destroy(); override;

    { Throws an exception if this descriptor is not fully defined  }
    procedure CheckDescriptor();
    { Returns a format string. It uses the FormatString property first, and then the Decimals property.    }
    function  GetDisplayFormat(): string;
  published
    { The name of the field this column represents }
    property Name: string read FName write FName;
    property DisplayType: TColumnDisplayType read FDisplayType write FDisplayType;  // cdtDefault

    property Title: string read FTitle write FTitle;
    property TitleKey: string read FTitleKey write FTitleKey;

    property Visible: Boolean read FVisible write FVisible;                         // True
    property Width: Integer read FWidth write FWidth;                               // 90
    property ReadOnly: Boolean read FReadOnly write FReadOnly;                      // True
    property DisplayIndex: Integer read FDisplayIndex write FDisplayIndex;          // 0
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;                // -1 , -1 means not defined
    property Decimals: Integer read FDecimals write FDecimals;                      // -1 , -1 means not defined
    property FormatString: string read FFormatString write FFormatString;

    property Aggregate: TAggregateFunctionType read FAggregate write FAggregate;    // aftNone
    property AggregateFormat: string read FAggregateFormat write FAggregateFormat;
  end;

  { TSelectSqlColumnsEnumerator }
  TSelectSqlColumnsEnumerator = class(TEnumeratorBase)
  private
    function GetCurrent: TSelectSqlColumn;
  public
    property Current: TSelectSqlColumn read GetCurrent;
  end;

  { TSelectSqlColumns }
  TSelectSqlColumns = class(TPersistent)
  private
    FList: TCollection;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSelectSqlColumn;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure CheckDescriptors();

    procedure Clear();

    function Add(FieldName: string; TitleKey: string = ''; DisplayType: TColumnDisplayType = cdtDefault ): TSelectSqlColumn; overload;

    procedure Add(Item: TSelectSqlColumn); overload;
    procedure Remove(Item: TSelectSqlColumn);

    function  IndexOf(Item: TSelectSqlColumn): Integer;
    function  Contains(const FieldName: string): Boolean;
    function  Find(const FieldName: string): TSelectSqlColumn;

    function GetEnumerator(): TSelectSqlColumnsEnumerator;

    property Items[Index: Integer]: TSelectSqlColumn read GetItem ; default;
    property Count: Integer read GetCount;
  published
    property List: TCollection read FList write FList;
  end;

  { TSelectSqlParser }
  { A simple SELECT Sql parser. It parses a SELECT statement into its constituents parts.
    The SELECT statement can contain nested sub-SELECTs or column names in double quotes or angle brackets.}
  TSelectSqlParser = class
  private
    type
      TToken = (toNone, toSelect, toFrom, toWhere, toGroupBy, toHaving, toOrderBy);
  private
    FTokens : array of string; // = ['select', 'from', 'where', 'group by', 'having', 'order by'];
    FText: string;
    FSelect: string;
    FFrom: string;
    FWhere: string;
    FGroupBy: string;
    FHaving: string;
    FOrderBy: string;

    FCurToken: TToken;
    FCurPos: Integer;
    FLastPos: Integer;
    FTextLength : Integer;

    { Skips characters by increasing the current position until the SkipChar is found.}
    function  SkipToChar(SkipChar: Char): Integer;
    { Returns true if sToken is at the current position in text }
    function  FindTokenAtCurrentPos(sToken: string): Boolean;
    { Performs a "token change". Makes the NewToken the curToken,
      copies a part of the text string setting the last clause string
      and adjusts curPos, lastPos and curToken. }
    procedure TokenChange(NewToken: TToken; sNewToken: string);
    { The actual parsing procedure. Returns true only when finds
      one of the token strings (SELECT, FROM, WHERE etc).}
    function  NextTokenEnd(): Boolean;
    { Parses the passed in text }
    procedure DoParse(Text: string);
  public
    constructor Create(Text: string = '');

    { Parses Text into the constituent parts of a SELECT SQL statement. }
    procedure Parse(Text: string);
    procedure Clear();
    function ToString(): string; override;

    property Select   : string   read FSelect   write FSelect ;
    property From     : string   read FFrom     write FFrom   ;
    property Where    : string   read FWhere    write FWhere  ;
    property GroupBy  : string   read FGroupBy  write FGroupBy;
    property Having   : string   read FHaving   write FHaving ;
    property OrderBy  : string   read FOrderBy  write FOrderBy;
  end;

  { TSelectSql }
  TSelectSql = class(TCollectionItem)
  public const
     LB = sLineBreak;
     SPACE = ' ';
     SPACES = '  ';
     DefaultName = 'SelectSql';
  private
    FColumns: TSelectSqlColumns;
    FFilters: TSqlFilterDefs;
    FCompanyAware: boolean;
    FConnectionName: string;
    FDateRange: TDateRange;
    FDateRangeColumn: string;

    FFrom: string;
    FGroupBy: string;
    FHaving: string;
    FName: string;
    FOrderBy: string;
    FSelect: string;
    FTable: TMemTable;
    FTag: TObject;
    FTitle: string;
    FTitleKey: string;
    FWhere: string;
    FWhereUser: string;

    function  GetIsEmpty: Boolean;
    procedure SetSqlText(Value: string);
  public
    constructor Create(ACollection: TCollection = nil); override; overload;
    constructor Create(SqlText: string); overload;
    destructor Destroy(); override;

    { Adds a Carriage Return (LineBreak) after S }
    class  function CR(S: string): string;
    { Replaces any trailing comma with space in S, ignoring trailing spaces.   }
    class  procedure ReplaceLastComma(var S: string);
    { Concatenates Clause + Delimiter + Plus }
    class  function AddTo(Clause: string; Delimiter: string; Plus: string): string;
    { Concatenates Keyword + Clause  }
    class  function NormalizeClause(Clause: string; Keyword: string): string;
    { Returns true if Value contains any of the mask characters (%, ?, *)  }
    class  function IsMasked(Value: string): Boolean;

    { Constructs a DateTime param pair (date range params) suitable for thw WHERE part in a SelectSql }
    class  function DateRangeConstructWhereParams(Range: TDateRange; FieldName: string): string;
    { Replaces any DateTime param pair (date range params) found in SqlText with actual values.  }
    class procedure DateRangeReplaceWhereParams(var SqlText: string; Provider: TSqlProvider);

    { Throws an exception if this descriptor is not fully defined }
    procedure CheckDescriptor();
    procedure Clear();
    procedure Assign(Source: TPersistent); override;
    function  Clone(): TSelectSql;

    { Concatenates Keyword + Clause for all clauses }
    function  GetSqlText: string;

    procedure AddToWhere(Plus: string); overload;
    procedure AddToWhere(Plus: string; Delimiter: string);  overload;
    procedure OrToWhere(Plus: string);
    procedure AddToGroupBy(Plus: string);
    procedure AddToHaving(Plus: string);
    procedure AddToOrderBy(Plus: string);

    { Returns concatenated the SELECT and FROM clauses only. }
    function  SelectFromToString(): string;

    { Parses StatementText and assigns its clause properties. }
    procedure Parse(SqlText: string);
    { Creates a select * from TableName statement and then calls Parse() }
    procedure ParseFromTableName(TableName: string);
    { Tries to get the main table name from the statement }
    function  GetMainTableName(): string;

    { Sets-up the column types, the captions and the visibility of Table.Columns. }
    procedure SetupTable(Table: TDataset);

    property Select   : string   read FSelect   write FSelect ;
    property From     : string   read FFrom     write FFrom   ;
    property Where    : string   read FWhere    write FWhere  ;
    property WhereUser: string   read FWhereUser write FWhereUser;
    property GroupBy  : string   read FGroupBy  write FGroupBy;
    property Having   : string   read FHaving   write FHaving ;
    property OrderBy  : string   read FOrderBy  write FOrderBy;

    { Returns true if the statement is empty }
    property IsEmpty  : Boolean  read GetIsEmpty;
    { The DataTable that results after the select execution }
    property Table    : TMemTable read FTable write FTable;
    { A user defined value. }
    property Tag      : TObject   read FTag write FTag;
  published
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property TitleKey: string read FTitleKey write FTitleKey;

    property Text: string read GetSqlText write SetSqlText;
    property CompanyAware: boolean read FCompanyAware write FCompanyAware;
    property ConnectionName: string read FConnectionName write FConnectionName;

    { A fully qualified (i.e. TABLE_NAME.FIELD_NAME) column of type date or datetime.
      It works in conjuction with DateRange property in order to produce
      a fixed part in the WHERE clause of this select statement. }
    property DateRangeColumn: string read FDateRangeColumn write FDateRangeColumn; // ''
    { It works in conjuction with DateRangeColumn property in order to produce
      a fixed part in the WHERE clause of this select statement. }
    property DateRange: TDateRange read FDateRange write FDateRange;               // drLastWeek

    { The list of column descriptors of columns to display.
      If null or empty, then all columns are displayed.
      Else only the columns defined in this list are displayed.}
    property Columns: TSelectSqlColumns read FColumns write FColumns;
    { The filter descriptors used to generate the "user where" clause.
      User's where is appended to the WHERE clause.    }
    property Filters: TSqlFilterDefs read FFilters write FFilters;

(*

*)
  end;


implementation

{ DateRanges }

class function DateRanges.ToDates(Range: TDateRange; Today: TDateTime; var FromDate: TDateTime; var ToDate: TDateTime): Boolean;
begin
  Result := True;

  FromDate := Today;
  ToDate := Today;

  case Range of
    drToday              : ;
    drYesterday          : begin FromDate := FromDate - 1; ToDate := ToDate - 1; end;
    drTomorrow           : begin FromDate := FromDate + 1; ToDate := ToDate + 1; end;

    drLastWeek           : FromDate := FromDate - 7;
    drLastTwoWeeks       : FromDate := FromDate - 14;
    drLastMonth          : FromDate := FromDate - 30;
    drLastTwoMonths      : FromDate := FromDate - 60;
    drLastThreeMonths    : FromDate := FromDate - 90;
    drLastSemester       : FromDate := FromDate - 180;
    drLastYear           : FromDate := FromDate - 365;
    drLastTwoYears       : FromDate := FromDate - 730;

    drNextWeek           : ToDate := ToDate + 7;
    drNextTwoWeeks       : ToDate := ToDate + 14;
    drNextMonth          : ToDate := ToDate + 30;
    drNextTwoMonths      : ToDate := ToDate + 60;
    drNextThreeMonths    : ToDate := ToDate + 90;
    drNextSemester       : ToDate := ToDate + 180;
    drNextYear           : ToDate := ToDate + 365;
    drNextTwoYears       : ToDate := ToDate + 730;
    else
      Result := False;
  end;
end;

class function DateRanges.IsPast(Range: TDateRange): Boolean;
begin
  Result := Range in [
    drToday,
    drYesterday,

    drLastWeek,
    drLastTwoWeeks,
    drLastMonth,
    drLastTwoMonths,
    drLastThreeMonths,
    drLastSemester,
    drLastYear,
    drLastTwoYears
  ];
end;

class function DateRanges.GetWhereRangeItems(): TListerItems;
var
  P : PTypeInfo;
  EnumName: string;
  Range : TDateRange;
  Title : string;
  Ordinal: Integer;
  ItemId : string;
begin
  P := TypeInfo(TDateRange);
  //TypeName := P^.Name;
  Result := TListerItems.Create();

  for Range in WhereRanges do
  begin
    Ordinal := Ord(Range);
    ItemId  := IntToStr(Ordinal);

    EnumName := GetEnumName(P, Ordinal);
    EnumName := EnumName.Remove(1, 2);
    Title := Sys.SplitCamelCase(EnumName);
    Result.Add(ItemId, Title, Ordinal);
  end;
end;



{ TSqlFilterDef }

function TSqlFilterDef.GetAggregateFunc: string;
begin
  if ValidAggregateFunctions.IndexOf(LowerCase(fAggregateFunc)) <> -1 then
     Result := LowerCase(fAggregateFunc)
  else
     Result := '';
end;

constructor TSqlFilterDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FEnumResultField := 'Id';
  FEnumOptionList := TStringList.Create();
end;

destructor TSqlFilterDef.Destroy();
begin
  FEnumOptionList.Free();
  inherited Destroy();
end;

class constructor TSqlFilterDef.Create();
begin
  ValidAggregateFunctions := TStringList.Create();
  ValidAggregateFunctions.AddStrings(['', 'count', 'avg', 'sum', 'max', 'min']);
end;

class destructor TSqlFilterDef.Destroy();
begin
  ValidAggregateFunctions.Free();
end;

procedure TSqlFilterDef.CheckDescriptor();
var
  FirstPart : string;
  sFormat   : string;
  Lines     : TStringArray;
  i         : Integer;
begin
  if Sys.IsEmpty(FieldPath) then
     Sys.Error('SqlFilterDef must have a FieldPath');

  FirstPart := Format('Invalid SqlFilterDef: %s ', [FieldPath]);

  if (Mode in [sfmEnumConst, sfmEnumQuery]) and not (DataType in [dftString, dftInteger]) then
     Sys.Error(FirstPart +  'Invalid data type. Only string and integer is allowed.');

  if Mode = sfmEnumConst then
  begin
    if (not Assigned(EnumOptionList)) or (EnumOptionList.Count = 0) then
       Sys.Error(FirstPart +  'Enum Constant: Option List not defined.');
  end else if Mode = sfmEnumQuery then
  begin
    if Sys.IsEmpty(EnumResultField) then
      Sys.Error(FirstPart + 'Enum Query. Result Field Name not defined.');

    if Sys.IsEmpty(EnumSql) then
       Sys.Error(FirstPart + 'Enum Query. SELECT Sql is not defined.');

    if not Sys.IsEmpty(EnumDisplayLabels) then
    begin
       sFormat := 'Invalid field titles in line %s ';

       Lines := EnumDisplayLabels.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);
       for i := Low(Lines) to High(Lines) do
         if not Lines[i].Contains('=') then
           Sys.Error(Format(FirstPart + sFormat, [i + 1]));
    end;

  end;

end;

function TSqlFilterDef.SetUseRange(Value: Boolean): TSqlFilterDef;
begin
  UseRange := Value;
  Result := Self;
end;

function TSqlFilterDef.SetPutInHaving(Value: Boolean): TSqlFilterDef;
begin
  PutInHaving := Value;
  Result := Self;
end;

function TSqlFilterDef.SetAggregateFunc(Value: string): TSqlFilterDef;
begin
  AggregateFunc := Value;
  Result := Self;
end;

function TSqlFilterDef.SetInitialValue(Value: string): TSqlFilterDef;
begin
  InitialValue := Value;
  Result := Self;
end;


{ TSqlFilterDefsEnumerator }

function TSqlFilterDefsEnumerator.GetCurrent: TSqlFilterDef;
begin
  Result := FItemList[Position] as TSqlFilterDef;
end;

{ TSqlFilterDefs }

constructor TSqlFilterDefs.Create();
begin
  inherited Create();
  FList := TCollection.Create(TSqlFilterDef);
end;

destructor TSqlFilterDefs.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TSqlFilterDefs.SetMultiChoise(Value: Boolean);
var
  Item: TSqlFilterDef;
begin
  for Item in Self do
    if Item.Mode in [sfmEnumQuery, sfmEnumConst] then
       Item.EnumIsMultiChoise := Value;
end;

procedure TSqlFilterDefs.CheckDescriptors();
var
  Item: TSqlFilterDef;
begin
  for Item in Self do
    Item.CheckDescriptor();
end;

procedure TSqlFilterDefs.Clear();
begin
   FList.Clear();
end;

function TSqlFilterDefs.Add(TableName: string; FieldName: string; TitleKey: string; DataType: TDataFieldType; Mode: TSqlFilterMode): TSqlFilterDef;
begin
  Result := Add(Sys.FieldPath(TableName, FieldName), TitleKey, DataType, Mode);
end;

function TSqlFilterDefs.Add(FieldPath: string; TitleKey: string; DataType: TDataFieldType; Mode: TSqlFilterMode): TSqlFilterDef;
begin
  Result := TSqlFilterDef.Create(Self.List);
  Result.FieldPath := FieldPath;
  Result.TitleKey := TitleKey;
  Result.Mode := Mode;
  Result.DataType := DataType;
end;

function TSqlFilterDefs.Add(TableName: string; FieldName: string; TitleKey: string; DataType: TDataFieldType): TSqlFilterDef;
begin
  Result := Add(Sys.FieldPath(TableName, FieldName), TitleKey, DataType);
end;

function TSqlFilterDefs.AddLocator(FieldPath: string; TitleKey: string; Locator: string; DataType: TDataFieldType): TSqlFilterDef;
begin
  Result := Add(FieldPath, TitleKey, DataType, sfmLocator);
  Result.Locator := Locator;
end;

function TSqlFilterDefs.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSqlFilterDefs.GetItem(Index: Integer): TSqlFilterDef;
begin
  Result := FList.Items[Index] as TSqlFilterDef;
end;

procedure TSqlFilterDefs.Add(Item: TSqlFilterDef);
begin
  Item.Collection := FList;
end;

procedure TSqlFilterDefs.Remove(Item: TSqlFilterDef);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
     FList.Delete(Index);
end;

function TSqlFilterDefs.IndexOf(Item: TSqlFilterDef): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if Item = FList.Items[i] then
      Exit(i); // =>
  end;
end;

function TSqlFilterDefs.Contains(const FieldPath: string): Boolean;
begin
   Result := Find(FieldPath) <> nil;
end;

function TSqlFilterDefs.Find(const FieldPath: string): TSqlFilterDef;
var
  i : Integer;
  Item : TSqlFilterDef;
begin
  Result := nil;

  for i := 0 to FList.Count - 1 do
  begin
    Item := FList.Items[i] as TSqlFilterDef;
    if Sys.IsSameText(FieldPath, Item.FieldPath) then
      Exit(Item);  // =>
  end;

end;

function TSqlFilterDefs.GetEnumerator(): TSqlFilterDefsEnumerator;
begin
  Result := TSqlFilterDefsEnumerator.Create(Sys.CollectionToArray(Self.List));
end;

{ TSelectSqlColumn }

constructor TSelectSqlColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  Visible := True;
  Width   := 90;
  ReadOnly:= True;
  DisplayIndex  := 0;
  GroupIndex    := -1;
  Decimals      := -1;
  Aggregate     := aftNone;
end;

destructor TSelectSqlColumn.Destroy();
begin
  inherited Destroy();
end;

procedure TSelectSqlColumn.CheckDescriptor();
begin
  if Sys.IsEmpty(Name) then
    Sys.Error('TSelectSqlColumn Name is empty');
end;

function TSelectSqlColumn.GetDisplayFormat(): string;
begin
  Result := '';

  if not Sys.IsEmpty(FormatString) then
    Result := FormatString
  else if Decimals > 0 then
  begin
    Result := '#,#';
    Result := Result + '0.'.PadRight(Decimals, '0');
  end;
end;

{ TSelectSqlColumnsEnumerator }

function TSelectSqlColumnsEnumerator.GetCurrent: TSelectSqlColumn;
begin
  Result := FItemList[Position] as TSelectSqlColumn;
end;


{ TSelectSqlColumns }
constructor TSelectSqlColumns.Create();
begin
  inherited Create();
  FList := TCollection.Create(TSelectSqlColumn);
end;

destructor TSelectSqlColumns.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TSelectSqlColumns.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSelectSqlColumns.GetItem(Index: Integer): TSelectSqlColumn;
begin
  Result := FList.Items[Index] as TSelectSqlColumn;
end;

procedure TSelectSqlColumns.CheckDescriptors();
var
  Item: TSelectSqlColumn;
begin
  for Item in Self do
    Item.CheckDescriptor();
end;

procedure TSelectSqlColumns.Clear();
begin
  FList.Clear();
end;

function TSelectSqlColumns.Add(FieldName: string; TitleKey: string; DisplayType: TColumnDisplayType): TSelectSqlColumn;
begin
  Result := TSelectSqlColumn.Create(FList);
  Result.Name := FieldName;
  Result.TitleKey := TitleKey;
  Result.DisplayType := DisplayType;
end;

procedure TSelectSqlColumns.Add(Item: TSelectSqlColumn);
begin
  Item.Collection := FList;
end;

procedure TSelectSqlColumns.Remove(Item: TSelectSqlColumn);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
     FList.Delete(Index);
end;

function TSelectSqlColumns.IndexOf(Item: TSelectSqlColumn): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if Item = FList.Items[i] then
      Exit(i); // =>
  end;
end;

function TSelectSqlColumns.Contains(const FieldName: string): Boolean;
begin
  Result := Find(FieldName) <> nil;
end;

function TSelectSqlColumns.Find(const FieldName: string): TSelectSqlColumn;
var
  i : Integer;
  Item : TSelectSqlColumn;
begin
  Result := nil;

  for i := 0 to FList.Count - 1 do
  begin
    Item := FList.Items[i] as TSelectSqlColumn;
    if Sys.IsSameText(FieldName, Item.Name) then
      Exit(Item);  // =>
  end;
end;

function TSelectSqlColumns.GetEnumerator(): TSelectSqlColumnsEnumerator;
begin
   Result := TSelectSqlColumnsEnumerator.Create(Sys.CollectionToArray(Self.List));
end;





{ TSelectSqlParser }
constructor TSelectSqlParser.Create(Text: string);
begin
  inherited Create();

   FTokens    := ['select', 'from', 'where', 'group by', 'having', 'order by'];
   Parse(Text);
end;

function TSelectSqlParser.SkipToChar(SkipChar: Char): Integer;
var
  C: Char;

begin
  Inc(FCurPos);

  while FCurPos <= Length(FText) - 1 do
  begin
    C := FText[FCurPos];
    if C = SkipChar then
    begin
      Inc(FCurPos);
      break;
    end;

    Inc(FCurPos);
  end;

  Result := FCurPos;

end;

function TSelectSqlParser.FindTokenAtCurrentPos(sToken: string): Boolean;
begin
  Result := False;
  if FCurPos + Length(sToken) <= Length(FText) - 1 then
  try
    Result := string.Compare(FText, FCurPos, sToken, 1, Length(sToken), [coIgnoreCase]) = 0;
  except
  end;
end;

procedure TSelectSqlParser.TokenChange(NewToken: TToken; sNewToken: string);
var
  Len : Integer;
  StartPos: Integer;
begin
  StartPos := FLastPos - 1;
  Len      := FCurPos - FLastPos;

  case FCurToken of
    toSelect  : Select  := Trim(FText.Substring(StartPos, Len));
    toFrom    : From    := Trim(FText.Substring(StartPos, Len));
    toWhere   : Where   := Trim(FText.Substring(StartPos, Len));
    toGroupBy : GroupBy := Trim(FText.Substring(StartPos, Len));
    toHaving  : Having  := Trim(FText.Substring(StartPos, Len));
    toOrderBy : OrderBy := Trim(FText.Substring(StartPos, Len));
  end;

  FCurPos   := FCurPos + Length(sNewToken);
  FLastPos  := FCurPos;
  FCurToken := NewToken;

end;

function TSelectSqlParser.NextTokenEnd(): Boolean;
var
  ParenCount: Integer;
  C: Char;
  i : Integer;
  NewToken: TToken;
begin

  ParenCount := 0;

  FTextLength := Length(FText);

  while FCurPos <= FTextLength - 1 do
  begin
    C := FText[FCurPos];

    if C = '"' then
       FCurPos := SkipToChar('"')
    else if C = '[' then
       FCurPos := SkipToChar(']')
    else if C = '(' then
    begin
      Inc(FCurPos);
      Inc(ParenCount);
    end else if C = ')' then
    begin
      Inc(FCurPos);
      Dec(ParenCount);
      if ParenCount < 0 then
        Sys.Error('TSelectSqlParser: Wrong parentheses');
    end else if (not (FText[FCurPos] in [#0..#32])) and ((FCurPos - 1 >= 1) and (FText[FCurPos - 1] in [#0..#32])) then
    begin

      if ParenCount = 0 then
      begin
        for i := 0 to Length(FTokens) - 1 do
        begin
          if FindTokenAtCurrentPos(FTokens[i]) then
          begin
            NewToken := TToken(i + 1);
            TokenChange(NewToken, FTokens[i]);
            Exit(True);  //=>
          end;
        end;
      end;

      Inc(FCurPos);
    end else begin
      Inc(FCurPos);
    end;

  end;

  Exit(False);

end;

procedure TSelectSqlParser.DoParse(Text: string);
begin
  FCurPos    := 1;
  FLastPos   := 1;

  FCurToken  := toNone;
  FText := ' ' + Text + ' ';

  while FCurToken <= toOrderBy do
  begin
    if not NextTokenEnd() then
       break;
  end;

  TokenChange(toNone, '');
end;

procedure TSelectSqlParser.Parse(Text: string);
begin
  Clear();
  if not Sys.IsEmpty(Text) then
    DoParse(Text);
end;

procedure TSelectSqlParser.Clear();
begin
  Select   := '';
  From     := '';
  Where    := '';
  GroupBy  := '';
  Having   := '';
  OrderBy  := '';
end;

function TSelectSqlParser.ToString(): string;
var
  SB: IStringBuilder;
begin
  SB := TStrBuilder.Create();
  SB.AppendLine(Format('select   : %s'   , [Select ]));
  SB.AppendLine(Format('from     : %s'   , [From   ]));
  SB.AppendLine(Format('where    : %s'   , [Where  ]));
  SB.AppendLine(Format('group by : %s'   , [GroupBy]));
  SB.AppendLine(Format('having   : %s'   , [Having ]));
  SB.AppendLine(Format('order by : %s'   , [OrderBy]));

  Result := SB.ToString();
end;




{ TSelectSql }

constructor TSelectSql.Create(ACollection: TCollection = nil);
begin
  inherited Create(ACollection);

  FColumns := TSelectSqlColumns.Create();
  FFilters := TSqlFilterDefs.Create();

  DateRange := drLastWeek;
  DateRangeColumn := '';
end;

constructor TSelectSql.Create(SqlText: string);
begin
  Create(nil);
  Text := SqlText;
end;

destructor TSelectSql.Destroy();
begin
  if Assigned(FColumns) then FColumns.Free();
  FFilters.Free();
  inherited Destroy();
end;

class function TSelectSql.CR(S: string): string;
begin
  Result := S.TrimRight() + LB;
end;

class procedure TSelectSql.ReplaceLastComma(var S: string);
var
  i : Integer;
begin
  i := Length(S);

  if i > 0 then
  begin
    while (i > 0) and (S[i] in Sys.WhiteSpace) do
      Dec(i);

    if S[i] = ',' then
      S[i] := ' ';
  end;
end;

class function TSelectSql.AddTo(Clause: string; Delimiter: string; Plus: string): string;
begin

  if Sys.IsEmpty(Plus) then
     Plus := '';

  if (not Sys.IsEmpty(Clause)) and (not Sys.IsEmpty(Delimiter)) then
  begin
    Clause := Clause.TrimRight();
    Delimiter := Delimiter.Trim();
    Plus := Plus.Trim();

    if Clause.EndsWith(Delimiter, True) or Plus.StartsWith(Delimiter, True) then
       Exit(CR(Clause) + SPACES + Plus)
    else
       Exit(CR(Clause) + SPACES + Delimiter + SPACE + Plus);
  end;

  Result := SPACES + Plus.Trim();

end;

class function TSelectSql.NormalizeClause(Clause: string; Keyword: string): string;
begin
  if (not Sys.IsEmpty(Clause)) and  (not Sys.IsEmpty(Keyword)) then
  begin
    Clause := Clause.Trim();
    Keyword := Keyword.Trim();

    if not Clause.StartsWith(Keyword, True) then
      Exit(CR(Keyword) + SPACES + Clause);
  end;

  if not Sys.IsEmpty(Clause) then
     Result := Clause
  else
     Result := '';
end;

class function TSelectSql.IsMasked(Value: string): Boolean;
begin
  Result := Sql.IsMasked(Value);
end;

class function TSelectSql.DateRangeConstructWhereParams(Range: TDateRange; FieldName: string): string;
var
  sFrom : string;
  sTo   : string;
begin
  sFrom := ':' + DateRanges.PrefixFrom + Sys.GetEnumName(TypeInfo(TDateRange), Ord(Range));
  sTo   := ':' + DateRanges.PrefixTo   + Sys.GetEnumName(TypeInfo(TDateRange), Ord(Range));

  Result := Format(' ((%s >= %s) and (%s <= %s)) ', [FieldName, sFrom, FieldName, sTo]) ;
end;

class procedure TSelectSql.DateRangeReplaceWhereParams(var SqlText: string; Provider: TSqlProvider);
var
  P        : PTypeInfo;
  EnumName : string;
  Ordinal  : Integer;
  sFrom    : string;
  sTo      : string;
  Range    : TDateRange;

  function Replace(Range: TDateRange; S: string): string;
  var
    FromDate: TDateTime;
    ToDate  : TDateTime;

    sFromValue : string;
    sToValue   : string;
  begin
    FromDate := Date();
    ToDate   := Date();

    DateRanges.ToDates(Range, Date(),  FromDate, ToDate);

    sFromValue := Provider.QSDateTime(StartOfTheDay(FromDate));
    sToValue   := Provider.QSDateTime(EndOfTheDay(ToDate));

    S := S.Replace(sFrom, sFromValue);
    S := S.Replace(sTo, sToValue);

    Result := S;
  end;

begin
  if not SqlText.Contains(DateRanges.PrefixFrom) then
    Exit; // =>

  sFrom := '';
  sTo   := '';

  P := TypeInfo(TDateRange);

  for Range in DateRanges.WhereRanges do
  begin
    Ordinal  := Ord(Range);
    EnumName := GetEnumName(P, Ordinal);
    EnumName := EnumName.Remove(1, 2);

    sFrom := ':' + DateRanges.PrefixFrom + EnumName;

    if SqlText.Contains(sFrom) then
    begin
      sTo := ':' + DateRanges.PrefixTo + EnumName;
      SqlText := Replace(Range, SqlText);
      Exit; // =>
    end;
  end;

end;

procedure TSelectSql.CheckDescriptor();
begin

end;

procedure TSelectSql.Clear();
begin

end;

procedure TSelectSql.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TSelectSql.Clone(): TSelectSql;
begin

end;

function TSelectSql.GetSqlText: string;
begin
  // TODO: TSelectSql.GetSqlText
end;

procedure TSelectSql.AddToWhere(Plus: string);
begin

end;

procedure TSelectSql.AddToWhere(Plus: string; Delimiter: string);
begin

end;

procedure TSelectSql.OrToWhere(Plus: string);
begin

end;

procedure TSelectSql.AddToGroupBy(Plus: string);
begin

end;

procedure TSelectSql.AddToHaving(Plus: string);
begin

end;

procedure TSelectSql.AddToOrderBy(Plus: string);
begin

end;

function TSelectSql.SelectFromToString(): string;
begin

end;

procedure TSelectSql.SetSqlText(Value: string);
begin
  Parse(Value);
end;

procedure TSelectSql.Parse(SqlText: string);
var
  Parser: TSelectSqlParser;
begin
  Select   := '';
  From     := '';
  Where    := '';
  WhereUser:= '';
  GroupBy  := '';
  Having   := '';
  OrderBy  := '';

  if not Sys.IsEmpty(SqlText) then
  begin
    Parser := TSelectSqlParser.Create('');
    try
      Parser.Parse(SqlText);
      Select   := Parser.Select ;
      From     := Parser.From   ;
      Where    := Parser.Where  ;
      GroupBy  := Parser.GroupBy;
      Having   := Parser.Having ;
      OrderBy  := Parser.OrderBy;
    finally
      Parser.Free();
    end;
  end;
end;

procedure TSelectSql.ParseFromTableName(TableName: string);
begin

end;

function TSelectSql.GetMainTableName(): string;
begin

end;

procedure TSelectSql.SetupTable(Table: TDataset);
begin

end;

function TSelectSql.GetIsEmpty: Boolean;
begin
  Result := Sys.IsEmpty(Text);
end;

end.


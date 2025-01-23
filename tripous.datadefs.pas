unit Tripous.DataDefs;

{$mode DELPHI}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
interface

uses
  Classes
  , SysUtils

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
  public
    constructor Create(ACollection: TCollection = nil); override;
    destructor Destroy(); override;
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
  TSelectSql = class
  private
    FCompanyAware: boolean;
    FConnectionName: string;
    FFrom: string;
    FGroupBy: string;
    FHaving: string;
    FName: string;
    FOrderBy: string;
    FSelect: string;
    FTitle: string;
    FTitleKey: string;
    FWhere: string;
    FWhereUser: string;
    procedure Parse(SqlText: string);
    function  GetSqlText: string;
    procedure SetSqlText(Value: string);
  public
    constructor Create(SqlText: string);
    destructor Destroy(); override;

    property Select   : string   read FSelect   write FSelect ;
    property From     : string   read FFrom     write FFrom   ;
    property Where    : string   read FWhere    write FWhere  ;
    property WhereUser: string   read FWhereUser write FWhereUser;
    property GroupBy  : string   read FGroupBy  write FGroupBy;
    property Having   : string   read FHaving   write FHaving ;
    property OrderBy  : string   read FOrderBy  write FOrderBy;
  published
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property TitleKey: string read FTitleKey write FTitleKey;

    property Text: string read GetSqlText write SetSqlText;
    property CompanyAware: boolean read FCompanyAware write FCompanyAware;
    property ConnectionName: string read FConnectionName write FConnectionName;
  end;


implementation

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
end;

destructor TSelectSqlColumn.Destroy();
begin
  inherited Destroy();
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

constructor TSelectSql.Create(SqlText: string);
begin
  inherited Create();
  Text := SqlText;
end;

destructor TSelectSql.Destroy();
begin
  inherited Destroy();
end;

function TSelectSql.GetSqlText: string;
begin
  // TODO: TSelectSql.GetSqlText
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

end.


unit Tripous.Data;

{$MODE DELPHI}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
   Classes
  ,SysUtils
  ,Variants
  ,DB
  ,bufdataset
  ,sqldb

  ,Tripous
  ,Tripous.MemTable

  ,Dialogs
  ;

type
  TMetaNodeType = (
    ntNone,
    ntDatabases,
    ntDatabase,
    ntTables,
    ntTable,
    ntFields,
    ntField,
    ntIndexes,
    ntIndex,
    ntConstraints,
    ntConstraint,
    ntViews,
    ntView,
    ntTriggers,
    ntTrigger,
    ntProcedures,
    ntProcedure,
    ntSequences,
    ntSequence
  );



  TSqlProviderType = (
    ptNone,
    ptFirebird,
    ptSqlite,
    ptMsSql,
    ptMySql,
    ptPostgreSql,
    ptOracle
  );

  TSqlProviderTypeArray = array of TSqlProviderType;


  TSetOfFieldType    = set of TFieldType;



  { forward }
  TSqlProvider = class;
  TSqlConnectionInfo = class;
  TSqlConnectionInfoArray = array of TSqlConnectionInfo;
  TMetaNode = class;
  TMetaNodeArray = array of TMetaNode;
  TMetaDatabase = class;
  TSqlStore = class;

  { TMetaNode }
  TMetaNode = class
  private
    FName: string;
    FNodeType: TMetaNodeType;
    FDatabase: TMetaDatabase;
    FParentNode: TMetaNode;
  protected
    FTag: TObject;
    FIsContainer: Boolean;
    function GetDisplayText: string; virtual;
    function GetNodes: TMetaNodeArray;  virtual;

    procedure DoClear(); virtual;

    function  ListToArray(List: TGenObjectList<TMetaNode>): TMetaNodeArray; virtual;
    function  UnQuote(const S: string): string;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string; ANodeType: TMetaNodeType);
    destructor Destroy(); override;

    property Database: TMetaDatabase read FDatabase;
    property NodeType: TMetaNodeType read FNodeType;
    property IsContainer: Boolean read FIsContainer;
    property Name: string read FName;
    property DisplayText: string read GetDisplayText;

    property Nodes: TMetaNodeArray read GetNodes;

    property ParentNode: TMetaNode read FParentNode;
    property Tag: TObject read FTag write FTag;
  end;

  { TMetaField }

  TMetaField = class(TMetaNode)
  private
    FDataSubType: string;
    FDataType: string;
    FDefaultValue: string;
    FExpression: string;
    FIsNullable: Boolean;
    FOrdinalPosition: Integer;
    FPrecision: Integer;
    FScale: Integer;
    FSizeInBytes: Integer;
    FSizeInChars: Integer;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property DataType: string read FDataType;
    property DataSubType: string read FDataSubType;
    property IsNullable: Boolean read FIsNullable;
    property SizeInChars: Integer read FSizeInChars;
    property SizeInBytes: Integer read FSizeInBytes;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
    property DefaultValue: string read FDefaultValue;
    property Expression: string read FExpression;    // when is a calculated field
    property OrdinalPosition: Integer read FOrdinalPosition;
  end;

  { TMetaFields }

  TMetaFields = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaField>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(FieldName: string): TMetaField;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const FieldName: string): TMetaField;

    property List: TGenObjectList<TMetaField> read FList;
  end;

  { TMetaIndex }

  TMetaIndex = class(TMetaNode)
  private
    FFieldCount: Integer;
    FFields: string;
    FForeignKey: string;
    FIndexType: string;
    FIsActive: Boolean;
    FIsDescending: Boolean;
    FIsUnique: Boolean;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property IndexType: string read FIndexType;
    property Fields: string read FFields;
    property FieldCount: Integer read FFieldCount;
    property IsActive: Boolean read FIsActive;
    property IsUnique: Boolean read FIsUnique;
    property IsDescending: Boolean read FIsDescending;
    property ForeignKey: string read FForeignKey;
  end;

  { TMetaIndexes }

  TMetaIndexes = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaIndex>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(IndexName: string): TMetaIndex;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const IndexName: string): TMetaIndex;

    property List: TGenObjectList<TMetaIndex> read FList;
  end;

  { TMetaTrigger }

  TMetaTrigger = class(TMetaNode)
  private
    FDefinition: string;
    FIsActive: Boolean;
    FIsValid: Boolean;
    FTableName: string;
    FTriggerType: string;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property TriggerType: string read FTriggerType;
    property TableName: string read FTableName;
    property Definition: string read FDefinition;
    property IsActive: Boolean read FIsActive;
    property IsValid: Boolean read FIsValid;
  end;

  { TMetaTriggers }

  TMetaTriggers = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaTrigger>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;

    function Add(IndexName: string): TMetaTrigger;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const TriggerName: string): TMetaTrigger;

    property List: TGenObjectList<TMetaTrigger> read FList;
  end;


  { TMetaTable }

  TMetaTable = class(TMetaNode)
  private
    FFields: TMetaFields;
    FIndexes: TMetaIndexes;
    FTriggers: TMetaTriggers;
  protected
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property Fields: TMetaFields read FFields;
    property Indexes: TMetaIndexes read FIndexes;
    property Triggers: TMetaTriggers read FTriggers;
  end;

  { TMetaTables }
  TMetaTables = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaTable>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(TableName: string): TMetaTable;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const TableName: string): TMetaTable;

    property List: TGenObjectList<TMetaTable> read FList;
  end;

  { TMetaView }

  TMetaView = class(TMetaNode)
  private
    FDefinition: string;
    FFields: TMetaFields;
  protected
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property Fields: TMetaFields read FFields;
    property Definition: string read FDefinition;
  end;

  { TMetaViews }

  TMetaViews = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaView>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(ViewName: string): TMetaView;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const ViewName: string): TMetaView;

    property List: TGenObjectList<TMetaView> read FList;
  end;

  { TMetaProcedure }

  TMetaProcedure = class(TMetaNode)
  private
    FDefinition: string;
    FIsValid: Boolean;
    FProcedureType: string;
  protected
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property ProcedureType: string read FProcedureType;
    property Definition: string read FDefinition;
    property IsValid: Boolean read FIsValid;
  end;

  { TMetaProcedures }

  TMetaProcedures = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaProcedure>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(ProcedureName: string): TMetaProcedure;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const ProcedureName: string): TMetaProcedure;

    property List: TGenObjectList<TMetaProcedure> read FList;
  end;


  { TMetaSequence }

  TMetaSequence = class(TMetaNode)
  private
    FCurrentValue: Integer;
    FInitialValue: Integer;
    FIncrementBy: Integer;
  protected
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; const AName: string);
    destructor Destroy(); override;

    property CurrentValue: Integer read FCurrentValue;
    property InitialValue: Integer read FInitialValue;
    property IncrementBy: Integer read FIncrementBy;
  end;

  { TMetaSequences }

  TMetaSequences = class(TMetaNode)
  private
     FList: TGenObjectList<TMetaSequence>;
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
    function Add(AName: string): TMetaSequence;
  public
    constructor Create(AParentNode: TMetaNode);
    destructor Destroy(); override;

    function Find(const AName: string): TMetaSequence;

    property List: TGenObjectList<TMetaSequence> read FList;
  end;

  { TMetaDatabase }

  TMetaDatabase = class(TMetaNode)
  private
    FConnectionInfo: TSqlConnectionInfo;
    FProcedures: TMetaProcedures;
    FSequences: TMetaSequences;
    FTables: TMetaTables;
    FViews: TMetaViews;
    FSqlStore: TSqlStore;
    FLoaded: Boolean;
    FLoading: Boolean;
    FSqlProviderType: TSqlProviderType;

    procedure LoadField(tblSql: TMemTable; MetaField: TMetaField);

    procedure LoadTables();
    procedure LoadViews();
    procedure LoadIndexes();
    procedure LoadTriggers();
    procedure LoadProcedures();
    procedure LoadSequences();
  protected
    function GetNodes: TMetaNodeArray;  override;
    procedure DoClear(); override;
  public
    constructor Create(AParentNode: TMetaNode; ConnectionInfo: TSqlConnectionInfo);
    destructor Destroy(); override;

    procedure Clear();
    procedure Load();

    function FindTable(const TableName: string): TMetaTable;

    property ConnectionInfo: TSqlConnectionInfo read FConnectionInfo;

    property Tables: TMetaTables read FTables;
    property Views: TMetaViews read FViews;
    property Procedures: TMetaProcedures read FProcedures;
    property Sequences: TMetaSequences read FSequences;

    property Loading: Boolean read FLoading;
    property Loaded: Boolean read FLoaded;

    property SqlStore: TSqlStore read FSqlStore;
    property SqlProviderType: TSqlProviderType read FSqlProviderType;
  end;

  { TMetaDatabases }

  TMetaDatabases = class(TMetaNode)
  private
    FList: TGenObjectList<TMetaDatabase>;
  protected
    function GetNodes: TMetaNodeArray;  override;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Clear();
    procedure ReLoad();

    function  Add(ConnectionInfo: TSqlConnectionInfo): TMetaDatabase;
    procedure AddRange(ConnectionInfoList: TSqlConnectionInfoArray);
    procedure Remove(ADatabase: TMetaDatabase);

    property List: TGenObjectList<TMetaDatabase> read FList;
  end;


   { TSqlConnectionInfo }
  TSqlConnectionInfo = class(TCollectionItem)
  private
    FAutoCreateGenerators: Boolean;
    FCharSet: string;
    FConnectionString: string;
    FConnectorType: string;
    FDatabaseName: string;
    FHostName: string;
    FName: string;
    FPassword: string;
    FProvider: string;
    FUserName: string;
    FParams: TStrings;
    function GetConnectorType: string;
    function GetProvider: string;
    procedure SetConnectionString(AValue: string);
  public
    constructor Create(ACollection: TCollection = nil); override;
    destructor Destroy(); override;

    function Clone(): TSqlConnectionInfo;

    function  GetSqlProvider(): TSqlProvider;
    procedure SetupConnection(SqlConnector: TSQLConnector);

    property ConnectorType: string read GetConnectorType;
    property DatabaseName: string read FDatabaseName;
    property HostName: string read FHostName;
    property UserName: string read FUserName;
    property Password: string read FPassword;
    property CharSet: string read FCharSet;
    property Params: TStrings read FParams;
  published
    property Name: string read FName write FName;
    property Provider: string read GetProvider write FProvider;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property AutoCreateGenerators: Boolean read FAutoCreateGenerators write FAutoCreateGenerators;
  end;

  { TSqlConnectionInfoList }
  TSqlConnectionInfoList = class(TPersistent)
  private
    fSqlConnections: TCollection;
    function GetCount: Integer;
    function GetItem(Index: Integer): TSqlConnectionInfo;
  public
    const DefaultFilePath = 'SqlConnections.json';

    constructor Create();
    destructor Destroy(); override;

    procedure Clear();

    procedure Load(FilePath: string = DefaultFilePath);
    procedure Save(FilePath: string = DefaultFilePath);

    procedure Remove(Item: TSqlConnectionInfo);
    procedure Add(Item: TSqlConnectionInfo);

    function  IndexOf(Item: TSqlConnectionInfo): Integer;
    function  Contains(const Name: string): Boolean;
    function  Find(const Name: string): TSqlConnectionInfo;

    property Items[Index: Integer]: TSqlConnectionInfo read GetItem ; default;
    property Count: Integer read GetCount;
  published
    property SqlConnections: TCollection read fSqlConnections write fSqlConnections;
  end;

  { SqlProviders }

  SqlProviders = class
  private class var
    Providers : TList;
  public
    const SFirebird = 'Firebird';
    const SSqlite = 'Sqlite';
    const SMsSql = 'MsSql';
    const SMySql = 'MySql';
    const SPostgreSql = 'PostgreSql';
    const SOracle = 'Oracle';

    const ProviderNames: TStringArray = [SFirebird, SSqlite, SMsSql, SMySql, SPostgreSql, SOracle];
    const ProviderTypes: TSqlProviderTypeArray = [ptFirebird, ptSqlite, ptMsSql, ptMySql, ptPostgreSql, ptOracle];

    class constructor Create();
    class destructor Destroy();

    class function ProviderTypeToString(ProviderType: TSqlProviderType): string;
    class function StringToProviderType(ProviderType: string): TSqlProviderType;

    class function FindSqlProvider(const ProviderName: string): TSqlProvider;
  end;

  { TSqlProvider }
  TSqlProvider = class
  protected
    FName: string;
    FProviderType: TSqlProviderType;

    FTablesAndFieldsSql: string;
    FViewsAndFieldsSql: string;
    FIndexesSql: string;
    FTriggersSql: string;
    FProceduresSql: string;
    FSequencesSql: string;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    property Name: string read FName;
    property ProviderType: TSqlProviderType read FProviderType;

    // metadata SELECTs
    property TablesAndFieldsSql: string read FTablesAndFieldsSql;
    property ViewsAndFieldsSql: string read FViewsAndFieldsSql;
    property IndexesSql: string read FIndexesSql;
    property TriggersSql: string read FTriggersSql;
    property ProceduresSql: string read FProceduresSql;
    property SequencesSql: string read FSequencesSql;
  end;

  { TSqlProviderFirebird - 'User = SYSDBA; Psw = password; Database = C:\Program Files\Firebird\Firebird_5_0\examples\empbuild\EMPLOYEE.FDB';  }
  TSqlProviderFirebird = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlProviderSqlite }

  TSqlProviderSqlite = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlProviderMsSql

  Download the dblib_current.zip
  from https://downloads.freepascal.org/fpc/contrib/windows/
  or from ftp://ftp.freepascal.org/fpc/contrib/windows/
  and unzip it.

  In the unzip folder go to Win32 or Win64 folder
  get the dblib.dll
  and the libiconv.dll
  and place them in the same folder as your app's executable.

  SEE: https://wiki.freepascal.org/mssqlconn }
  TSqlProviderMsSql = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlProviderMySql }

  TSqlProviderMySql = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlProviderPostgreSql }

  TSqlProviderPostgreSql = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlProviderOracle }

  TSqlProviderOracle = class(TSqlProvider)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

  { TSqlStore }
  TSqlStore = class
  private
    FConnectionInfo  : TSqlConnectionInfo;
    FProvider        : TSqlProvider;

    FSqlConnector    : TSQLConnector;
    FSqlQuery        : TSQLQuery;
    FSqlTransaction  : TSQLTransaction;

    function GetConnectionName: string;
  protected
    procedure BeginOperation(const SqlText: string; Params: array of const); virtual;
    procedure EndOperation(); virtual;
  public
    constructor Create(ConnectionInfo: TSqlConnectionInfo);
    destructor Destroy(); override;

    function  Select(const SqlText: string; Params: array of const): TMemTable; overload;
    function  Select(const SqlText: string): TMemTable; overload;

    procedure SelectTo(Table: TDataset; const SqlText: string; Params: array of const); overload;
    procedure SelectTo(Table: TDataset; const SqlText: string); overload;

    function  SelectResults(SqlText: string; const ResultFields: string; const Params: array of const): Variant; overload;
    function  SelectResults(SqlText: string; const ResultFields: string): Variant; overload;

    function  SelectResult(SqlText: string; Default: Variant; const Params: array of const): Variant; overload;
    function  SelectResult(SqlText: string; Default: Variant): Variant; overload;
    function  SelectResult(SqlText: string): Variant; overload;

    function  IntegerResult(SqlText: string; Default: Integer; const Params: array of const): Integer; overload;
    function  IntegerResult(SqlText: string; Default: Integer): Integer; overload;
    function  IntegerResult(SqlText: string): Integer; overload;

    procedure ExecSql(const SqlText: string; Params: array of const); overload;
    procedure ExecSql(const SqlText: string); overload;

    procedure GetNativeSchema(SchemaName, TableName, SqlText: string; Table: TDataset);

    property ConnectionName: string read GetConnectionName;
    property ConnectionInfo: TSqlConnectionInfo read FConnectionInfo;
    property Provider: TSqlProvider read FProvider;
    property SqlQuery: TSQLQuery read FSqlQuery;

  end;

  { DbSys }
  DbSys = class
  private
    class var
      FMetaDatabases: TMetaDatabases;

    class function GetBCDFieldTypes: TSetOfFieldType; static;
    class function GetBlobFieldTypess: TSetOfFieldType; static;
    class function GetDateTimeFieldTypes: TSetOfFieldType; static;
    class function GetFloatFieldTypes: TSetOfFieldType; static;
    class function GetIntegerFieldTypes: TSetOfFieldType; static;
    class function GetMetaDatabases: TMetaDatabases; static;
    class function GetStringFieldTypes: TSetOfFieldType; static;
    class function GetWideStringFieldTypes: TSetOfFieldType; static;
  public
    { construction }
    class constructor Create();
    class destructor Destroy();

    class function  AsInteger(Table: TDataset; const FieldName: string; Default: Integer = -1): Integer;
    class function  AsFloat(Table: TDataset; const FieldName: string; Default: Double = 0): Double;
    class function  AsFloatString(Table: TDataset; const FieldName: string; Digits: Integer = 3; Default: string = ''): string;
    class function  AsString(Table: TDataset; const FieldName: string; Default: string = ''): string;
    class function  AsBoolean(Table: TDataset; const FieldName: string; Default: Boolean = False): Boolean;
    class function  AsDateTime(Table: TDataset; const FieldName: string; Default: TDateTime = 0): TDateTime;
    class procedure SafeSetFieldValue(Table: TDataset; FieldName: string; Value: Variant);

    class function  CreateField(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
    class function  CreateFieldLookUp(Dataset: TDataset; const FieldName, KeyFields: string; LookupDataSet: TDataset; LookupKeyFields, LookupResultField: string): TField;
    class function  CreateFieldInternalCalc(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
    class function  AddFieldDef(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; Required: Boolean = False): TFieldDef;

    class procedure CopyFieldStructure(SourceField: TField; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
    class procedure CopyDatasetStructure(Source: TDataset; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
    class procedure MergeStructure(Source, Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);

    class procedure CopyField(SourceField, DestField: TField);
    class procedure CopyFieldBlob(SourceField, DestField: TBlobField);
    class procedure CopyRecord(Source, Dest: TDataset);
    class procedure CopyRecordAppend(Source, Dest: TDataset);
    class procedure CopyDataset(Source, Dest: TDataset);
    class procedure EmptyDataset(Dataset: TDataset);
    class function  IdenticalSchema(Table1, Table2: TDataset): Boolean;

    class procedure StrToBlob(Field: TBlobField; const Data: string);
    class function  BlobToStr(Field: TBlobField): string;

    class procedure StreamToBlob(Field: TBlobField; Stream: TStream);
    class function  BlobToStream(Field: TBlobField): TStream;

    class function  FieldValuesToVariantArray(Dataset: TDataset;  const ResultFields: string): Variant;
    class procedure GetKeyValuesList(List: TStrings; Table: TDataset; const FieldName: string; ModValue: Integer; DiscardBelowZeroes: Boolean);

    class procedure AssignParams(Params: TParams; A: array of const); overload;
    class procedure AssignParams(Params: TParams; tblParams: TDataset); overload;
    class procedure AssignParams(Params: TParams; A: array of Variant); overload;
    class procedure AssignStreamToParam(Param: TParam; Stream: TStream);

    { properties }
    class property MetaDatabases: TMetaDatabases read GetMetaDatabases;
    class property StringFieldTypes  : TSetOfFieldType read GetStringFieldTypes;
    class property WideStringFieldTypes : TSetOfFieldType read GetWideStringFieldTypes;
    class property IntegerFieldTypes : TSetOfFieldType read GetIntegerFieldTypes;
    class property FloatFieldTypes : TSetOfFieldType read GetFloatFieldTypes;
    class property BCDFieldTypes : TSetOfFieldType read GetBCDFieldTypes;
    class property DateTimeFieldTypes : TSetOfFieldType read GetDateTimeFieldTypes;
    class property BlobFieldTypes  : TSetOfFieldType read GetBlobFieldTypess;
  end;



implementation


uses
   Tripous.Data.Constants
   ;







{ TMetaNode }

constructor TMetaNode.Create(AParentNode: TMetaNode; const AName: string; ANodeType: TMetaNodeType);
var
  Parent: TMetaNode;
begin
  inherited Create();
  FParentNode := AParentNode;
  FName := AName;
  FNodeType := ANodeType;

  Parent := FParentNode;
  while Assigned(Parent) do
  begin
    if Parent is TMetaDatabase then
    begin
      FDatabase := Parent as TMetaDatabase;
      break;
    end;

    Parent := Parent.FParentNode;
  end;

  FIsContainer := not (FNodeType in [ntField, ntIndex, ntConstraint, ntTrigger, ntSequence]);
end;

destructor TMetaNode.Destroy();
begin
  inherited Destroy();
end;

function TMetaNode.GetNodes: TMetaNodeArray;
begin
  Result := [];
end;

procedure TMetaNode.DoClear();
begin
end;

function TMetaNode.GetDisplayText: string;
begin
  Result := FName;
end;

function TMetaNode.ListToArray(List: TGenObjectList<TMetaNode>): TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  if (List <> nil) and (List.Count > 0) then
  begin
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
      Result[i] := TMetaNode(List[i]);
  end;
end;

function TMetaNode.UnQuote(const S: string): string;
const
  A: array of char = ['''', '"', '[', ']', ' ', #9, #10, #11, #12, #13 ];
begin
   Result := S.Trim(A);
end;



{ TMetaField }

constructor TMetaField.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntField);
end;

destructor TMetaField.Destroy();
begin
  inherited Destroy();
end;

{ TMetaFields }

constructor TMetaFields.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Fields', ntFields);
  FList := TGenObjectList<TMetaField>.Create(True, True);
end;

destructor TMetaFields.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaFields.DoClear();
begin
  FList.Clear();
end;

function TMetaFields.Add(FieldName: string): TMetaField;
begin
  Result := TMetaField.Create(Self, FieldName);
  FList.Add(Result);
end;

function TMetaFields.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaFields.Find(const FieldName: string): TMetaField;
var
  Item : TMetaField;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(FieldName, Item.Name) then
      Exit(Item);
  end;
end;

{ TMetaIndex }

constructor TMetaIndex.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntIndex);
end;

destructor TMetaIndex.Destroy();
begin
  inherited Destroy();
end;


{ TMetaIndexes }

constructor TMetaIndexes.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Indexes', ntIndexes);
  FList := TGenObjectList<TMetaIndex>.Create(True, True);
end;

destructor TMetaIndexes.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaIndexes.DoClear();
begin
  FList.Clear();
end;

function TMetaIndexes.Add(IndexName: string): TMetaIndex;
begin
  Result := TMetaIndex.Create(Self, IndexName);
  FList.Add(Result);
end;

function TMetaIndexes.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaIndexes.Find(const IndexName: string): TMetaIndex;
var
  Item : TMetaIndex;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(IndexName, Item.Name) then
      Exit(Item);
  end;
end;

{ TMetaTrigger }

constructor TMetaTrigger.Create(AParentNode: TMetaNode; const AName: string);
begin
   inherited Create(AParentNode, AName, ntTrigger);
end;

destructor TMetaTrigger.Destroy();
begin
  inherited Destroy();
end;

{ TMetaTriggers }

constructor TMetaTriggers.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Triggers', ntTriggers);
  FList := TGenObjectList<TMetaTrigger>.Create(True, True);
end;

destructor TMetaTriggers.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaTriggers.DoClear();
begin
  FList.Clear();
end;

function TMetaTriggers.Add(IndexName: string): TMetaTrigger;
begin
  Result := TMetaTrigger.Create(Self, IndexName);
  FList.Add(Result);
end;

function TMetaTriggers.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaTriggers.Find(const TriggerName: string): TMetaTrigger;
var
  Item : TMetaTrigger;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(TriggerName, Item.Name) then
      Exit(Item);
  end;
end;


{ TMetaTable }
constructor TMetaTable.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntTable);
  FFields := TMetaFields.Create(Self);
  FIndexes := TMetaIndexes.Create(Self);
  FTriggers := TMetaTriggers.Create(Self);
end;

destructor TMetaTable.Destroy();
begin
  FTriggers.Free();
  FIndexes.Free();
  FFields.Free();
  inherited Destroy();
end;

procedure TMetaTable.DoClear();
begin
  FFields.DoClear();
  FIndexes.DoClear();
end;

{ TMetaTables }

constructor TMetaTables.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Tables', ntTables);
  FList := TGenObjectList<TMetaTable>.Create(True, True);
end;

destructor TMetaTables.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

function TMetaTables.Find(const TableName: string): TMetaTable;
var
  Item: TMetaTable;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(TableName, Item.Name) then
      Exit(Item);
  end;
end;

function TMetaTables.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

procedure TMetaTables.DoClear();
begin
  FList.Clear();
end;

function TMetaTables.Add(TableName: string): TMetaTable;
begin
  Result := TMetaTable.Create(Self, TableName);
  FList.Add(Result);
end;





{ TMetaView }

constructor TMetaView.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntView);
  FFields := TMetaFields.Create(Self);
end;

destructor TMetaView.Destroy();
begin
  FFields.Free();
  inherited Destroy();
end;

procedure TMetaView.DoClear();
begin
  FFields.DoClear();
end;


{ TMetaViews }
constructor TMetaViews.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Views', ntViews);
  FList := TGenObjectList<TMetaView>.Create(True, True);
end;

destructor TMetaViews.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaViews.DoClear();
begin
  FList.Clear();
end;

function TMetaViews.Find(const ViewName: string): TMetaView;
var
  Item: TMetaView;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(ViewName, Item.Name) then
      Exit(Item);
  end;
end;

function TMetaViews.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaViews.Add(ViewName: string): TMetaView;
begin
  Result := TMetaView.Create(Self, ViewName);
  FList.Add(Result);
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntProcedure);
end;

destructor TMetaProcedure.Destroy();
begin
  inherited Destroy();
end;

procedure TMetaProcedure.DoClear();
begin
end;

{ TMetaProcedures }

constructor TMetaProcedures.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Procedures', ntProcedures);
  FList := TGenObjectList<TMetaProcedure>.Create(True, True);
end;

destructor TMetaProcedures.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaProcedures.DoClear();
begin
  FList.Clear();
end;

function TMetaProcedures.Add(ProcedureName: string): TMetaProcedure;
begin
  Result := TMetaProcedure.Create(Self, ProcedureName);
  FList.Add(Result);
end;

function TMetaProcedures.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaProcedures.Find(const ProcedureName: string): TMetaProcedure;
var
  Item: TMetaProcedure;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(ProcedureName, Item.Name) then
      Exit(Item);
  end;

end;

{ TMetaSequence }

constructor TMetaSequence.Create(AParentNode: TMetaNode; const AName: string);
begin
  inherited Create(AParentNode, AName, ntSequence);
end;

destructor TMetaSequence.Destroy();
begin
  inherited Destroy();
end;

procedure TMetaSequence.DoClear();
begin
end;

{ TMetaSequences }

constructor TMetaSequences.Create(AParentNode: TMetaNode);
begin
  inherited Create(AParentNode, 'Sequences', ntSequences);
  FList := TGenObjectList<TMetaSequence>.Create(True, True);
end;

destructor TMetaSequences.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

procedure TMetaSequences.DoClear();
begin
  FList.Clear();
end;

function TMetaSequences.Add(AName: string): TMetaSequence;
begin
  Result := TMetaSequence.Create(Self, AName);
  FList.Add(Result);
end;

function TMetaSequences.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;

function TMetaSequences.Find(const AName: string): TMetaSequence;
var
  Item: TMetaSequence;
begin
  Result := nil;
  for Item in FList do
  begin
    if Sys.IsSameText(AName, Item.Name) then
      Exit(Item);
  end;
end;

{ TMetaDatabase }

constructor TMetaDatabase.Create(AParentNode: TMetaNode; ConnectionInfo: TSqlConnectionInfo);
begin
  inherited Create(AParentNode, ConnectionInfo.Name, ntDatabase);
  FDatabase := Self;
  FConnectionInfo := ConnectionInfo.Clone();

  FSqlStore := TSqlStore.Create(ConnectionInfo);
  FSqlProviderType := FSqlStore.Provider.ProviderType;

  FTables := TMetaTables.Create(Self);
  FViews  := TMetaViews.Create(Self);
  FProcedures := TMetaProcedures.Create(Self);
  FSequences := TMetaSequences.Create(Self);
end;

destructor TMetaDatabase.Destroy();
begin
  FSequences.Free();
  FProcedures.Free();
  FViews.Free();
  FTables.Free();
  FConnectionInfo.Free();
  FSqlStore.Free();
  inherited Destroy();
end;

procedure TMetaDatabase.DoClear();
begin
  FSequences.DoClear();
  FProcedures.DoClear();
  FViews.DoClear();
  FTables.DoClear();
end;

procedure TMetaDatabase.Clear();
begin
  FLoading := True;
  try
    DoClear();
    FLoaded := False;
  finally
    FLoading := False;
  end;
end;

procedure TMetaDatabase.LoadField(tblSql: TMemTable; MetaField: TMetaField);
begin
  MetaField.FDataType        := tblSql.FieldByName('DataType').AsString.Trim();
  MetaField.FDataSubType     := tblSql.FieldByName('DataSubType').AsString.Trim();
  MetaField.FIsNullable      := tblSql.FieldByName('IsNullable').AsInteger = 1;
  MetaField.FSizeInChars     := tblSql.FieldByName('SizeInChars').AsInteger;
  MetaField.FSizeInBytes     := tblSql.FieldByName('SizeInBytes').AsInteger;
  MetaField.FPrecision       := tblSql.FieldByName('DecimalPrecision').AsInteger;
  MetaField.FScale           := tblSql.FieldByName('DecimalScale').AsInteger;
  MetaField.FDefaultValue    := tblSql.FieldByName('DefaultValue').AsString.Trim();
  MetaField.FExpression      := tblSql.FieldByName('Expression').AsString.Trim();
  MetaField.FOrdinalPosition := tblSql.FieldByName('OrdinalPosition').AsInteger;
end;
procedure TMetaDatabase.LoadTables();
var
  SqlText : string;
  tblSql: TMemTable;
  TableName: string;
  FieldName: string;
  MetaTable: TMetaTable;
  MetaField: TMetaField;
begin
  SqlText := SqlStore.Provider.TablesAndFieldsSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin

      // table, if not already in the list
      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName);
      if not Assigned(MetaTable) then
        MetaTable := Tables.Add(TableName);

      // field
      FieldName := tblSql.FieldByName('FieldName').AsString;
      FieldName := UnQuote(FieldName);
      MetaField := MetaTable.Fields.Add(FieldName);
      LoadField(tblSql, MetaField);

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;
// -------------------------------------------------------
procedure TMetaDatabase.LoadViews();
var
  SqlText : string;
  tblSql: TMemTable;
  ViewName: string;
  FieldName: string;
  MetaView: TMetaView;
  MetaField: TMetaField;
begin
  SqlText := SqlStore.Provider.ViewsAndFieldsSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      // table, if not already in the list
      ViewName := tblSql.FieldByName('TableName').AsString;
      ViewName := UnQuote(ViewName);

      MetaView := Views.Find(ViewName);
      if not Assigned(MetaView) then
      begin
        MetaView := Views.Add(ViewName);
        MetaView.FDefinition := tblSql.FieldByName('Definition').AsString;
      end;

      // field
      FieldName := tblSql.FieldByName('FieldName').AsString;
      FieldName := UnQuote(FieldName);
      MetaField := MetaView.Fields.Add(FieldName);
      LoadField(tblSql, MetaField);

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;


procedure TMetaDatabase.LoadIndexes();
  // -------------------------------------------------------
  procedure LoadIndex(tblSql: TMemTable; MetaIndex: TMetaIndex);
  begin
    //MetaIndex.FFields         := tblSql.FieldByName('FieldName').AsString.Trim();
    MetaIndex.FFieldCount     := tblSql.FieldByName('FieldCount').AsInteger;
    MetaIndex.FForeignKey     := tblSql.FieldByName('ForeignKey').AsString.Trim();
    MetaIndex.FIndexType      := tblSql.FieldByName('IndexType').AsString.Trim();
    MetaIndex.FIsActive       := not tblSql.FieldByName('IsInactive').AsBoolean;
    MetaIndex.FIsDescending   := tblSql.FieldByName('IsDescending').AsBoolean;
    MetaIndex.FIsUnique       := tblSql.FieldByName('IsUnique').AsBoolean;
  end;
  // -------------------------------------------------------
var
  SqlText : string;
  tblSql: TMemTable;
  TableName: string;
  IndexName: string;
  FieldName: string;
  MetaTable: TMetaTable;
  MetaIndex: TMetaIndex;
begin
  SqlText := SqlStore.Provider.IndexesSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName);
      if Assigned(MetaTable) then
      begin
        IndexName := tblSql.FieldByName('IndexName').AsString;
        IndexName := UnQuote(IndexName);

        FieldName := tblSql.FieldByName('FieldName').AsString;
        FieldName := UnQuote(FieldName);

        MetaIndex := MetaTable.Indexes.Find(IndexName);
        if not Assigned(MetaIndex) then
        begin
          MetaIndex := MetaTable.Indexes.Add(IndexName);
          MetaIndex.FFields := FieldName;
          LoadIndex(tblSql, MetaIndex);
        end else begin
          MetaIndex.FFields := MetaIndex.FFields + ';' + FieldName;
        end;
      end;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;

procedure TMetaDatabase.LoadTriggers();
  // -------------------------------------------------------
  procedure LoadTrigger(tblSql: TMemTable; MetaTrigger: TMetaTrigger);
  begin
    MetaTrigger.FTableName     := UnQuote(tblSql.FieldByName('TableName').AsString.Trim());
    MetaTrigger.FTriggerType   := tblSql.FieldByName('TriggerType').AsString.Trim();
    MetaTrigger.FIsActive      := not tblSql.FieldByName('IsInactive').AsBoolean;
    MetaTrigger.FDefinition    := tblSql.FieldByName('Definition').AsString.Trim();
    MetaTrigger.FIsValid       := tblSql.FieldByName('IsValid').AsBoolean;
  end;
  // -------------------------------------------------------
var
  SqlText : string;
  tblSql: TMemTable;
  TableName: string;
  TriggerName: string;
  MetaTable: TMetaTable;
  MetaTrigger: TMetaTrigger;
begin
  SqlText := SqlStore.Provider.TriggersSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      TableName := tblSql.FieldByName('TableName').AsString;
      TableName := UnQuote(TableName);

      MetaTable := Tables.Find(TableName);
      if Assigned(MetaTable) then
      begin
        TriggerName := tblSql.FieldByName('TriggerName').AsString;
        TriggerName := UnQuote(TriggerName);

        MetaTrigger := MetaTable.Triggers.Add(TriggerName);
        LoadTrigger(tblSql, MetaTrigger);
      end;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;
end;
// -------------------------------------------------------
procedure TMetaDatabase.LoadProcedures();
var
  SqlText : string;
  tblSql: TMemTable;
  ProcedureName: string;
  MetaProcedure: TMetaProcedure;
begin
  SqlText := SqlStore.Provider.ProceduresSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      ProcedureName := tblSql.FieldByName('ProcedureName').AsString.Trim();
      ProcedureName := UnQuote(ProcedureName);

      MetaProcedure := Procedures.Add(ProcedureName);
      MetaProcedure.FProcedureType := tblSql.FieldByName('ProcedureType').AsString.Trim();
      MetaProcedure.FDefinition    := tblSql.FieldByName('Definition').AsString.Trim();
      MetaProcedure.FIsValid       := tblSql.FieldByName('IsValid').AsBoolean;
      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;
// -------------------------------------------------------
procedure TMetaDatabase.LoadSequences();
var
  SqlText : string;
  tblSql: TMemTable;
  SequenceName: string;
  MetaSequence: TMetaSequence;
begin
  SqlText := SqlStore.Provider.SequencesSql;

  if Sys.IsEmpty(SqlText) then
     Exit;

  tblSql := FDatabase.SqlStore.Select(SqlText);
  try
    tblSql.First();
    while not tblSql.Eof do
    begin
      SequenceName := tblSql.FieldByName('SequenceName').AsString.Trim();
      SequenceName := UnQuote(SequenceName);

      MetaSequence := Sequences.Add(SequenceName);
      MetaSequence.FCurrentValue := tblSql.FieldByName('CurrentValue').AsInteger;
      MetaSequence.FInitialValue := tblSql.FieldByName('InitialValue').AsInteger;
      MetaSequence.FIncrementBy  := tblSql.FieldByName('IncrementBy').AsInteger;

      tblSql.Next();
    end;
  finally
    tblSql.Free();
  end;

end;
// -------------------------------------------------------

procedure TMetaDatabase.Load();
begin
  if not FLoaded then
  begin
    FLoading := True;
    try
      DoClear();

      LoadTables();
      LoadViews();
      LoadIndexes();
      LoadTriggers();
      LoadProcedures();
      LoadSequences();

      {
      case SqlProviderType of
        ptFirebird   : LoadFirebird();
        ptSqlite     : LoadSqlite();
        ptMsSql      : LoadMsSql();
        ptMySql      : LoadMySql();
        ptPostgreSql : LoadPostgreSql();
        ptOracle     : LoadOracle();
      end;
      }

      FLoaded := True;
    finally
      FLoading := False;
    end;
  end;
end;

function TMetaDatabase.FindTable(const TableName: string): TMetaTable;
begin
  Result := Self.Tables.Find(TableName);
end;



function TMetaDatabase.GetNodes: TMetaNodeArray;
begin
  Result := [];
  SetLength(Result, 2);
  Result[0] := Tables;
  Result[1] := Views;
end;




{ TMetaDatabases }

constructor TMetaDatabases.Create();
begin
  inherited Create(nil, 'Databases', ntDatabases);
  FList := TGenObjectList<TMetaDatabase>.Create(True, True);
end;

destructor TMetaDatabases.Destroy();
begin
  FList.Free(); // this frees the objects too.
  inherited Destroy();
end;

procedure TMetaDatabases.Clear();
begin
  FList.Clear();
end;

procedure TMetaDatabases.ReLoad();
var
  A: TSqlConnectionInfoArray;
  i : Integer;
begin
  A := [];
  SetLength(A, FList.Count);
  for i := 0 to FList.Count - 1 do
    A[i] := FList[i].ConnectionInfo.Clone();

  FList.Clear();                                           // this frees the objects too.
  AddRange(A);                                                     // forces databases to call Load();

  for i := Low(A) to High(A) do                                    // dispose the TSqlConnectionInfo instances
     A[i].Free();
end;

function TMetaDatabases.Add(ConnectionInfo: TSqlConnectionInfo): TMetaDatabase;
begin
  Result := TMetaDatabase.Create(Self, ConnectionInfo);
  FList.Add(Result);
  Result.Load();
end;

procedure TMetaDatabases.AddRange(ConnectionInfoList: TSqlConnectionInfoArray);
var
  i : Integer;
begin
  for i := Low(ConnectionInfoList) to High(ConnectionInfoList) do
      Add(ConnectionInfoList[i]);
end;

procedure TMetaDatabases.Remove(ADatabase: TMetaDatabase);
begin
  FList.Remove(ADatabase); // this frees the object too.
end;

function TMetaDatabases.GetNodes: TMetaNodeArray;
var
  i : Integer;
begin
  Result := [];
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
      Result[i] := FList[i];
end;










type

   { SchemaCacheItem }

   TSchemaCacheItem = class
   private
     FConnectionName: string;
     FSchemaName: string;
     FSchemaTable: TDataset;
   public
     constructor Create(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
     destructor Destroy(); override;
   end;

   SchemaCache = class
   private
     class var List: TList;
   public
     class constructor Create();
     class destructor Destroy();

     class function Find(ConnectionName: string; SchemaName: string): TDataset;
     class procedure Add(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
   end;

class constructor SchemaCache.Create();
begin
  List := TList.Create();
end;

class destructor SchemaCache.Destroy();
begin
  Sys.ClearObjectList(List);
  List.Free();
end;

class function SchemaCache.Find(ConnectionName: string; SchemaName: string): TDataset;
var
  i : Integer;
  Item: TSchemaCacheItem;
begin
  for i := 0 to List.Count - 1 do
  begin
    Item := TSchemaCacheItem(List[i]);
    if Sys.IsSameText(Item.FConnectionName, ConnectionName) and Sys.IsSameText(Item.FSchemaName, SchemaName) then
       Exit(Item.FSchemaTable);
  end;

  Result := nil;
end;

class procedure SchemaCache.Add(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
var
  Item: TSchemaCacheItem;
begin
  Item := TSchemaCacheItem.Create(ConnectionName, SchemaName, SchemaTable);
  List.Add(Item);
end;

{ TSchemaCacheItem }

constructor TSchemaCacheItem.Create(ConnectionName: string; SchemaName: string; SchemaTable: TDataset);
begin
  FConnectionName := ConnectionName;
  FSchemaName := SchemaName;
  FSchemaTable := SchemaTable;
end;

destructor TSchemaCacheItem.Destroy();
begin
  FSchemaTable.Free();
  inherited Destroy();
end;




{ TSqlConnectionInfo }

procedure TSqlConnectionInfo.SetConnectionString(AValue: string);
var
  List: TStrings;
  i : Integer;
  Key, Value: string;
begin

  FConnectorType := '';
  FDatabaseName := '';
  FHostName := '';
  FUserName := '';
  FPassword := '';
  FCharSet  := '';

  FParams.Clear();

  FConnectionString := AValue;

  if not Sys.IsEmpty(FConnectionString) then
  begin
    List := Sys.Split(FConnectionString, ';');
    for i := 0 to List.Count - 1 do
    begin
      Key   := List.Names[i];
      Value := Trim(List.Values[Key]);
      Key   := Trim(Key);

      if      Sys.IsSameText(Key, 'Type')
           or Sys.IsSameText(Key, 'ConnectorType') then
        FConnectorType := Value
      else if Sys.IsSameText(Key, 'Database')
           or Sys.IsSameText(Key, 'DatabaseName')
           or Sys.IsSameText(Key, 'Database Name')
           or Sys.IsSameText(Key, 'Initial Catalog')  then
        FDatabaseName := Value
      else if Sys.IsSameText(Key, 'Host')
           or Sys.IsSameText(Key, 'HostName')
           or Sys.IsSameText(Key, 'Server')
           or Sys.IsSameText(Key, 'DataSource')
           or Sys.IsSameText(Key, 'Data Source') then
        FHostName := Value
      else if Sys.IsSameText(Key, 'User')
           or Sys.IsSameText(Key, 'UserName')
           or Sys.IsSameText(Key, 'UserId')
           or Sys.IsSameText(Key, 'User Id')  then
         FUserName := Value
      else if Sys.IsSameText(Key, 'Password')
           or Sys.IsSameText(Key, 'Psw')  then
         FPassword := Value
      else if Sys.IsSameText(Key, 'CharSet')  then
         FCharSet := Value
      else
         FParams.Add(List[i]);
    end;
  end;

end;

function TSqlConnectionInfo.GetProvider: string;
begin
  if not Sys.IsEmpty(FProvider) then
     Result := FProvider
  else
     Result := FConnectorType;
end;

function TSqlConnectionInfo.GetConnectorType: string;
begin
  Result := FConnectorType;

  if not Sys.IsEmpty(FProvider) then
  begin
    if Sys.IsSameText(FProvider, 'MsSql') then
      Result := 'MSSQLServer'
  end else
    Result := Provider;
end;

constructor TSqlConnectionInfo.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TStringList.Create();
end;

destructor TSqlConnectionInfo.Destroy();
begin
  FParams.Free();
  inherited Destroy();
end;

function TSqlConnectionInfo.Clone(): TSqlConnectionInfo;
var
  JsonText: string;
begin
  Result := TSqlConnectionInfo.Create();
  JsonText := Json.Serialize(Self);
  Json.Deserialize(Result, JsonText);
end;

function TSqlConnectionInfo.GetSqlProvider(): TSqlProvider;
begin
  Result := SqlProviders.FindSqlProvider(Provider);
end;

procedure TSqlConnectionInfo.SetupConnection(SqlConnector: TSQLConnector);
begin
  SqlConnector.ConnectorType := ConnectorType;
  SqlConnector.HostName := HostName;
  SqlConnector.DatabaseName := DatabaseName;
  SqlConnector.UserName := UserName;
  SqlConnector.Password := Password;
  SqlConnector.CharSet := CharSet;
  SqlConnector.Params.Assign(Params);
end;

{ TSqlConnectionInfoList }

function TSqlConnectionInfoList.GetItem(Index: Integer): TSqlConnectionInfo;
begin
  Result := fSqlConnections.Items[Index] as TSqlConnectionInfo;
end;

function TSqlConnectionInfoList.GetCount: Integer;
begin
  Result := fSqlConnections.Count;
end;

constructor TSqlConnectionInfoList.Create();
begin
  inherited Create();
  fSqlConnections := TCollection.Create(TSqlConnectionInfo);
end;

destructor TSqlConnectionInfoList.Destroy();
begin
  fSqlConnections.Free();
  inherited Destroy();
end;

procedure TSqlConnectionInfoList.Clear();
begin
  fSqlConnections.Clear();
end;

procedure TSqlConnectionInfoList.Load(FilePath: string);
begin
  Clear();
  Json.LoadFromFile(FilePath, Self);
end;

procedure TSqlConnectionInfoList.Save(FilePath: string);
begin
  Json.SaveToFile(FilePath, Self);
end;

procedure TSqlConnectionInfoList.Remove(Item: TSqlConnectionInfo);
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if Index >= 0 then
     fSqlConnections.Delete(Index);
end;

procedure TSqlConnectionInfoList.Add(Item: TSqlConnectionInfo);
begin
  Item.Collection := fSqlConnections;
end;

function TSqlConnectionInfoList.IndexOf(Item: TSqlConnectionInfo): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to fSqlConnections.Count - 1 do
  begin
    if Item = fSqlConnections.Items[i] then
      exit(i);
  end;
end;

function TSqlConnectionInfoList.Contains(const Name: string): Boolean;
begin
  Result := Find(Name) <> nil;
end;

function TSqlConnectionInfoList.Find(const Name: string): TSqlConnectionInfo;
var
  i : Integer;
  ConInfo : TSqlConnectionInfo;
begin
  Result := nil;

  for i := 0 to fSqlConnections.Count - 1 do
  begin
    ConInfo := fSqlConnections.Items[i] as TSqlConnectionInfo;
    if Sys.IsSameText(Name, ConInfo.Name) then
      exit(ConInfo);
  end;
end;

{ SqlProviders }

class constructor SqlProviders.Create();
begin
  Providers := TList.Create();

  Providers.Add(TSqlProviderFirebird.Create());
  Providers.Add(TSqlProviderMsSql.Create());
end;

class destructor SqlProviders.Destroy();
begin
  Sys.ClearObjectList(Providers);
  Providers.Free();
end;

class function SqlProviders.ProviderTypeToString(ProviderType: TSqlProviderType): string;
var
  i: Integer;
begin
  for i := Low(ProviderTypes) to High(ProviderTypes) do
      if ProviderType = ProviderTypes[i] then
         Exit(ProviderNames[i]);

  Result := '';
end;

class function SqlProviders.StringToProviderType(ProviderType: string): TSqlProviderType;
var
  i: Integer;
begin
  for i := Low(ProviderNames) to High(ProviderNames) do
      if Sys.IsSameText(ProviderType, ProviderNames[i]) then
         Exit(ProviderTypes[i]);

  Result := ptNone;
end;

class function SqlProviders.FindSqlProvider(const ProviderName: string): TSqlProvider;
var
  Item : Pointer;
begin
  Result := nil;
  for Item in Providers do
    if Sys.IsSameText(ProviderName, TSqlProvider(Item).Name) then
      exit(TSqlProvider(Item));
end;

{ TSqlProvider }

constructor TSqlProvider.Create();
begin
  inherited Create();
end;

destructor TSqlProvider.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderFirebird }

constructor TSqlProviderFirebird.Create();
begin
  inherited Create();
  FProviderType := ptFirebird;
  FName         := SqlProviders.SFirebird;

  FTablesAndFieldsSql := SFirebirdTablesAndFieldsSql;
  FViewsAndFieldsSql  := SFirebirdTablesAndFieldsSql;
  FViewsAndFieldsSql  := FViewsAndFieldsSql.Replace('t.RDB$VIEW_BLR is null', 't.RDB$VIEW_BLR is not null', [rfReplaceAll, rfIgnoreCase]);
  FIndexesSql         := SFirebirdIndexesSql;
  FTriggersSql        := SFirebirdTriggersSql;
  FProceduresSql      := SFirebirdProceduresSql;
  FSequencesSql       := SFirebirdSequencesSql;
end;

destructor TSqlProviderFirebird.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderSqlite }

constructor TSqlProviderSqlite.Create();
begin
  inherited Create();
end;

destructor TSqlProviderSqlite.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderMsSql }

constructor TSqlProviderMsSql.Create();
begin
  inherited Create();

  FProviderType := ptMsSql;
  FName         := SqlProviders.SMsSql;

  FTablesAndFieldsSql := SMsSqlTablesAndFieldsSql;
  //FViewsAndFieldsSql  := SMsSqlTablesAndFieldsSql;
  //FIndexesSql         := SMsSqlIndexesSql;
  //FTriggersSql        := SMsSqlTriggersSql;
  //FProceduresSql      := SMsSqlProceduresSql;
  //FSequencesSql       := SMsSqlSequencesSql;
end;

destructor TSqlProviderMsSql.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderMySql }

constructor TSqlProviderMySql.Create();
begin
  inherited Create();
end;

destructor TSqlProviderMySql.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderPostgreSql }

constructor TSqlProviderPostgreSql.Create();
begin
  inherited Create();
end;

destructor TSqlProviderPostgreSql.Destroy();
begin
  inherited Destroy();
end;

{ TSqlProviderOracle }

constructor TSqlProviderOracle.Create();
begin
  inherited Create();
end;

destructor TSqlProviderOracle.Destroy();
begin
  inherited Destroy();
end;

{ TSqlStore }

constructor TSqlStore.Create(ConnectionInfo: TSqlConnectionInfo);
begin
  inherited Create();
  FConnectionInfo := ConnectionInfo.Clone();
  FProvider       := FConnectionInfo.GetSqlProvider();

  FSqlConnector   := TSQLConnector.Create(nil);
  FSqlTransaction := TSQLTransaction.Create(nil);
  FSqlQuery       := TSQLQuery.Create(nil);

  FSqlConnector.Transaction := FSqlTransaction;
  FSqlQuery.DataBase        := FSqlConnector;

  FConnectionInfo.SetupConnection(FSqlConnector);
end;

destructor TSqlStore.Destroy();
begin
  FSqlQuery.Active := False;
  FSqlTransaction.Active := False;
  FSqlConnector.Connected := False;

  FSqlQuery.Free();
  FSqlTransaction.Free();
  FSqlConnector.Free();

  FConnectionInfo.Free();

  inherited Destroy();
end;

procedure TSqlStore.BeginOperation(const SqlText: string; Params: array of const);
begin
  EndOperation();

  FSqlConnector.Connected := True;
  FSqlTransaction.Active := True;

  if FSqlQuery.SQL.Text <> SqlText then
    FSqlQuery.SQL.Text := SqlText;

  if not FSqlQuery.Prepared then;
     FSqlQuery.Prepare();

  if (FSqlQuery.Params.Count > 0) and (Length(Params) > 0) then
     DbSys.AssignParams(FSqlQuery.Params, Params);
end;

procedure TSqlStore.EndOperation();
begin
  FSqlQuery.Active := False;
  if FSqlQuery.Prepared then
     FSqlQuery.UnPrepare();
  FSqlTransaction.Active := False;
  FSqlConnector.Connected := False;
end;

function TSqlStore.Select(const SqlText: string; Params: array of const): TMemTable;
var
  Table: TMemTable;
begin
  Table  := TMemTable.Create(nil);
  Result := Table;

  BeginOperation(SqlText, Params);
  try
    try
      FSqlQuery.Active := True;
      FSqlQuery.First();

      DbSys.CopyDatasetStructure(FSqlQuery, Table, True, True);
      Table.CreateDataset();
      Table.Active := True;
      DbSys.CopyDataset(FSqlQuery, Table);
    except
      on E: Exception do
      // TODO: handle Exception
      ;
    end;
  finally
    EndOperation();
  end;

end;

function TSqlStore.Select(const SqlText: string): TMemTable;
begin
  Result := Select(SqlText, []);
end;

procedure TSqlStore.SelectTo(Table: TDataset; const SqlText: string; Params: array of const);
begin
  BeginOperation(SqlText, Params);
  try
    try
      FSqlQuery.Active := True;
      FSqlQuery.First();

      Table.Active := True;
      DbSys.EmptyDataset(Table);
      DbSys.CopyDataset(FSqlQuery, Table);
    except
      on E: Exception do
      // TODO: handle Exception
      ;
    end;
  finally
    EndOperation();
  end;

end;

procedure TSqlStore.SelectTo(Table: TDataset; const SqlText: string);
begin
   SelectTo(Table, SqlText, []);
end;

function TSqlStore.SelectResults(SqlText: string; const ResultFields: string; const Params: array of const): Variant;
begin
  Result := Variants.Null;

  BeginOperation(SqlText, Params);
  try
    try
      FSqlQuery.Active := True;
      FSqlQuery.First();

      if FSqlQuery.RecordCount > 0 then;
         Result := DbSys.FieldValuesToVariantArray(FSqlQuery, ResultFields);
    except
      on E: Exception do
      // TODO: handle Exception
      ;
    end;
  finally
    EndOperation();
  end;

end;

function TSqlStore.SelectResults(SqlText: string; const ResultFields: string): Variant;
begin
  Result := SelectResults(SqlText, ResultFields, []);
end;

function TSqlStore.SelectResult(SqlText: string; Default: Variant; const Params: array of const): Variant;
begin
  Result := Default;

  BeginOperation(SqlText, Params);
  try
    try
      FSqlQuery.Active := True;
      FSqlQuery.First();

      if FSqlQuery.RecordCount > 0 then;
        Result := FSqlQuery.Fields[0].Value;
    except
      on E: Exception do
      // TODO: handle Exception
      ;
    end;
  finally
    EndOperation();
  end;
end;

function TSqlStore.SelectResult(SqlText: string; Default: Variant): Variant;
begin
   Result := SelectResult(SqlText, Default, []);
end;

function TSqlStore.SelectResult(SqlText: string): Variant;
begin
  Result := SelectResult(SqlText, Variants.Null, []);
end;

function TSqlStore.IntegerResult(SqlText: string; Default: Integer; const Params: array of const): Integer;
begin
  Result := SelectResult(SqlText, Default, Params);
end;

function TSqlStore.IntegerResult(SqlText: string; Default: Integer): Integer;
begin
  Result := SelectResult(SqlText, Default, []);
end;

function TSqlStore.IntegerResult(SqlText: string): Integer;
begin
  Result := SelectResult(SqlText, -1, []);
end;

procedure TSqlStore.ExecSql(const SqlText: string; Params: array of const);
begin
  BeginOperation(SqlText, Params);
  try
    try
      FSqlQuery.ExecSQL();
    except
      on E: Exception do
      // TODO: handle Exception
      ;
    end;
  finally
    EndOperation();
  end;
end;

procedure TSqlStore.ExecSql(const SqlText: string);
begin
  ExecSql(SqlText, []);
end;

procedure TSqlStore.GetNativeSchema(SchemaName, TableName, SqlText: string; Table: TDataset);
var
  cSelects : array[0..3] of string = ( '%s where %s.ID   = -100000000'  ,
                                       '%s where %s.CODE = ''00000000''',
                                       '%s where %s.NAME = ''00000000''',
                                       '%s where 1 > 2'
                                      );
  {-------------------------------------------------}
  function TryGetFieldList(var SchemaTable: TDataset; SqlText: string): Boolean;
  begin
    Result := False;
    try
      SchemaTable := Select(SqlText);
      Result := True;
    except
    end;
  end;
  {-------------------------------------------------}

var
  SchemaTable: TDataset;
  i : Integer;
  SqlText2 : string;
begin
  if Sys.IsEmpty(TableName) then
    Sys.Error('Cannot get native schema: TableName is empty.');

  if Sys.IsEmpty(SqlText)  then
    SqlText := Format('select * from %s', [TableName]);

  if Sys.IsEmpty(SchemaName)  then
    SchemaName := TableName;

  if Table = nil then
    Sys.Error('Cannot get native schema: Table is nil.');

  SchemaTable := SchemaCache.Find(Self.ConnectionName, SchemaName);

  if SchemaTable = nil then
  begin
    for i := Low(cSelects) to High(cSelects) do
    begin
       SqlText2 := Format(cSelects[i], [SqlText, TableName]);

      if TryGetFieldList(SchemaTable, SqlText2) then
      begin
        SchemaCache.Add(Self.ConnectionName, SchemaName, SchemaTable);
        break;
      end;
    end;
  end;

  if SchemaTable = nil then
    Sys.Error('Cannot get native schema for: %s', [SchemaName]);

  DbSys.MergeStructure(SchemaTable, Table);
end;

function TSqlStore.GetConnectionName: string;
begin
  Result := FConnectionInfo.Name;
end;






{ DbSys }

class constructor DbSys.Create();
begin
end;

class destructor DbSys.Destroy();
begin
  if Assigned(FMetaDatabases) then
    FMetaDatabases.Free();
end;

class function DbSys.GetStringFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftString
  ,ftGuid
  ,ftFixedChar
  ];
end;

class function DbSys.GetWideStringFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftWideString
  ,ftFixedWideChar
  ];
end;

class function DbSys.GetIntegerFieldTypes: TSetOfFieldType; static;
begin
   Result :=
  [ftAutoInc
  ,ftSmallint
  ,ftInteger
  ,ftWord
  ,ftLargeint
  ];
end;

class function DbSys.GetMetaDatabases: TMetaDatabases; static;
begin
  if FMetaDatabases = nil then
    FMetaDatabases := TMetaDatabases.Create();
  Result := FMetaDatabases;
end;

class function DbSys.GetFloatFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftFloat
  ,ftCurrency
  ];
end;

class function DbSys.GetBCDFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftBCD
  ,ftFMTBcd
  ];
end;

class function DbSys.GetDateTimeFieldTypes: TSetOfFieldType; static;
begin
  Result :=
  [ftDate
  ,ftTime
  ,ftDateTime
  ,ftTimeStamp
  ];
end;

class function DbSys.GetBlobFieldTypess: TSetOfFieldType; static;
begin
  Result :=
  [ftMemo
  ,ftWideMemo
  ,ftFmtMemo

  ,ftBlob
  ,ftGraphic

  ,ftOraBlob
  ,ftOraClob
  ];
end;






(*----------------------------------------------------------------------------*)
class function DbSys.AsInteger(Table: TDataset; const FieldName: string; Default: Integer = -1): Integer;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsInteger
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsFloat(Table: TDataset; const FieldName: string; Default: Double = 0): Double;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsFloat
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsFloatString(Table: TDataset; const FieldName: string; Digits: Integer = 3; Default: string = ''): string;
var
  Field : TField;
begin

  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Sys.CommaToDot(Sys.DoubleToStr(Field.AsFloat , Digits))
  else
    Result := Default;

end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsString(Table: TDataset; const FieldName: string; Default: string = ''): string;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsString
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsBoolean(Table: TDataset; const FieldName: string; Default: Boolean = False): Boolean;
var
  Field : TField;
begin
  Result := Default;
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
  begin
    if (Field.DataType in IntegerFieldTypes) or (Field.DataType in FloatFieldTypes) then
      Result := Boolean(Integer(Field.AsInteger))
    else if (Field.DataType = ftBoolean) then
      Result := Field.AsBoolean
    else if (Field.DataType in StringFieldTypes) then
    begin
      if not TryStrToBool(Field.AsString, Result) then
        Result := Default;
    end
  end;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AsDateTime(Table: TDataset; const FieldName: string; Default: TDateTime = 0): TDateTime;
var
  Field : TField;
begin
  Field := Table.FindField(FieldName);
  if Assigned(Field) and (not Field.IsNull) then
    Result := Field.AsDateTime
  else
    Result := Default;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.SafeSetFieldValue(Table: TDataset; FieldName: string; Value: Variant);
begin
  if Table.FindField(FieldName) <> nil then
  begin
    if not (Table.State in [dsEdit, dsInsert]) then
      Table.Edit;
    Table.FieldByName(FieldName).Value := Value;
  end;
end;
(*----------------------------------------------------------------------------*)
class function  DbSys.CreateField(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer = 0; DisplayLabel: string = ''): TField;
var
  Owner: TComponent;
begin
  if Assigned(Dataset.Owner) then
    Owner := Dataset.Owner
  else
    Owner := Dataset;

  Result := DefaultFieldClasses[DataType].Create(Owner);

  Result.FieldName := FieldName;
  if Size > 0 then
    Result.Size      := Size;

  Result.DataSet := Dataset;

  if DisplayLabel <> '' then
    Result.DisplayLabel := DisplayLabel;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.CreateFieldInternalCalc(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer; DisplayLabel: string): TField;
begin
  Result := CreateField(Dataset, FieldName, DataType, Size, DisplayLabel);
  Result.FieldKind := fkInternalCalc;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.CreateFieldLookUp(Dataset: TDataset; const FieldName, KeyFields: string; LookupDataSet: TDataset; LookupKeyFields, LookupResultField: string): TField;
var
  Field      : TField;
  FieldClass : TFieldClass;
begin
  Field := LookupDataSet.FindField(LookupResultField);
  if not Assigned(Field) then
    raise Exception.CreateFmt('CreateFieldLookUp'#13'Field %s not found in LookUp Table', [LookupResultField]);

  FieldClass := TFieldClass(Field.ClassType);

  Field := FieldClass.Create(Dataset);
  Field.FieldName          := FieldName;
  Field.FieldKind          := fkLookup;
  Field.KeyFields          := KeyFields;
  Field.DataSet            := Dataset;

  Field.LookupKeyFields    := LookupKeyFields;
  Field.LookupResultField  := LookupResultField;
  Field.LookupDataSet      := LookupDataSet;

  //if Assigned(FieldList) then
    //FieldList.Add(Field);

  Result := Field;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.AddFieldDef(Dataset: TDataset; const FieldName: string; DataType: TFieldType; Size: Integer; Required: Boolean): TFieldDef;
begin
  Result          := Dataset.FieldDefs.AddFieldDef();
  Result.Name     := FieldName;
  Result.DataType := DataType;
  Result.Size     := Size;
  Result.Required := Required;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyFieldStructure(SourceField: TField; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  TargetField : TField;
  FieldClass  : TFieldClass;
begin
  FieldClass  := TFieldClass(SourceField.ClassType);
  if (FieldClass.InheritsFrom(TAutoIncField)) and AutoIncToInt then
    FieldClass := TIntegerField;

  TargetField := FieldClass.Create(Target);

  TargetField.Size              := SourceField.Size;
  TargetField.FieldKind         := SourceField.FieldKind;
  TargetField.FieldName         := SourceField.FieldName;
  TargetField.DisplayLabel      := SourceField.DisplayLabel;
  TargetField.Visible           := SourceField.Visible;

  if SetRequired then
    TargetField.Required := SourceField.Required;

  //TargetField.Lookup            := SourceField.Lookup;
  TargetField.KeyFields         := SourceField.KeyFields;
  TargetField.LookupDataSet     := SourceField.LookupDataSet;
  TargetField.LookupResultField := SourceField.LookupResultField;
  TargetField.LookupKeyFields   := SourceField.LookupKeyFields;

  if SourceField is TBCDField then
    TBCDField(TargetField).Precision := TBCDField(SourceField).Precision;

  TargetField.DataSet      := Target;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyDatasetStructure(Source: TDataset; Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  i : Integer;
begin
  Target.Active := False;
  Target.Fields.Clear;

  for i := 0 to Source.FieldCount - 1 do
    CopyFieldStructure(Source.Fields[i], Target, SetRequired, AutoIncToInt);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.MergeStructure(Source, Target: TDataset; SetRequired: Boolean = False; AutoIncToInt: Boolean = True);
var
  i : Integer;
begin
  for i := 0 to Source.FieldCount - 1 do
    if Target.FindField(Source.Fields[i].FieldName) = nil then
      CopyFieldStructure(Source.Fields[i], Target, SetRequired, AutoIncToInt);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyField(SourceField, DestField: TField);
begin
  if Assigned(SourceField) and Assigned(DestField) then
  begin
    if SourceField.IsNull then
      DestField.Clear
    else if (SourceField is TLargeintField) or (DestField is TLargeintField) then
      DestField.AsInteger := SourceField.AsInteger
    else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
      CopyFieldBlob(TBlobField(SourceField), TBlobField(DestField))
    else
      DestField.Value := SourceField.Value;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyFieldBlob(SourceField, DestField: TBlobField);
var
  MS : TMemoryStream;
begin
  if Assigned(SourceField) and Assigned(DestField) then
  begin
    MS := TMemoryStream.Create;
    try
      SourceField.SaveToStream(MS);
      MS.Position := 0;
      DestField.LoadFromStream(MS);
    finally
      MS.Free;
    end;
  end;

end;
(*----------------------------------------------------------------------------
  Source and Dest datasets may not be identical in schema
 ----------------------------------------------------------------------------*)
class procedure DbSys.CopyRecord(Source, Dest: TDataset);
var
  i            : Integer;
  SourceField  : TField;
  DestField    : TField;
  MS           : TMemoryStream;
begin
  MS         := TMemoryStream.Create;
  try
    Dest.Edit;

    for i := 0 to Source.FieldCount - 1 do
    begin
      SourceField  := Source.Fields[i];
      DestField    := Dest.FindField(Source.Fields[i].FieldName);
      if (not SourceField.IsNull) and Assigned(DestField) then
      begin
        if (SourceField is TLargeintField) or (DestField is TLargeintField) then
            DestField.AsInteger := SourceField.AsInteger
        else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
        begin
          MS.Clear;
          TBlobField(SourceField).SaveToStream(MS);
          MS.Position := 0;
          TBlobField(DestField).LoadFromStream(MS);
        end else
          DestField.Value := SourceField.Value;
      end;
    end;

    Dest.Post;
  finally
    MS.Free;
  end;

end;
(*----------------------------------------------------------------------------
  Source and Dest datasets may not be identical in schema
 ----------------------------------------------------------------------------*)
class procedure DbSys.CopyRecordAppend(Source, Dest: TDataset);
begin
  Dest.Append;
  CopyRecord(Source, Dest);
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.CopyDataset(Source, Dest: TDataset);
var
  BM         : TBookMark;
  SourceList : TList;
  DestList   : TList;
  i          : Integer;
  Field      : TField;
  SourceField: TField;
  DestField  : TField;
  MS         : TMemoryStream;
begin
  BM := Source.Bookmark;
  Source.DisableControls;
  Dest.DisableControls;
  SourceList := TList.Create;
  DestList   := TList.Create;
  MS         := TMemoryStream.Create;
  try

    for i := 0 to Source.FieldCount - 1 do
    begin
      Field := Dest.FindField(Source.Fields[i].FieldName);
      if Assigned(Field) then
      begin
        SourceList.Add(Source.Fields[i]);
        DestList.Add(Field);
      end;
    end;

    Source.First;
    while not Source.Eof do
    begin

      Dest.Append;

      for i := 0 to SourceList.Count - 1 do
      begin
        SourceField := TField(SourceList[i]);
        DestField   := TField(DestList[i]);
        if (not SourceField.IsNull) then
        begin
          if (SourceField is TLargeintField) or (DestField is TLargeintField) then
            DestField.AsInteger := SourceField.AsInteger
          else if ((SourceField is TBlobField) and (DestField is TBlobField)) then
          begin
            MS.Clear;
            TBlobField(SourceField).SaveToStream(MS);
            MS.Position := 0;
            TBlobField(DestField).LoadFromStream(MS);
          end else
            //SourceField.AssignValue(DestField.Value);
            DestField.Value := SourceField.Value;
        end;
      end;

      Dest.Post;

      Source.Next;
    end;

    if not Dest.IsEmpty then
      Dest.First;

  finally
    MS.Free;
    DestList.Free;
    SourceList.Free;
    Dest.EnableControls;
    Source.Bookmark := BM;
    Source.EnableControls;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.EmptyDataset(Dataset: TDataset);
begin
  if Assigned(Dataset) and (Dataset.Active) and (not Dataset.IsEmpty) then
  begin
    Dataset.DisableControls;
    try
      Dataset.First;
      while not Dataset.Eof do
        Dataset.Delete;
    finally
      Dataset.EnableControls;
    end;
  end;

end;
(*----------------------------------------------------------------------------*)
class function  DbSys.IdenticalSchema(Table1, Table2: TDataset): Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 0 to Table1.FieldCount - 1 do
    if not (Sys.IsSameText(Table1.Fields[i].FieldName, Table2.Fields[i].FieldName) and (Table1.Fields[i].FieldKind = Table2.Fields[i].FieldKind)) then
    begin
      Result := False;
      Exit; //==>
    end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.StrToBlob(Field: TBlobField; const Data: string);
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer(PChar(Data)^, Length(Data));
    MS.Position := 0;
    Field.Assign(nil);
    Field.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.BlobToStr(Field: TBlobField): string;
var
  MS: TMemoryStream;
begin
  Result := '';
  MS := TMemoryStream.Create;
  try
    Field.SaveToStream(MS);
    MS.Position := 0;
    SetLength(Result, MS.Size);
    MS.ReadBuffer(PChar(Result)^, MS.Size);
  finally
    MS.Free;
  end;
end;
(*----------------------------------------------------------------------------*)
class procedure DbSys.StreamToBlob(Field: TBlobField; Stream: TStream);
begin
  Field.Assign(nil);
  Field.LoadFromStream(Stream);
end;
(*----------------------------------------------------------------------------*)
class function  DbSys.BlobToStream(Field: TBlobField): TStream;
begin
  Result := TMemoryStream.Create;
  Field.SaveToStream(Result);
  Result.Position := 0;
end;
(*----------------------------------------------------------------------------*)
class function DbSys.FieldValuesToVariantArray(Dataset: TDataset;  const ResultFields: string): Variant;
var
  i    : Integer;
  List : TStringList;
begin
  Result := Variants.Null;

  List   := TStringList.Create;
  try
    if Sys.IsEmpty(ResultFields) then
    begin
      for i := 0 to Dataset.Fields.Count - 1 do
        List.Add(Dataset.Fields[i].FieldName);
    end else begin
      Sys.Split(ResultFields, ';', List);
    end;

    { we're going to return the value of a SINGLE field }
    if List.Count = 1 then
    begin
      if (Dataset.FindField(List[0]) <> nil) then
        Result := Dataset.Fields[0].Value
      else
        Result := Variants.Null;

    { we're going to return values more than one field }
    end else if List.Count > 1 then
    begin
      Result := VarArrayCreate([0, List.Count - 1], varVariant);
      for i := 0 to List.Count - 1 do
        if (Dataset.FindField(List[i]) <> nil) then
          Result[i] := Dataset.Fields[i].Value
        else
          Result[i] := Variants.Null;
    end;

  finally
    List.Free;
  end;
end;
(*----------------------------------------------------------------------------
  for creating statements like

    select
      *
    from
      TABLE_NAME
    where
      FIELD_NAME in (...)

  in order to avoid limit problems many servers, like Oracle,
  impose, for the in set of values.
 ----------------------------------------------------------------------------*)
class procedure DbSys.GetKeyValuesList(List: TStrings; Table: TDataset; const FieldName: string; ModValue: Integer; DiscardBelowZeroes: Boolean);
var
  Counter   : Integer;
  S         : string;
  Field     : TField;
  i         : Integer;
  IsString  : Boolean;
begin
  List.Clear;

  Field := Table.FindField(FieldName);
  if not Assigned(Field) then
    Exit; //==>

  IsString := (Field is TStringField);

  Counter := 0;
  S       := '';


  Table.DisableControls;
  try
    Table.First;
    while not Table.Eof do
    begin
      if (not Field.IsNull) then
      begin
        case IsString of
          False : if (DiscardBelowZeroes and (Field.AsInteger > 0)) or (not DiscardBelowZeroes) then
                  begin
                    S  := S + Field.AsString + ', ';
                    Inc(Counter);
                  end;
          True  : begin
                    S  := S + Sys.QS(Field.AsString) + ', ';
                    Inc(Counter);
                  end;
        end;

        if (Counter mod ModValue = 0) then
        begin
          List.Add(S);
          S := '';
        end;

      end;
      Table.Next;
    end;
  finally
    Table.EnableControls;
  end;


  if S <> '' then
    List.Add(S);

  for i := 0 to List.Count - 1 do
  begin
    S := List[i];
    if Length(S) > 2 then
      SetLength(S, Length(S) - 2);
    List[i] := Format('(%s)', [S]);
  end;

end;

class procedure DbSys.AssignParams(Params: TParams; A: array of const);
var
  i : Integer;
  //Stream : TStream;
begin
  if (Params.Count > 0) and (Length(A) > 0) then
  begin

    // Just a single element.
    // Three cases:
    // 1. AssignParams(Params, [MyStream])
    // 2. AssignParams(Params, [MyDataset])
    // 3. AssignParams(Params, [VarArrayOf([Param1, ..., ParamN])])
    if Length(A) = 1 then
    begin
      case A[0].VType of
        vtObject   : if A[0].VObject is TStream then
                       AssignStreamToParam(Params[0],  A[0].VObject as TStream)
                     else if A[0].VObject is TDataset then
                       AssignParams(Params, A[0].VObject as TDataset);
        vtVariant  : if VarIsArray(A[0].VVariant^) then
                       AssignParams(Params, A[0].VVariant^);
      end;
    end else begin
      // array with more than a single element
      for i := 0 to High(A) do
      begin
        case A[i].VType of
          vtInteger       : Params.Items[i].Value := A[i].VInteger;
          vtBoolean       : Params.Items[i].Value := A[i].VBoolean;
          vtChar          : Params.Items[i].Value := A[i].VChar;
          vtWideChar      : ;
          vtExtended      : Params.Items[i].Value := A[i].VExtended^;
          vtString        : Params.Items[i].Value := A[i].VString^;    // short string
          vtPointer       : ;                                          // Longint(Args[i].VPointer)
          vtPChar         : ;
          vtObject        : if A[i].VObject is TStream then
                              AssignStreamToParam(Params[i],  A[i].VObject as TStream);
          vtClass         : ;                                          // Args[i].VClass
          vtPWideChar     : ;
          vtAnsiString    : Params.Items[i].Value := AnsiString(A[i].VAnsiString);
          vtCurrency      : Params.Items[i].Value := A[i].VCurrency^;
          vtVariant       : Params.Items[i].Value := A[i].VVariant^;
          vtInterface     : ;
          vtWideString    : Params.Items[i].Value := WideString(A[i].VWideString);
          vtInt64         : Params.Items[i].Value := A[i].VInt64^;
          vtUnicodeString : Params.Items[i].Value := UnicodeString(A[i].VUnicodeString);
          vtQWord         : Params.Items[i].Value := A[i].VQWord^;
        end;
      end;
    end;
  end;

end;

class procedure DbSys.AssignParams(Params: TParams; tblParams: TDataset);
var
  i        : Integer;
  Field    : TField;
  Param    : TParam;
  MS       : TMemoryStream;
begin
  if not Assigned(tblParams) then
    Exit; //==>

  for i := 0 to Params.Count - 1 do
  begin
    Param := Params[i];
    Field := tblParams.FindField(Param.Name);

    if Assigned(Field) then
    begin
      if Field.IsNull then
        Param.Value := Variants.Null
      else if (Field.DataType in BlobFieldTypes) then
      begin
        MS := TMemoryStream.Create;
        try
          TBlobField(Field).SaveToStream(MS);
          MS.Position := 0;
          if Param.DataType = ftGraphic then
            Param.LoadFromStream(MS, ftGraphic)
          else Param.LoadFromStream(MS, ftBlob);
        finally
          MS.Free;
        end;
      end else begin
        //Param.AssignField(Field);
        Param.Value := Field.Value;
      end;
    end;
  end;

end;


{ to be used as AssignParams(Params, VarArrayOf([Param1, ..., ParamN])); }
class procedure DbSys.AssignParams(Params: TParams; A: array of Variant);
var
  i : Integer;
  Stream: TStream;
begin
  if (Length(A) > 0) and (Params.Count > 0) then
  begin
    for i := Low(A) to High(A) do
    begin
      if Sys.IsBox(A[i]) and (Sys.UnBox(A[i]) is TStream) then
      begin
        Stream :=  TStream(Sys.UnBox(A[i]));
        Params.Items[i].DataType := ftBlob;
        Params.Items[i].Size     := Stream.Size;
        Stream.Position := 0;
        Params.Items[i].LoadFromStream(Stream, ftBlob);
      end else begin
        Params.Items[i].Value := A[i];
      end;
    end;
  end;

end;

class procedure DbSys.AssignStreamToParam(Param: TParam; Stream: TStream);
begin
  Param.DataType  := ftBlob;
  Param.Size      := Stream.Size;
  Stream.Position := 0;
  Param.LoadFromStream(Stream, ftBlob);
end;















initialization
  //

end.

